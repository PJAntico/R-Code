library(parallel)
library(data.table)
library(microbenchmark)
library(reshape)
library(Rcpp)
library(readr)
library(DescTools)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RQuantLib)


# Ncoreslogical <- detectCores(logical = TRUE)
# Ncoreslogical #Hyperthreaded

# -----------------------------------------------------------------------------------------------------------------------
# only need to run the below section 1x per day - after receiving the daily equity options close file from CBOE DataShop
# the section creates the larger table of all equity options by exp.

# detectCores()
# foreach::getDoParWorkers()

eqOptionsFile <- read_csv("C:/Users/pjant/Downloads/UnderlyingOptionsEODCalcs_2020-05-07.zip")

# create a proforma table of equity options with only the fields that will be needed, than name the columns
proFormaTable <- eqOptionsFile %>%
  filter(implied_volatility_1545 != .02, expiration >= today()) %>%
  select(underlying_symbol, strike, option_type, expiration, close, active_underlying_price_1545,
                        delta_1545, implied_volatility_1545, open_interest)
colnames(proFormaTable)[1] <- c("symbol")
colnames(proFormaTable)[5:8] <- c("optclose", "stockprice", "delta", "ivol")

# remove any expiry dates that are before today - these come into the file for some reason

# proFormaTable <- proFormaTable[proFormaTable$expiration >= today(),]
# proFormaTable <- proFormaTable[proFormaTable$expiration >= as.Date("2019-06-12"),]
proFormaTable <- as.data.table(proFormaTable)

# calculate theoretical black-scholes prices for every option
i = 0
x = NULL
for(i in 1:nrow(proFormaTable)){
  if(proFormaTable$stockprice[[i]] == 0) { 
    x[[i]] <- as.double(0)
  } else {
    if(proFormaTable$option_type[[i]] == "C"){
      x[[i]] <- EuropeanOption("call", proFormaTable$stockprice[[i]], proFormaTable$strike[[i]], .01, .0235,
                    (proFormaTable$expiration[[i]] - today())/360, proFormaTable$ivol[[i]])$value
      } else {
      x[[i]] <- EuropeanOption("put", proFormaTable$stockprice[[i]], proFormaTable$strike[[i]], .01, .0235,
                                 (proFormaTable$expiration[[i]] - today())/360, proFormaTable$ivol[[i]])$value
    }
  }
}
  
x <- as.data.frame(x)
proFormaTable$tprice <- x


# split the proforma table into two, one for Calls and one for Puts
# proFormaTableCalls <- proFormaTable[proFormaTable$option_type == "C",]
# proFormaTablePuts <- proFormaTable[proFormaTable$option_type == "P",]


# Narrow down the above proforma Call and PUt tables to contain ONLY ATM strikes
# proFormaTableCalls$diff <- abs(proFormaTableCalls$strike - proFormaTableCalls$stockprice)
# proFormaTablePuts$diff <- abs(proFormaTablePuts$strike - proFormaTablePuts$stockprice)

# atmTableCalls <- ddply(proFormaTableCalls, c("symbol","expiration"), function(z) {z[z$diff == min(z$diff),]})
# atmTablePuts <- ddply(proFormaTablePuts, c("symbol","expiration"), function(z) {z[z$diff == min(z$diff),]})

# atmTableCalls <- atmTableCalls[!duplicated(atmTableCalls[1:4]),]    # this code creates ETN (w/84 strike), AND ETN (w/85 strike)
# atmTableCallsVol <- spread(atmTableCalls[, -c(5,7,9)], expiration, ivol)
# atmTableCallsPrice <- spread(atmTableCalls[, -c(7:9)], expiration, optclose)

# atmTablePuts <- atmTablePuts[!duplicated(atmTablePuts[1:4]),]
# atmTablePutsVol <- spread(atmTablePuts[, -c(5,7,9)], expiration, ivol)
# atmTablePutsPrice <- spread(atmTablePuts[, -c(7:9)], expiration, optclose)

#grp.atmTableCalls <- atmTableCalls %>%
#  group_by(symbol) %>%
#  mutate(bberg_id = paste0(symbol, " US ", format(expiration, "%m/%d/%Y"), " ", option_type, strike)) %>%
#  mutate(id = row_number())

# --------------------------------------------------
### new trial code for all strikes - 8/30/2019
proFormaTable <- proFormaTable %>%
#  group_by(symbol, strike) %>%
  mutate(bberg_id = paste0(symbol, " US ", format(expiration, "%m/%d/%Y"), " ", option_type, strike)) %>%
  mutate(id = row_number()) %>%
  mutate(moneyness = stockprice / strike)

split.df <- split(proFormaTable, with(proFormaTable, interaction(symbol, strike, option_type)), drop = TRUE)
split.df <- lapply(split.df, as.data.table)

# -------------------------------------------------


# split.df <- split(grp.atmTableCalls, grp.atmTableCalls$symbol)



# for (i in 1:length(split.df)) {assign(unique(split.df[[i]]$name), split.df[[i]])}

ratio_list = NULL
combo_list = NULL
date_list = NULL
vol_list = NULL
oi_list = NULL
# strike_list = NULL
delta_list = NULL
optclose_list = NULL
opt_type_list = NULL
moneyness_list = NULL
tprice_list = NULL

stock = NULL
combos = NULL
price = NULL
strike = NULL
j = 0
k = 0


for(j in 1:length(split.df)){
  if(nrow(split.df[[j]]) > 2) {
    ratio_list[[j]] = combn(split.df[[j]]$ivol, 2, function(x) x[1]/x[2])
    combo_list[[j]] = combn(split.df[[j]]$id, 2, function(y) paste(y[1]," ",y[2]))
    date_list[[j]] = combn(split.df[[j]]$expiration, 2, function(z) paste(z[1]," ", z[2]))
    vol_list[[j]] = combn(split.df[[j]]$ivol, 2, function(w) paste(w[1], " ", w[2]))
    oi_list[[j]] = combn(split.df[[j]]$open_interest, 2, function(v) paste(v[1]," ", v[2]))
    delta_list[[j]] = combn(split.df[[j]]$delta, 2, function(t) paste(t[1]," ", t[2]))
    optclose_list[[j]] = combn(split.df[[j]]$optclose, 2, function(s) paste(s[1]," ", s[2]))
    opt_type_list[[j]] = combn(split.df[[j]]$option_type, 2, function(t) paste(t[1]," ", t[2]))
    moneyness_list[[j]] = combn(split.df[[j]]$moneyness, 2, function(u) paste(u[1]," ", u[2]))
    tprice_list[[j]] = combn(split.df[[j]]$tprice, 2, function(v) paste(v[1]," ", v[2]))
    
    ratio_list[[j]] = as.data.frame(ratio_list[[j]])
    combo_list[[j]] = as.data.frame(combo_list[[j]])
    date_list[[j]] = as.data.frame(date_list[[j]])
    vol_list[[j]] = as.data.frame(vol_list[[j]])
    oi_list[[j]] = as.data.frame(oi_list[[j]])
    delta_list[[j]] = as.data.frame(delta_list[[j]])
    optclose_list[[j]] = as.data.frame(optclose_list[[j]])
    opt_type_list[[j]] = as.data.frame(opt_type_list[[j]])
    moneyness_list[[j]] = as.data.frame(moneyness_list[[j]])
    tprice_list[[j]] = as.data.frame(tprice_list[[j]])
    
    stock[[j]] = as.character(split.df[[j]]$symbol[1])
    price[[j]] = as.character(split.df[[j]]$stockprice[1])
    strike[[j]] = as.character(split.df[[j]]$strike[1])
    
    ratio_list[[j]]$vols <- vol_list[[j]]
    ratio_list[[j]]$stock <- stock[[j]]
    ratio_list[[j]]$combo <- combo_list[[j]]
    ratio_list[[j]]$dates <- date_list[[j]]
    ratio_list[[j]]$oi <- oi_list[[j]]
    ratio_list[[j]]$strike <- strike[[j]]
    ratio_list[[j]]$delta <- delta_list[[j]]
    ratio_list[[j]]$price <- price[[j]]
    ratio_list[[j]]$optclose <- optclose_list[[j]]
    ratio_list[[j]]$opttype <- opt_type_list[[j]]
    ratio_list[[j]]$moneyness <- moneyness_list[[j]]
    ratio_list[[j]]$tprice <- tprice_list[[j]]
  
    colnames(ratio_list[[j]]) <- c("ratio", "vols", "stock", "opt_combo", "expy_combo", "oi", "strike", "delta", "price", "optclose", "opttype",
                                  "moneyness", "trpice")
  }
}

ratio_list <- ratio_list[lengths(ratio_list) != 0]

for (k in 1:length(ratio_list)) {
  ratio_list[[k]]$ratio <- as.data.frame(ratio_list[[k]]$ratio)
  ratio_list[[k]]$stock <- as.data.frame(ratio_list[[k]]$stock)
  ratio_list[[k]]$price <- as.data.frame(ratio_list[[k]]$price)
  ratio_list[[k]]$strike <- as.data.frame(ratio_list[[k]]$strike)
  ratio_list[[k]] <- do.call(cbind, ratio_list[[k]])
  colnames(ratio_list[[k]]) <- c("ratio", "vols", "ticker", "combo", "dates", "oi", "strike", "delta", "stock_price", "optclose", "opttype",
                                 "moneyness", "tprice")
}

ratio_list <- rbindlist(ratio_list)

ratio_list <- ratio_list %>% separate(vols, c("s_vol", "l_vol"), "  ")
ratio_list <- ratio_list %>% separate(combo, c("s_combo", "l_combo"), "  ")
ratio_list <- ratio_list %>% separate(dates, c("s_dates", "l_dates"), "  ")
ratio_list <- ratio_list %>% separate(oi, c("s_oi", "l_oi"), "  ")
ratio_list <- ratio_list %>% separate(delta, c("s_delta", "l_delta"), "  ")
ratio_list <- ratio_list %>% separate(optclose, c("s_close", "l_close"), "  ")
ratio_list <- ratio_list %>% separate(opttype, c("s_type", "l_type"))
ratio_list <- ratio_list %>% separate(moneyness, c("s_mny", "l_mny"), sep = "  ")
ratio_list <- ratio_list %>% separate(tprice, c("s_tp", "l_tp"), sep = "  ")

ratio_list$s_oi <- as.numeric(ratio_list$s_oi)
ratio_list$l_oi <- as.numeric(ratio_list$l_oi)
ratio_list$s_combo <- as.numeric(ratio_list$s_combo)
ratio_list$l_combo <- as.numeric(ratio_list$l_combo)
ratio_list$s_vol <- round(as.numeric(ratio_list$s_vol), 2)
ratio_list$l_vol <- round(as.numeric(ratio_list$l_vol), 2)
ratio_list$s_dates <- as.Date(ratio_list$s_dates)
ratio_list$l_dates <- as.Date(ratio_list$l_dates)
ratio_list$s_delta <- round(as.numeric(ratio_list$s_delta), 2)
ratio_list$l_delta <- round(as.numeric(ratio_list$l_delta), 2)
ratio_list$strike <- as.numeric(as.character(ratio_list$strike))
ratio_list$s_close <- as.numeric(ratio_list$s_close)
ratio_list$l_close <- as.numeric(ratio_list$l_close)
ratio_list$stock_price <- as.numeric(as.character(ratio_list$stock_price))
ratio_list$s_mny <- round(as.numeric(ratio_list$s_mny), 2)
ratio_list$l_mny <- round(as.numeric(ratio_list$l_mny), 2)
ratio_list$ratio <- round(ratio_list$ratio, 2)
ratio_list$s_tp <- round(as.numeric(ratio_list$s_tp), 2)
ratio_list$l_tp <- round(as.numeric(ratio_list$l_tp), 2)





# setnames(ratio_list, old = c("s_optclose", "l_optclose", "s_opttype", "l_opttype", "s_moneyness", "l_moneyness"), 
#          new = c("s_close", "l_close", "s_type", "l_type", "s_mny", "l_mny"))


z <- ratio_list %>% 
  filter(ratio >= 1.05 & s_oi >= 2000 & l_oi >= 2000 & s_dates >= "2020-06-01" & l_dates <= "2020-12-31" & stock_price >= 10 & s_vol >= .15 & 
           abs(s_delta) >= .2 & abs(s_delta) <= .8 & ticker != "^VIX" & ticker != "OXY") %>%
  arrange(desc(ratio))

z_a <- ratio_list %>% 
  filter(ratio >= 1.05 & s_oi >= 2000 & l_oi >= 2000 & s_dates >= "2020-07-01" & l_dates <= "2020-12-31" & stock_price >= 10 & s_vol >= .15 & 
           abs(s_delta) >= .2 & abs(s_delta) <= .8 & ticker != "^VIX" & ticker != "OXY") %>%
  arrange(desc(ratio))


rm(combo_list)
rm(date_list)
rm(delta_list)
rm(moneyness_list)
rm(oi_list)
rm(opt_type_list)
rm(optclose_list)
rm(x)
rm(tprice_list)
rm(vol_list)
rm(price)
rm(stock)
rm(strike)

write.csv(z, paste("C:/Users/pjant/Trading/Daily Equity Option Ratio Runs/","z_",format(now(),"%Y%m%d"),
                   ".csv", sep=""))
write.csv(z_a, paste("C:/Users/pjant/Trading/Daily Equity Option Ratio Runs/","z_",format(now(),"%Y%m%d_a"),
                   ".csv", sep=""))
write.csv(ratio_list, 
          paste("C:/Users/pjant/Trading/Daily Equity Option Ratio Runs/","ratio_list_",format(now(),"%Y%m%d"),
                ".csv", sep=""))

rm(eqOptionsFile)
rm(proFormaTable)
rm(split.df)


ratio_list %>%
  filter(ratio >= .9 & ticker == "TLT" & s_dates == "2019-07-12") %>%
  arrange(desc(ratio))

ratio_list %>%
  filter(ratio >= 1.00 & ticker == "^VIX" & s_dates >= "2019-08-15") %>%
  arrange(desc(ratio))
  

ratio_list %>% 
  filter(s_oi >= 500 & l_oi >= 500 & l_dates <= "2019-08-01" & stock_price >= 5) %>%
  summarise(mean(ratio), median(ratio), min(ratio), max(ratio), sd(ratio), n())


ratio_list %>% 
  filter(s_oi >= 1000 & l_oi >= 1000 & s_dates >= "2019-08-15" & l_dates <= "2020-12-31" & stock_price >= 10 & s_vol >= .15) %>%
  summarise(mean(ratio), median(ratio), min(ratio), max(ratio), sd(ratio), n())



## ----------------------------------------------------------------------------------------------------------------------------------
## Run the below code possibly 1x / week to keep an eye on the changing profile of equity option expirations

# check number of options expiring on each expiration date
nonNA_counts <- sapply(atmTableCallsVol, function(x) sum(!is.na(x)))
nonNA_counts <- as.data.frame(nonNA_counts)
nonNA_counts <- nonNA_counts[-c(1:4), , drop = FALSE]

# graph the number of options expiring on each expiration date
par(mar = c(7, 4, 4, 1) + .1)
barplot(nonNA_counts[,1], yaxt = "none", names.arg = rownames(nonNA_counts), las = 2, cex.names = .9, ylab = "Number of Options")
axis(2, seq(0, 4000, 500), las = 2, hadj = 1)
mtext(side = 3, line = 1.0, "Number of Option Expiries by Date for all ATM Equity Options")

# graph the number of options for each equity
byEquity <- rowSums(atmTableCallsVol[, -c(1:4)] >= 0, na.rm = TRUE)
byEquity <- as.data.frame(byEquity)
summary(byEquity)
hist(byEquity$byEquity, xlab = "Number of Options", ylab = "Number of Equities", main = "Number of Options per Equity", labels = T,
     breaks = 30, include.lowest = TRUE, xlim = c(1, 35), xaxp = c(1, 35, 34))

ggplot(as.data.frame(byEquity$byEquity), aes(x = data.frame(byEquity[1]))) + geom_bar() + scale_x_continuous(breaks = 1:40)



## ---------------------------------------------------------------------------------------------------------------------------------

# run a linear interpolation on all equities in the atmCallsTable to the dates specified in the optDates list below - this list needs to be updated
# every week or two

optDatesList <- as.Date(c("2019/05/17", "2019/06/21", "2019/07/19", "2019/08/16" ,"2019/09/20", "2019/10/18", "2019/11/15", "2019/12/20", 
                          "2020/01/17", "2021/01/15"), origin = "1970-01-01")


callsPriceMatrix <- atmTableCallsPrice[, as.character(optDatesList)]
callsVolMatrix <- atmTableCallsVol[, as.character(optDatesList)]
putsPriceMatrix <- atmTablePutsPrice[, as.character(optDatesList)]
putsVolMatrix <- atmTablePutsVol[, as.character(optDatesList)]

rownames(callsPriceMatrix) <- make.names(atmTableCallsPrice$symbol, unique = TRUE)
rownames(callsVolMatrix) <- make.names(atmTableCallsVol$symbol, unique = TRUE)
rownames(putsPriceMatrix) <- make.names(atmTablePutsPrice$symbol, unique = TRUE)
rownames(putsVolMatrix) <- make.names(atmTablePutsVol$symbol, unique = TRUE)

# atmTableCallsPrice <- atmTableCallsPrice[apply(atmTableCallsPrice, 1, function(y) any(!is.na(y))),]
# atmTablePutsPrice <- atmTablePutsPrice[apply(atmTablePutsPrice, 1, function(z) any(!is.na(z))),]

atmCallsFinal <- combn(callsPriceMatrix, 2, function(x) x[,1]/x[,2])
atmCallsFinal <- as.data.frame(atmCallsFinal)
rownames(atmCallsFinal) <- rownames(callsPriceMatrix)
colnames(atmCallsFinal) <- combn(callsPriceMatrix, 2, function(y) paste(colnames(y[1]),"/",colnames(y[2])))
atmCallsFinal <- format(round(atmCallsFinal, 3))




ticker <- "AAPL"
equityTable <- proForma[grepl(ticker, proForma$symbol),]
equityTable <- equityTable[, colSums(equityTable != 0) > 0]
rownames(equityTable) <- equityTable[,1]
equityTable <- equityTable[,-1]

tequityTable <- as.data.frame(t(as.data.frame(equityTable)))
tequityTable$date <- rownames(tequityTable)
tequityTable <- melt(tequityTable, id.vars = c("date"))
ggplot(tequityTable, aes(x = date, y = value, color = variable)) +
  geom_point()
