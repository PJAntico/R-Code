Sys.setenv("plotly_username" = "PACapital")
Sys.setenv("plotly_api_key" = "")

require("openxlsx")
require("dplyr")
require("data.table")
require("arsenal")
require("lubridate")
require("timeDate")
require("ggplot2")
require("DescTools")
require("plyr")
require("Rblpapi")
require("reshape2")
require("tidyr")
require("RQuantLib")
require("plotly")


## Create a file from the CME data

cme.settle <- read.csv("ftp://ftp.cmegroup.com/settle/cme.settle.20200507.s.csv")
cme.slate <- read.xlsx("C:/Users/pjant/Downloads/Product Slate Export.xlsx")

# to see how many Futures are actually in the larger "Settle" data.frame.  Then to count the number of futures by symbol.
d <- cme.settle[cme.settle$SecTyp == "FUT" | cme.settle$SecTyp == "OOF",]  # dataframe of Futures and Options in the larger "settle" data.frame
d <- cbind(d, cme.slate[match(d$Sym, cme.slate$Globex), c("Product.Name","Product.Group","Sub.Group")])  # add the Product Name descriptor to each future in "d"

# create the "Product Name" key first by reducing the data.frame 'd' to just symbol and product name, and then
# getting rid of duplicates so only unique value are shown
key <- d[, c(1:2, 30:32)]
key <- unique(key)


# look up the Product Name in the key and add it to the settle data frame
settle <- cbind(cme.settle, key[match(cme.settle$Sym, key$Sym), c("Product.Name","Product.Group","Sub.Group")])
settle$BizDt <- as.Date(settle$BizDt, origin = "1899-12-30")
settle$MatDt <- as.Date(settle$MatDt, origin = "1899-12-30")
settle$LastTrdDt <- as.Date(settle$LastTrdDt, origin = "1899-12-30")

# change the UndlyMMY date (which is just in "MMYYYY" format), to the proper futures expiration date. First line of code below
# first changes "NA's" to 1/1/1970.  Have to do this because the NA's prevent the function timeNthNdayInMonth from working
settle$UndlyMMY[is.na(settle$UndlyMMY)] <- as.numeric(format.Date("1970-01-01", "%Y%m"))
settle$UndlyMMY <- as.Date(timeNthNdayInMonth(parse_date_time(settle$UndlyMMY[!is.na(settle$UndlyMMY)], 
                                                              "ym"), nday = 3, nth = 3, format = "%Y-%m-%d"), origin = "1899-12-30")
settle$UndlyMMY <- settle$UndlyMMY - 2

# aggregate the open interest for ALL strikes in a given option, into one line - then later used this one number in the final file as
# the open interest for the contract
oiByContract <- aggregate(settle$PrevDayOI, by = list(settle$Sym, settle$MatDt, settle$UndlyMMY), FUN = sum)
colnames(oiByContract) <- c("Sym", "MatDt", "UndlyMMY", "OpenInt")


# define which options products want in the final file
eurodollarSymbols <- c("E01","E02","E03","E04","E21","E22","E23","E24","E31","E32","E33","E34",
                       "GE","GE0", "GE2","GE3","GE4","GE5","TE2","TE3","TE4")

# create the key for the underlying futures closing prices by first reducing the data.frame settle to just symbol, future, 
# UndlyMMY, and settle price, renaming this to "futuresKey", and then getting rid of duplicates so only unique values are shown
futuresKey <- settle[settle$SecTyp == "FUT", c(2, 5, 7, 14)]
eurodollarFuturesKey <- subset(futuresKey, futuresKey$Sym %in% eurodollarSymbols)



# subset the settle file, keeping all rows with a symbol that is in the "eurodollarSymbols" list
largeedMatrix <- subset(settle, settle$Sym %in% eurodollarSymbols)

# get rid of any rows that have NA's in the product group - NOT SURE THAT NEED THIS LINE OF CODE
largeedMatrix <- subset(largeedMatrix, !is.na(largeedMatrix$Product.Group))
largeedMatrix$PutCall <- replace(largeedMatrix$PutCall, largeedMatrix$PutCall %in% c(1,0), c("C","P"))

# the symbol %>% passes the left hand side of the operator to the first argument of the right hand side of the operator.  The
# MatDt and UndlyMMY are the "optional variables to use when determining uniqueness", as defined in the help documentation for
# the function "distinct".  The .keep_all = T tells the function to keep all variables in "a".
smalledMatrix <- largeedMatrix %>% distinct(MatDt, UndlyMMY, PutCall, .keep_all = T)

# narrow "smalledMatrix" down to just Options
edOptionsMatrix <- smalledMatrix[smalledMatrix$SecTyp == "OOF" & smalledMatrix$PutCall == "P",]

edOptionsMatrix <- select(edOptionsMatrix, BizDt, Sym, ID, PutCall, SecTyp, MatDt, UndlyMMY, Product.Name, Product.Group, PrevDayOI)
edOptionsMatrix <- merge(edOptionsMatrix, oiByContract, by = c("Sym", "MatDt", "UndlyMMY"))

edOptionsMatrix <- cbind(edOptionsMatrix, eurodollarFuturesKey[match(edOptionsMatrix$UndlyMMY, eurodollarFuturesKey$MatDt),
                                                               c("Sym", "SecTyp", "MatDt", "SettlePrice")])



# get atm strike for each option and add it as a column to edOptionsMatrix
strikeMatrix <- NULL
i = 1
for(i in 1:nrow(edOptionsMatrix)){
  strikeMatrix[[i]] <- largeedMatrix[edOptionsMatrix$MatDt[[i]] == largeedMatrix$MatDt &
                                       edOptionsMatrix$UndlyMMY[[i]] == largeedMatrix$UndlyMMY &
                                       largeedMatrix$PutCall == "P", ]
  strikeMatrix[[i]] <- rbind(strikeMatrix[[i]])
  edOptionsMatrix$Strike[[i]] <- Closest(strikeMatrix[[i]]$StrkPx, edOptionsMatrix$SettlePrice[[i]])
  edOptionsMatrix$optSettlePrice[[i]] <- as.numeric(strikeMatrix[[i]][Closest(strikeMatrix[[i]]$StrkPx, edOptionsMatrix$SettlePrice[[i]], which = TRUE),
                                                           "SettlePrice"])
}

colnames(edOptionsMatrix)[12:15] <- c("FutSym", "FutSecType", "FutMatDt", "FutSettlePrice")
## End of section creating the file from the CME data


# Calculate options Implied Volatility
type <- NULL
value <- NULL
underlying <- NULL
strike <- NULL
dividendYield <- NULL
riskFreeRate <- NULL
maturity <- NULL
volatility <- NULL
impliedVolatility <- NULL

i = 1
edOptionsMatrix$MatDt <- as.Date(edOptionsMatrix$MatDt)
edOptionsMatrix$BizDt <- as.Date(edOptionsMatrix$BizDt)
edOptionsMatrix$UndlyMMY <- as.Date(edOptionsMatrix$UndlyMMY)
for(i in 1:nrow(edOptionsMatrix)) {
    isub = c(1,2,4,16,22:26)
    if(i %in% isub) next
    if((edOptionsMatrix$MatDt[[i]] - edOptionsMatrix$BizDt[[i]]) <= 7 || edOptionsMatrix$optSettlePrice[[i]] > 1000) next()
    type[[i]] <- "call"
    value[[i]] <- edOptionsMatrix$optSettlePrice[[i]] * 100
    underlying[[i]] <- (10000 - edOptionsMatrix$FutSettlePrice[[i]]*100)
    strike[[i]] <- (10000 - edOptionsMatrix$Strike[[i]]*100)
    dividendYield[[i]] <- 0.00
    riskFreeRate[[i]] <- 0.00
    maturity[[i]] <- (edOptionsMatrix$MatDt[[i]] - edOptionsMatrix$BizDt[[i]]) / 360
    volatility[[i]] <- .6
    impliedVolatility[[i]] <- EuropeanOptionImpliedVolatility(type[[i]], value[[i]], underlying[[i]], strike[[i]], dividendYield[[i]],
                                                             riskFreeRate[[i]], maturity[[i]], volatility[[i]])
    impliedVolatility[[i]] <- impliedVolatility[[i]] * (10000-(edOptionsMatrix$FutSettlePrice[[i]]*100))
    rownames(edOptionsMatrix)[[i]] <- i
}

edOptionsMatrix$ImpVol <- as.numeric(format(impliedVolatility, digits = 2))
edOptionsMatrix$optSettlePrice <- edOptionsMatrix$optSettlePrice * 100
edOptionsMatrix <- edOptionsMatrix[complete.cases(edOptionsMatrix),]
edOptionsMatrix$MatDt <- as.character(edOptionsMatrix$MatDt)

# create bloomberg symbols for each row in the largeedMatrix
bberg.symbols <- c("1K", "2K", "3K", "4K", "1F", "2F", "3F", "4F", "1P", "2P", "3P", "4P", "ED", "0E", "2E", "3E", "4E", "5E", "TT", "TS", "TN")
bberg.months <- c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z")
edOptionsMatrix$bbergSym <- bberg.symbols[match(edOptionsMatrix$Sym, eurodollarSymbols)]
edOptionsMatrix$bbergSym <- paste(edOptionsMatrix$bbergSym, bberg.months[month(edOptionsMatrix$MatDt)], substr(year(edOptionsMatrix$MatDt),4,4),
                                edOptionsMatrix$PutCall," ",edOptionsMatrix$Strike," Comdty", sep = "")



p <- ggplot(edOptionsMatrix, aes(x = MatDt, y = UndlyMMY, size = OpenInt, color = Product.Name, UnderlyingP = FutSettlePrice, 
                                 Symbol = bbergSym, Price = optSettlePrice, Vol = ImpVol)) + 
  geom_point() +
  geom_text(aes(label = edOptionsMatrix$ImpVol), size = 2.0, nudge_y = -50) +
  geom_text(aes(label = edOptionsMatrix$optSettlePrice), size = 2.0, nudge_y = 50) +
  scale_size_continuous() +
  scale_x_discrete(name = "Option Expiration", breaks = edOptionsMatrix$MatDt) +
  scale_y_date(name = "Underlying Eurodollar Future", breaks = edOptionsMatrix$UndlyMMY) +
  ggtitle("Eurodollar Options Universe") +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme(plot.title = element_text(hjust = 0.5))

# edOptionsMatrix$MatDt <- as.Date(edOptionsMatrix$MatDt, origin = "1970-01-01")
# edOptionsMatrix$UndlyMMY <- as.Date(edOptionsMatrix$UndlyMMY, origin = "1970-01-01")

# g = tableGrob(unique(cbind(edOptionsMatrix$FutMatDt, edOptionsMatrix$FutSettlePrice)), theme = ttheme_minimal())
# p <- p + annotation_custom(grob = g, xmin = edOptionsMatrix$MatDt[16], xmax = edOptionsMatrix$MatDt[20],
#                            ymin = edOptionsMatrix$UndlyMMY[20], 
#                            ymax = edOptionsMatrix$UndlyMMY[25])

p <- p + theme()
p <- ggplotly(p, tooltip = c("x", "y", "size", "color", "UnderlyingP", "Symbol", "Price", "ImpVol"))
print(p)

htmlwidgets::saveWidget(as_widget(p), paste("c:/users/pjant/Trading/Eurodollar Option Graphs/EurodollarOptionGraph ", 
                                            format(now(), "%Y%m%d_%H%M%S.html")))
ggsave(paste("c:/users/pjant/Trading/Eurodollar Option Graphs/EurodollarOptionGraph ", format(now(), "%Y%m%d_%H%M%S.pdf")))
