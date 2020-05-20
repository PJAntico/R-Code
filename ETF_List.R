library(Rblpapi)
library(writexl)
blpConnect()

assets <- c("HYG US Equity","SPY US Equity","EEM US Equity","UVXY US Equity", "SLV US Equity",
            "QQQ US Equity","XOP US Equity","GLD US Equity","XLF US Equity", "TLT US Equity", 
            "SMH US Equity","XBI US Equity","XLU US Equity")


attribute <- c("FUND_TOTAL_ASSETS", "EQY_SH_OUT_REAL", "CUR_MKT_CAP", "FUND_NET_ASSET_VAL",
               "TOTAL_NUMBER_OF_HOLDINGS_IN_PORT", "FUND_STRATEGY", "FUND_RTG_CLASS_FOCUS")

total_assets <- NULL
shares_outstanding <- NULL
market_cap <- NULL
nav <- NULL
num_holdings <- NULL
fund_strategy <- NULL
fund_rating_focus <- NULL
df <- NULL

i = 1
for (i in 1:length(assets)) {
  total_assets[i] <- bdp(assets[i], attribute[1])
  shares_outstanding[i] <- bdp(assets[i], attribute[2])
  market_cap[i] <- bdp(assets[i], attribute[3])
  nav[i] <- bdp(assets[i], attribute[4])
  num_holdings[i] <- bdp(assets[i], attribute[5])
  fund_strategy[i] <- bdp(assets[i], attribute[6])
  fund_rating_focus[i] <- bdp(assets[i], attribute[7])
}

df <- cbind(total_assets, shares_outstanding, market_cap, nav, num_holdings, fund_strategy,
            fund_rating_focus)
df <- as.data.frame(df)
rownames(df) <- c(assets)

df <- format(df, big.mark = ",")
print(df)

write_xlsx(df,"df.xlsx")
