library(Rblpapi)
library(xts)
library(PerformanceAnalytics)
library(ggplot2)

# Define the vector of equity names  - e.g. MU US Equity

equities <- c("SPY US", "HYG US", "AGNC US", "EEM US", "UVXY US", "SLV US",
              "QQQ US", "XOP US", "GLD US", "XLF US", "TLT US", "SMH US",
              "XBI US", "XLU US")

# Define the required fields - e.g. MU US 1M 100 VOL BVOL Equity

vols <- c("Equity")

# Define the start date and end date of the analysis

startDate <- as.Date("2019-12-10")
endDate <- as.Date("2020-5-20")

# Create a vector (row) of ticker names to be input into bdh

i = 1
j = 1
volNames <- matrix(NA, nrow = length(equities), ncol = length(vols))

for(i in 1:length(equities)){
  for(j in 1:length(vols)) {
    volNames[i, j] <- paste(equities[i], vols[j])
  }
}

volNames <- as.vector(t(volNames))

# Open Bloomberg connection
blpConnect()

# Pull Bloomberg data
undData <- bdh(volNames, "3MO_CALL_IMP_VOL", start.date = startDate, end.date = endDate)
undData <- undData[volNames]

# Put together a giant list of data for all tickers
tsList <- lapply(undData, function(x) xts(x[[2]], x[[1]]))
mydataLevels <- Reduce(merge, tsList)


# Give proper column names
headings <- bdp(names(tsList), "NAME_RT")
headings <- t(headings)

names(mydataLevels) <- headings

# Create the boxplot
boxplot(as.data.frame(mydataLevels), las = 2, par(mar = c(12, 5, 4, 2) + 0.1))

# Try new code to make the boxplot and then add a point on each for the latest observation
a <- stack(as.data.frame(mydataLevels))
b <- stack(as.data.frame(last(mydataLevels)))

g <- ggplot(a, aes(x = ind, y = values)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

g <- g +
  geom_point(data = b, aes(x = ind, y = values), color = "blue")

print(g)


