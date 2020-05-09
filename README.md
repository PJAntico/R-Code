# R-Code
R-Codes built for Derivatives Trading

File "Eurodollar Futures Options v5..." takes in every single Futures and Options closing price from the CME, pulls the Eurodollar Futures and Options out, sorts only the ATM options out, calculates the Implied Volatility of these ATM options, and then plots them al on a Plotly chart with 'Hoverpoints' showing more information.

File "Pro Forma Equity Screener" takes in 35 variables for 1mm+ (every) equity option, calculates an implied volatility for every one of them, orders them by strike, and outputs vol ratios for every combination of expiry per strike (per ticker).  The result is 100k+ of calendar spreads every day, filtered by a pre-specified minimum open interest on each side, ranked starting with highest ratio.
