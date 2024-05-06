# Overview
yfinanceDV is an R package designed as an extension of the quantmod package, focusing on visualizing financial data(e.g. Stock Indices, Commodities, etc.) sourced from Yahoo Finance using Plotly. 
This package facilitates the creation of candlestick charts enriched with volume, simple moving averages, and various technical indicators(for example RSI, MACD, etc.). 
With yfinanceDV, users can easily generate insightful visual representations of financial data to aid in analysis and decision-making.

# Installation
Package can be installed directly from GitHub using the devtools package:
```
devtools::install_github("Yann729/yfinanceDV")
```

# Usage
Examples of plotting Financial Data Visualization
```
library(yfinanceDV)

# Plotting stock data for AAPL
pltDV("AAPL")

# Plotting stock data for AAPL from a specific date
pltDV("AAPL", from = "2022-01-01")

# Plotting stock data for AAPL within a date range
pltDV("AAPL", from = "2021-01-01", to = "2022-01-01")

# Plotting stock data for AAPL within a date range with a 50-period SMA
pltDV("AAPL", from = "2021-01-01", to = "2022-01-01", SMA_period = 50)
```
# Contributing
Contributions to yfinanceDV are welcome! If you find any issues or have suggestions for improvements, please open an issue on GitHub.

# License
This package is under the MIT License. See the LICENSE file for details.

Please feel free to customize it further! Let me know if you need any more assistance.
