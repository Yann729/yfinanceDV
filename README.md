# Overview
yfinanceDV is an R package that provides functions to visualize financial data sourced from Yahoo Finance using Plotly. 
It allows users to easily plot candlestick charts along with volume, moving averages, and other technical indicators.

# Installation
You can install the package directly from GitHub using the devtools package:
devtools::install_github("Yann729/yfinanceDV")

# Usage
Plotting Financial Data

library(yfinanceDV)

# Plotting stock data for AAPL
pltDV("AAPL")

# Plotting stock data for AAPL from a specific date
pltDV("AAPL", from = "2022-01-01")

# Plotting stock data for AAPL within a date range
pltDV("AAPL", from = "2021-01-01", to = "2022-01-01")

# Plotting stock data for AAPL within a date range with a 50-period SMA
pltDV("AAPL", from = "2021-01-01", to = "2022-01-01", SMA_period = 50)

# Contributing
Contributions to yfinanceDV are welcome! If you find any issues or have suggestions for improvements, please open an issue on GitHub.

# License
This package is distributed under the MIT License. See the LICENSE file for details.


Feel free to customize it further based on your specific package features and requirements! Let me know if you need any more assistance.
