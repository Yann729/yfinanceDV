#' Plot data visualization for financial data sourced from yfinance
#'
#' This function plots a visualization of financial data, including candlestick charts, simple moving averages,
#' volume bars, and optional technical indicators (e.g., RSI, MACD, Stochastic Oscillator).
#'
#' @param symbol The symbol of data used for plotting (e.g., Stock Indices(AAPL, AMZN), Futures Contracts(CL=F, ),
#' Commodities(GOLD), Forex(EURUSD=X), etc).
#' @param from Start date for retrieving financial data (default: "1000-05-01").
#' If not specified, data will be retrieved from the earliest available date.
#' @param to End date for retrieving stock data (default: "4000-01-01").
#' If not specified, data will be retrieved up to the latest available date.
#' @details If the \code{from} parameter is set to "1000-05-01" and the \code{to} parameter is set to "4000-01-01",
#' the function will retrieve the entire historical data available for the specified symbol.
#' This means it will fetch data from the earliest available date up to the latest available date.
#'
#' @param SMA_period Period for the simple moving average calculation (default: 50).
#' @param indicator Optional technical indicator to include in the visualization (choices: "RSI", "MACD", "Stochastic").
#' @return A Plotly object displaying the stock visualization.
#'
#' @examples
#' pltDV("AAPL", from = "2020-01-01", to = "2021-01-01",
#' SMA_period = 50, indicator = "RSI")
#'
#' @seealso \code{\link{getSymbols}} function in the \code{quantmod} package.
#' @importFrom quantmod getSymbols
#' @importFrom grDevices adjustcolor
#' @importFrom plotly plot_ly subplot
#' @importFrom xts as.xts
#' @importFrom dplyr %>%
#' @importFrom zoo index coredata
#' @importFrom TTR SMA RSI MACD stoch
#'
#' @export
pltDV <- function(symbol, from = "1000-05-01", to = "4000-01-01"
                  , SMA_period = 50, indicator = NULL) {
  # Retrieve financial data sourced from yfinance
  tryCatch({
    data <- quantmod::getSymbols(symbol, src = "yahoo", from = from, to = to, auto.assign = FALSE)
  }, error = function(e) {
    stop("Please check if the input symbol is vaild or format of 'from' and 'to' dates are correct.")
  })

  # Transform data to xts object
  data_xts <- xts::as.xts(data)

  # Create a dataframe from the stock data
  df <- data.frame(Date = zoo::index(data_xts), zoo::coredata(data_xts))
  names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

  # Warn if SMA period is less than the period between 'from' and 'to' dates
  if ((as.Date(to) - as.Date(from)) < SMA_period) {
    warning("The period of SMA should be larger than the period between 'from' and 'to' dates. SMA plotting will be omitted.")
    SMA_period <- NULL  # Set SMA_period to NULL to skip SMA plotting
  }

  # Color palette dictionary for different indicators
  colors <- list(
    SMA = grDevices::adjustcolor("#006BA2", alpha.f = 0.7),
    Volume = grDevices::adjustcolor("pink", alpha.f = 0.3),
    RSI = grDevices::adjustcolor("#925977", alpha.f = 0.5),
    MACD = grDevices::adjustcolor("#FFA500", alpha.f = 0.6),
    Stochastic = grDevices::adjustcolor("red", alpha.f = 0.8)
  )

  # Plot using Plotly
  main_plot <- plotly::plot_ly(data = df, type = "candlestick",
                               x = ~Date,
                               open = ~Open,
                               close = ~Close,
                               high = ~High,
                               low = ~Low,
                               increasing = list(line = list(color = "rgba(0, 191, 191, 1)")),
                               decreasing = list(line = list(color = "#8A8A8A")),
                               name = "Price")

  # Add SMA trace if SMA_period is not NULL
  if (!is.null(SMA_period)) {
    sma_col_name <- paste(SMA_period, "Period SMA")
    df[[sma_col_name]] <- TTR::SMA(df$Close, n = SMA_period)

    main_plot <- main_plot %>%
      plotly::add_trace(data = df, x = ~Date, y = ~df[[sma_col_name]], type = "scatter", mode = "lines", name = paste(SMA_period, "Period SMA"),
                        line = list(color = colors$SMA))
  }

  main_plot <- main_plot %>%
    plotly::add_trace(data = df, x = ~Date, y = ~Volume, type = "bar", name = "Volume", yaxis = "y2",
                      marker = list(color = colors$Volume)) %>%

    plotly::layout(title = paste("Data Visualization for", symbol),
                   xaxis = list(title = "Date", tickformat = "%Y-%m-%d"),  # Set tickformat to display only date part
                   yaxis = list(title = "Price", side = "left"),  # Set left y-axis title
                   yaxis2 = list(title = "Volume", overlaying = "y", side = "right"),  # Secondary y-axis for volume
                   showlegend = TRUE,
                   paper_bgcolor = grDevices::adjustcolor("#BFD8E5", alpha.f = 0.7),  # Set background color for the entire plot
                   plot_bgcolor = grDevices::adjustcolor("#BFD8E5", alpha.f = 0.7))   # Set background color for the plot area

  # Warn users about missing date inputs
  if (from == "1000-05-01" & to == "4000-01-01") {
    warning("No date range specified. Defaulting to whole historical data of the symbol.")
  } else if (from == "1000-05-01") {
    warning("No start date specified. Data will be retrieved from the beginning to the end date.")
  } else if (to == "4000-01-01") {
    warning("No end date specified. Data will be retrieved from the start date to the present.")
  }

  # Conditionally add technical indicator subplot
  if (!is.null(indicator)) {
    if (indicator %in% c("RSI", "MACD", "Stochastic")) {
      # Calculate technical indicator
      if (indicator == "RSI") {
        df$Indicator <- TTR::RSI(df$Close)

        # Add overbought and oversold lines
        overbought <- 70
        oversold <- 30
        add_lines <- list(
          list(type = "line", xref = "paper", x0 = 0, y0 = overbought, x1 = 1, y1 = overbought, line = list(color = "red", width = 1.5, dash = "dot")),
          list(type = "line", xref = "paper", x0 = 0, y0 = oversold, x1 = 1, y1 = oversold, line = list(color = "red", width = 1.5, dash = "dot"))
        )

        # Create indicator subplot
        indicator_subplot <- plotly::plot_ly(data = df, x = ~Date, y = ~Indicator, type = "scatter", mode = "lines", name = indicator,
                                             line = list(color = colors[[indicator]])) %>%
          plotly::layout(xaxis = list(title = "Date", tickformat = "%Y-%m-%d"),  # Set tickformat to display only date part
                         yaxis = list(title = "Price", side = "left"),  # Set left y-axis title
                         yaxis2 = list(title = "Volume", overlaying = "y", side = "right"),  # Secondary y-axis for volume
                         paper_bgcolor = grDevices::adjustcolor("#BFD8E5", alpha.f = 0.7),  # Set background color for the entire subplot
                         plot_bgcolor = grDevices::adjustcolor("#BFD8E5", alpha.f = 0.7),   # Set background color for the subplot area
                         shapes = add_lines,
                         xaxis = list(range = range(df$Date)))  # Match x-axis range with main plot

      } else if (indicator == "MACD") {
        macd <- TTR::MACD(df$Close)
        df$Indicator <- macd[, "macd"]
        add_lines <- NULL

        # Create indicator subplot
        indicator_subplot <- plotly::plot_ly(data = df, x = ~Date, y = ~Indicator, type = "scatter", mode = "lines", name = indicator,
                                             line = list(color = colors[[indicator]])) %>%
          plotly::layout(xaxis = list(title = "Date", tickformat = "%Y-%m-%d"),  # Set tickformat to display only date part
                         yaxis = list(title = "Price", side = "left"),  # Set left y-axis title
                         yaxis2 = list(title = "Volume", overlaying = "y", side = "right"),  # Secondary y-axis for volume
                         paper_bgcolor = grDevices::adjustcolor("#BFD8E5", alpha.f = 0.7),  # Set background color for the entire subplot
                         plot_bgcolor = grDevices::adjustcolor("#BFD8E5", alpha.f = 0.7),   # Set background color for the subplot area
                         shapes = add_lines,
                         xaxis = list(range = range(df$Date)))  # Match x-axis range with main plot

      } else if (indicator == "Stochastic") {
        # Calculate Stochastic Oscillator
        stoch <- TTR::stoch(df[,c("High","Low","Close")])
        df$K <- stoch[,"fastK"] * 100
        df$D <- stoch[,"fastD"] * 100

        # Add overbought and oversold lines
        overbought <- 80
        oversold <- 20
        add_lines <- list(
          list(type = "line", xref = "paper", x0 = 0, y0 = overbought, x1 = 1, y1 = overbought, line = list(color = "red", width = 1.5, dash = "dot")),
          list(type = "line", xref = "paper", x0 = 0, y0 = oversold, x1 = 1, y1 = oversold, line = list(color = "red", width = 1.5, dash = "dot"))
        )

        # Create indicator subplot
        indicator_subplot <- plotly::plot_ly(data = df, x = ~Date, y = ~K, type = "scatter", mode = "lines", name = "%K",
                                             line = list(color = colors[[indicator]])) %>%
          plotly::add_trace(data = df, x = ~Date, y = ~D, type = "scatter", mode = "lines", name = "%D",
                            line = list(color = grDevices::adjustcolor("blue", alpha.f = 0.7))) %>%
          plotly::layout(xaxis = list(title = "Date", tickformat = "%Y-%m-%d"),  # Set tickformat to display only date part
                         yaxis = list(title = "Price", side = "left"),  # Set left y-axis title
                         yaxis2 = list(title = "Volume", overlaying = "y", side = "right"),  # Secondary y-axis for volume
                         paper_bgcolor = grDevices::adjustcolor("#BFD8E5", alpha.f = 0.7),  # Set background color for the entire subplot
                         plot_bgcolor = grDevices::adjustcolor("#BFD8E5", alpha.f = 0.7),   # Set background color for the subplot area
                         shapes = add_lines,
                         xaxis = list(range = range(df$Date)))  # Match x-axis range with main plot
      }

      # Combine the main plot and indicator subplot
      suppressWarnings(combined_plot <- plotly::subplot(main_plot, indicator_subplot, nrows = 2))

      # Modify the default legend label for the candlestick trace
      combined_plot$x$data[[1]]$name <- "Price"

      suppressWarnings(print(combined_plot))
    }
  } else {
    # If no indicator specified, print only the main plot
    suppressWarnings(print(main_plot))
  }
}

