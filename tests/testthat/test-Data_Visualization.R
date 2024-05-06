test_that("pltDV function working in different scenarios", {
  # No date range specified while using default parameters
  expect_warning(
    pltDV("AAPL"),
    "No date range specified. Defaulting to whole historical data of the symbol."
  )

  # No start date specified while using default parameters
  expect_warning(
    pltDV("AAPL", to = "2022-03-01"),
    "No start date specified. Data will be retrieved from the beginning to the end date."
  )

  # No end date specified while using default parameters
  expect_warning(
    pltDV("AAPL", from = "2022-01-01"),
    "No end date specified. Data will be retrieved from the start date to the present."
  )

  # Function runs normally with 'from' and 'to' dates specified
  pltDV("AAPL", from = "2021-01-01", to = "2022-01-01")

  # Function runs normally with 'from', 'to', and SMA period specified
  pltDV("AAPL", from = "2021-01-01", to = "2022-01-01", SMA_period = 100)
})


test_that("pltDV function handles missing or invalid inputs", {
  # Missing symbol
  expect_error(pltDV(), "Please check if the input symbol is vaild or format of 'from' and 'to' dates are correct.")
  # Invalid symbol
  expect_error(pltDV("INVALID_SYMBOL"), "Please check if the input symbol is vaild or format of 'from' and 'to' dates are correct.")
  # Invalid date format
  expect_error(pltDV("AAPL", from = INVALID_DATE), "Please check if the input symbol is vaild or format of 'from' and 'to' dates are correct.")
  expect_error(pltDV("AAPL", from = "12345"), "Please check if the input symbol is vaild or format of 'from' and 'to' dates are correct.")
  # Date chaos('from' date later than 'to' date)
  expect_error(pltDV("AAPL", from = "2023-01-01", to = "2022-01-01"), "Please check if the input symbol is vaild or format of 'from' and 'to' dates are correct.")
})

test_that("pltDV function handles SMA period greater than the range", {
  expect_warning(
    pltDV("AAPL", from = "2022-01-01", to = "2022-01-31", SMA_period = 50),
    "The period of SMA should be larger than the period between 'from' and 'to' dates. SMA plotting will be omitted.")
})

test_that("Plot structure is correct", {
  # Execute the plotting function
  plot <- pltDV("AAPL", from = "2020-01-01", to = "2021-01-01", SMA_period = 50, indicator = "RSI")

  # Check if the main plot contains the expected traces (e.g., candlestick chart, SMA line, volume bar)
  expect_true("Price" %in% plot$x$data[[1]]$name)  # Candlestick chart
  expect_true("50 Period SMA" %in% plot$x$data[[2]]$name)  # SMA line
  expect_true("Volume" %in% plot$x$data[[3]]$name)  # Volume bar
  expect_true("RSI" %in% plot$x$data[[4]]$name)  # Volume bar
})




