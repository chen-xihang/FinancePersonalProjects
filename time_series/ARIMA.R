### Using ARIMA to forecast commodity prices
library(fpp3)
library(quantmod)
library(tidyverse)
library(fabletools)

# calling historical prices
getSymbols("BZ=F", src = "yahoo", from = "2008-01-01", to = Sys.Date())
brent <- `BZ=F`[, "BZ=F.Close"]
plot(brent, major.ticks = "year")

brent_df <- tibble(
  date = index(brent),
  price = as.numeric(brent)
)

brent_tsbl <- as_tsibble(brent_df, index = date)

training_data_brent <- brent_tsbl |>
  dplyr::filter(year(date)<=2022)
