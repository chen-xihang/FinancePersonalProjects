### Using ARIMA to forecast commodity prices
library(fpp3)
library(quantmod)
library(tidyverse)
library(fabletools)
library(tsibble)
library(dplyr)
library(imputeTS)

# calling historical prices
getSymbols("BZ=F", src = "yahoo", from = "2008-01-01", to = Sys.Date())
brent <- `BZ=F`[, "BZ=F.Close"]

brent_tsbl <- tibble(
  date = index(brent),
  price = as.numeric(brent)
) |>
  as_tsibble(index = date) |>
  dplyr::filter(year(date)<=2022) |>
  fill_gaps(date = seq.Date(min(date), max(date), by = "day")) |>
  dplyr::mutate(price = na_interpolation(price, option = "linear"))

brent_tsbl |> autoplot(price) + labs(title = "Brent Price Before 2022") 

# testing stationarity
brent_tsbl |> ACF(price) |> autoplot() #ACF 
brent_tsbl |> features(price, unitroot_kpss)

# one level difference
d1_brent <- brent_tsbl %>%
  mutate(price_diff = difference(price)) |>
  dplyr::filter(!is.na(.data$price_diff))|>
  as_tsibble()

d1_brent |> autoplot(price_diff) + labs(title = "Price Difference Before 2022") 
d1_brent |> ACF(price_diff) |> autoplot() + labs(title = "ACF of Price Difference Before 2022") 
d1_brent |> features(price_diff, unitroot_kpss) # d = 1


