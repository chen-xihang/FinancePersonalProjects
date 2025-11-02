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

test <- tibble(
  date = index(brent),
  price = as.numeric(brent)
) |>
  dplyr::filter(year(date)>2022, year(date)<=2024) |>
  bind_rows(
    tibble(
      date  = as.Date(c("2022-12-31", "2023-01-01","2023-01-02","2023-01-03")), # forward fill
      price = c(84.33, 83.260, 82.26, 85.91)
    )
  ) |>
  mutate(date = as.Date(date)) |>
  arrange(date) |>
  distinct(date, .keep_all = TRUE) |>
  as_tsibble(index = date) |>
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

# ARIMA with d = 1
fit <- brent_tsbl %>%
  model(arima_fixed_d = ARIMA(price ~ pdq(d = 1)))

brent_tsbl %>% 
  model(arima = ARIMA(price)) # gives the same ARIMA model, minimises AICc

fit %>% gg_tsresiduals() # plotting residues

fit %>%
  augment() %>%
  autoplot(.fitted, colour = "green") +   # predicted (fitted) = black
  autolayer(brent_tsbl, price, colour = "blue") +  # actual = red
  labs(title = "ARIMA Fitted vs Actual (Training Set)",
       y = "Price", x = "Time")

# checking with test data set
fit_2y <- fit |> forecast(h = "2 years")
fit_2y_test <- fit |> forecast(new_data = test)

autoplot(fit_2y, brent_tsbl) +
  labs(title = "ARIMA Forecast: 2 Years Ahead",
       y = "Price", x = "Time")

autoplot(fit_2y_test, brent_tsbl) +
  labs(title = "ARIMA Forecast: 2 Years Ahead",
       y = "Price", x = "Time")
