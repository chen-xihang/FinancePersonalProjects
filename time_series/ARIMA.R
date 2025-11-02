library(fpp3)
library(quantmod)
library(tidyverse)
library(fabletools)

# Get data and aggregate to weekly to reduce noise
getSymbols("BZ=F", src = "yahoo", from = "2008-01-01", to = Sys.Date())
brent <- `BZ=F`[, "BZ=F.Close"]

# Create weekly data to reduce noise and improve model performance
brent_tsbl <- tibble(
  date = index(brent),
  price = as.numeric(brent)
) |>
  as_tsibble(index = date) |>
  filter(year(date) <= 2022) |>
  # Aggregate to weekly data
  index_by(week = tsibble::yearweek(date)) |>
  summarise(price = mean(price, na.rm = TRUE)) |>
  as_tsibble(index = week) |>
  fill_gaps() |>
  mutate(price = na_interpolation(price, option = "linear"))

# Test data similarly aggregated
test_data <- tibble(
  date = index(brent),
  price = as.numeric(brent)
) |>
  as_tsibble(index = date) |>
  filter(year(date) > 2022, year(date) <= 2024) |>
  index_by(week = yearweek(date)) |>
  summarise(price = mean(price, na.rm = TRUE)) |>
  as_tsibble(index = week)

# Explore multiple ARIMA models
fit_models <- brent_tsbl %>%
  model(
    auto_arima = ARIMA(price),                    # Fully automatic
    arima_111 = ARIMA(price ~ pdq(1,1,1)),        # Common financial model
    arima_210 = ARIMA(price ~ pdq(2,1,0)),        # AR(2) with differencing
    arima_012 = ARIMA(price ~ pdq(0,1,2)),        # MA(2) with differencing
    arima_212 = ARIMA(price ~ pdq(2,1,2)),        # ARMA(2,2) with differencing
  )

# Compare models
fit_models %>% glance() %>% arrange(AICc)

# Check the best model
best_fit <- fit_models %>% select(auto_arima) #auto arima has min AICc
report(best_fit)

# Check residuals
best_fit %>% gg_tsresiduals()

# Plot fitted values
best_fit %>%
  augment() %>%
  autoplot(.fitted, color = "red") +
  autolayer(brent_tsbl, price, color = "blue") +
  labs(title = "ARIMA Fitted vs Actual (Training Set)",
       y = "Price", x = "Time")

# Generate forecast
forecast_result <- best_fit %>% forecast(h = 104)  # 2 years of weeks

# Plot forecast with test data for comparison
autoplot(forecast_result, brent_tsbl) +
  autolayer(test_data, price, color = "darkgreen") +
  labs(title = "ARIMA Forecast vs Actual Test Data",
       y = "Price", x = "Time")
