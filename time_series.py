import pandas as pd
import matplotlib.pyplot as plt
import yfinance as yf

# Download historical daily data for the last 5 years
data = yf.download("AAPL", period="5y", interval="1d")

# Plot the time series
plt.plot(data.index, data['Close'], marker='x', linestyle='-', color='blue')
plt.title('Daily Applr Stock Prices')
plt.xlabel('Days')
plt.ylabel('Close Price (USD)')
plt.grid(True)
plt.show()