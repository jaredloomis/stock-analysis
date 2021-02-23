"""
chart.py [TICKER]
Displays a chart of all available price data for ticker.
"""
import pandas as pd
import matplotlib.pyplot as plt
import psycopg2
import sys

ticker = sys.argv[1]
print("Ticker: " + ticker)

# Connect to an existing database
conn = psycopg2.connect("dbname=stock_market_indicators user=postgres")

# Open a cursor to perform database operations
cur = conn.cursor()

# Fetch all samples
cur.execute(
  """
  SELECT indicator_value, start_time FROM IndicatorSample
  WHERE stock_id = %s AND indicator_id = %s
  ORDER BY start_time
  """,
  (ticker, "price")
)
samples_df = pd.DataFrame(cur.fetchall())

# Create x, y axes
prices = samples_df.loc[:, 0].astype(float)
dates = pd.to_datetime(samples_df.loc[:, 1])

# Show plot
plt.scatter(dates, prices)
plt.show()
