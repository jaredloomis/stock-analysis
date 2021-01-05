import psycopg2
import scipy.stats
import numpy as np


def create_price_correlation_sample__pearson(cur, ticker_a, ticker_b, timerange_a, timerange_b):
    """
    Create a sample representing the correlation between two tickers, given a timerange for each ticker.
    """

    indicator_id = "correlation_price-pearson"

    # Fetch samples
    #samples_a = fetch_all_samples(cur, "price", ticker_a, timerange_a)
    #samples_b = fetch_all_samples(cur, "price", ticker_b, timerange_b)

    try:
        samples_a, samples_b = fetch_paired_samples(cur, "price", ticker_a, ticker_b, timerange_a)
    except:
        return 0, 0, 0

    # Transform into arrays of just price.
    # NB This assumes that the two tickers have samples at the same frequency.
    # TODO Ensure the two samples are at the same frequency - Fill in missing samples, drop samples, etc.
    prices_a = samples_a[:, 1].astype(np.float)
    prices_b = samples_b[:, 1].astype(np.float)

    try:
        ret = scipy.stats.pearsonr(prices_a, prices_b)
        return ret[0], ret[1], samples_a.shape[0]
    except:
        return 0, 0, 0


def fetch_paired_samples(cur, indicator, ticker_a, ticker_b, timerange):
    cur.execute(
        """
        SELECT a.stock_id stock_id_a, a.indicator_value indicator_value_a, b.stock_id stock_id_b, b.indicator_value indicator_value_b, a.indicator_id indicator_id, a.start_time start_time
        FROM indicatorsample a, indicatorsample b
        WHERE a.stock_id = %s AND b.stock_id = %s
          AND a.start_time = b.start_time AND a.end_time = b.end_time AND a.indicator_id = b.indicator_id
          AND a.start_time > %s AND a.end_time < %s
          AND a.indicator_id = %s
        ORDER BY a.stock_id, b.stock_id, a.start_time;
        """,
        (ticker_a, ticker_b, timerange[0], timerange[1], indicator)
    )
    all_samples = np.array(cur.fetchall())
    return np.array(all_samples[:, :2]), np.array(all_samples[:, 2:4])


def fetch_all_samples(cur, indicator, ticker, timerange):
    cur.execute(
        """
        SELECT stock_id, indicator_value, start_time, end_time FROM IndicatorSample
        WHERE stock_id = %s AND start_time > %s AND end_time < %s AND indicator_id = %s
        ORDER BY start_time
        """,
        (ticker, timerange[0], timerange[1], indicator)
    )
    return np.array(cur.fetchall())


def enough_samples_exist(cur, indicator, ticker, timerange):
    cur.execute(
        """
        SELECT COUNT(*)
        FROM indicatorsample
        WHERE stock_id = %s
          AND indicator_id = %s
          AND start_time > %s AND end_time < %s
        """,
        (ticker, indicator, timerange[0], timerange[1])
    )
    count = cur.fetchone()[0]
    return count > 7


def find_timeoffset_correlation(cur):
    cur.execute("SELECT * FROM IndicatorSample WHERE ")


def list_stocks(cur):
    cur.execute(
        """
        SELECT DISTINCT stock_id FROM indicatorsample
        """
    )
    xs = np.array(cur.fetchall())
    return xs.reshape((xs.shape[0],))


# Connect to an existing database
conn = psycopg2.connect("dbname=stock_market_indicators user=postgres")

# Open a cursor to perform database operations
cur = conn.cursor()

# Loop through every stock combo, print their corr
timerange = "1992-06-04", "2021-06-04"
indicator = "price"
all_stocks = list_stocks(cur)[7:]
for stock_a in all_stocks:
    if not enough_samples_exist(cur, indicator, stock_a, timerange):
        print("Not enough samples for " + stock_a)
        continue
    for stock_b in all_stocks:
        if stock_a != stock_b:
            if not enough_samples_exist(cur, indicator, stock_b, timerange):
                print("Not enough samples for " + stock_b)
                continue

            corr = create_price_correlation_sample__pearson(
                cur, stock_a, stock_b, timerange, timerange
            )
            if corr[0] != 0 and not np.isnan(corr[0]):
                print("Correlation between " + stock_a + " and " + stock_b + ": " + str(corr))

# Make the changes to the database persistent
# conn.commit()

# Close communication with the database
cur.close()
conn.close()
