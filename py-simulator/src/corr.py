import psycopg2
import scipy.stats
import numpy as np
import matplotlib.pyplot as plt


def create_price_correlation_sample__pearson(cur, ticker_a, ticker_b, timerange_a, timerange_b):
    """
    Create a sample representing the correlation between two tickers, given a timerange for each ticker.
    :return: (correlation r value, std deviation, # of samples)
    """

    indicator_id = "correlation_price-pearson"

    # Fetch samples
    samples_a = fetch_all_samples(cur, "price", ticker_a, timerange_a)
    samples_b = fetch_all_samples(cur, "price", ticker_b, timerange_b)

    if samples_a.shape[0] < 7 or samples_b.shape[0] < 7:
        print("SKIPPING", ticker_a, ticker_b)
        return 0, 0

    """
    try:
        samples_a, samples_b = fetch_paired_samples(cur, "price", ticker_a, ticker_b, timerange_a)
    except:
        return 0, 0, 0
    """

    # Transform into arrays of just price.
    # NB This assumes that the two tickers have samples at the same frequency.
    # TODO Ensure the two samples are at the same frequency - Fill in missing samples, drop samples, etc.
    try:
        prices_a = samples_a[:, 1].astype(np.float)
        prices_b = samples_b[:, 1].astype(np.float)
    except IndexError:
        print(samples_a)
        print(samples_b.shape)
    # Drop off extra data so both arrays have the same length
    min_price_len = min(prices_a.shape[0], prices_b.shape[0])
    prices_a = prices_a[:min_price_len]
    prices_b = prices_b[:min_price_len]

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


def plot_offset_prices(cur, indicator_id, ticker_a, timerange_a, ticker_b, timerange_b):
    sample_rows_a = fetch_all_samples(cur, indicator_id, ticker_a, timerange_a)
    samples_a = sample_rows_a[:, 1].astype(np.float)
    sample_rows_b = fetch_all_samples(cur, indicator_id, ticker_b, timerange_b)
    samples_b = sample_rows_b[:, 1].astype(np.float)

    fig, axs = plt.subplots(2, 1)
    axs[0].plot(samples_a)
    axs[0].set_title(ticker_a + " - " + timerange_a[0] + " to " + timerange_a[1])
    axs[1].plot(samples_b)
    axs[1].set_title(ticker_b + " - " + timerange_b[0] + " to " + timerange_b[1])
    plt.tight_layout()
    plt.show()


def offset_price_correlation_matrix(indicator, timerange_a, timerange_b):
    # TODO return a value (numpy matrix? / DataFrame)
    # Loop through every stock combo, print their corr
    all_stocks = list_stocks(cur)
    np.random.shuffle(all_stocks)
    for stock_a in all_stocks:
        if not enough_samples_exist(cur, indicator, stock_a, timerange_a):
            continue
        for stock_b in all_stocks:
            if stock_a == stock_b:
                continue

            corr = create_price_correlation_sample__pearson(
                cur, stock_a, stock_b, timerange_a, timerange_b
            )

            if corr[0] == 0 or np.isnan(corr[0]):
                continue

            print("Correlation between " + stock_a + " and " + stock_b + ": " + str(corr))
            print("(During " + str(timerange_a) + " and " + str(timerange_b) + ")")

            #if abs(corr[0]) >= 0.7:
            #    plot_offset_prices(cur, indicator, stock_a, timerange_a, stock_b, timerange_b)


def insert_price_correlation_sample(cur, sample):
    cur.execute(
        """
        INSERT INTO IndicatorSample (indicator_id, )
        VALUES ()
        """
    )


# Connect to an existing database
conn = psycopg2.connect("dbname=stock_market_indicators user=postgres")

# Open a cursor to perform database operations
cur = conn.cursor()

timerange_a = "2018-04-01", "2018-06-01"
timerange_b = "2018-05-01", "2018-07-01"
offset_price_correlation_matrix("price", timerange_a, timerange_b)
#plot_offset_prices(cur, "price", "OMC", ("2018-04-01", "2018-06-01"), "SJM", ("2018-05-01", "2018-07-01"))

# Make the changes to the database persistent
# conn.commit()

# Close communication with the database
cur.close()
conn.close()
