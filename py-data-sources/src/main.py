import os
import argparse
from secedgar.filings import Filing, FilingType
from parse_xbrl import parse_xbrl

parser = argparse.ArgumentParser(description="Fetch SEC filings data.")
parser.add_argument("tickers", type=str, nargs="+", help="Stock tickers")
parser.add_argument("--start-date", default="20200601",
                    help="Earliest filing date to look for documents. Format: YYYYMMDD.")
args = parser.parse_args()
top_level_dir = "../data/SEC-EDGAR-filings"

for ticker in args.tickers:
    my_filings = Filing(cik_lookup=ticker, filing_type=FilingType.FILING_4, start_date=args.start_date)
    my_filings.save(top_level_dir)

    report_dir = top_level_dir + "/" + ticker + "/4"
    for file_name in os.listdir(report_dir):
        if file_name.endswith(".txt"):
            file_path = report_dir + "/" + file_name
            print(parse_xbrl(file_path))
