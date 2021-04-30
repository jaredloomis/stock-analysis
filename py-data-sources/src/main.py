import os
import json
import argparse
from secedgar.filings import Filing, FilingType
from parse_xbrl import parse_xbrl

parser = argparse.ArgumentParser(description="Fetch SEC filings data.")
parser.add_argument("tickers", type=str, nargs="+", help="Stock tickers")
parser.add_argument("--start-date", default="20200601",
                    help="Earliest filing date to look for documents. Format: YYYYMMDD.")
parser.add_argument("--filing-type", default='10Q',
                    help="Filing Type. Options: 10Q, 4")
args = parser.parse_args()
top_level_dir = "../data/SEC-EDGAR-filings"

def filing_type_from_str(filing_type_str):
  if filing_type_str == '10Q':
    return FilingType.FILING_10Q
  elif filing_type_str == '4':
    return FilingType.FILING_4

for ticker in args.tickers:
    filing_type = filing_type_from_str(args.filing_type)
    my_filings = Filing(cik_lookup=ticker, filing_type=filing_type, start_date=args.start_date)
    my_filings.save(top_level_dir)

    report_dir = top_level_dir + "/" + ticker + "/" + args.filing_type
    for file_name in os.listdir(report_dir):
        if file_name.endswith(".txt"):
            file_path = report_dir + "/" + file_name
            print(json.dumps(parse_xbrl(file_path, filing_type)))
