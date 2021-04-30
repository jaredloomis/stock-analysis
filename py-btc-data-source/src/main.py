import json
from exchanges.bitfinex import Bitfinex

price = Bitfinex().get_current_price()

samples = [{
  "quoteTicker": "BTC",
  "quoteValue": float(price),
  "quoteTime": "",
  "quoteApiId": "btc_py",
  "quoteRaw": {}
}]

print(json.dumps(samples))

