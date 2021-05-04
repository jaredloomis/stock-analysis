import json
import datetime
from exchanges.bitfinex import Bitfinex

price = float(Bitfinex().get_current_price())

samples = [{
  "quoteTicker": "BTC",
  "quoteValue": price,
  "quoteTime": datetime.datetime.utcnow().replace(tzinfo=datetime.timezone.utc).isoformat(),
  "quoteApiId": "btc_py-bitfinex",
  "quoteRaw": {
    "price": price
  }
}]

print(json.dumps(samples))
