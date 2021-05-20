

import urllib.request, json 
with urllib.request.urlopen("https://api.coindesk.com/v1/bpi/historical/close.json?start=2013-09-01&end=2013-09-05") as url:
    data = json.loads(url.read().decode())
    print(data)