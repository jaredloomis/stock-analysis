var Crawler = require("crawler");

const yargs = require('yargs');

let { tickers } = yargs.command('fetch-quote <tickers...>', 'Fetch a quote for each ticker')
  .help()
  .argv;
// Tickers can be provided as 'AAPL AMD ...' or 'AAPL,AMD,...'.
tickers = tickers.flatMap(ticker => ticker.split(','));

const totalRequestCount = tickers.length;
let accumRequestCount = 0;
const results = [];

function buildQuote(ticker, time, value, apiID, raw) {
  return {
    quoteTicker: ticker,
    quoteTime: time,
    quoteValue: value,
    quoteApiId: apiID,
    quoteRaw: raw
  };
}

function parseNumber(num) {
  if(typeof num === 'string') {
    return parseFloat(num.replace(',', ''))
  }
  return num;
}

const flows = [
  {
    name: "Yahoo Stock Quotes",
    url: /https:\/\/finance.yahoo.com\/quote\/(.+)\/?/,
    handler: async (error, res) => {
      const { $, options } = res;
      const value = parseNumber($('#quote-header-info .Fz\\(36px\\)').text());
      const raw = {
        url: res.request.uri.href,
        //body: $('body').text()
      };

      results.push(buildQuote(options.ticker, new Date(), value, "Yahoo.com", raw));
    }
  }
];

async function flowHandler(error, res) {
  const url = res.request.uri.href;
  flows
    .filter(flow => flow.url.test(url))
    .forEach(flow => flow.handler(error, res));
}

/**
 * Basic config applied to all crawlers.
 */
var c = new Crawler({
  maxConnections : 10,
  callback : function (error, res, done) {
    // General handling
    if (error) {
      console.log(error);
    }
    ++accumRequestCount;

    // Site/page-specific handling
    flowHandler(error, res).then(done);

    if(accumRequestCount === totalRequestCount) {
      console.log(JSON.stringify(results));
    }
  }
});

tickers.forEach(ticker =>
  c.queue({
    url: `https://finance.yahoo.com/quote/${ticker}`,
    ticker: ticker
  })
);
