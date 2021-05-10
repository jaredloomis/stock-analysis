import fetchYahooQuote    from "./fetch-yahoo-quote";
import transform          from "./transformer";
import joinOn             from "./joiner";
import fetchInsiderTrades from "./fetch-insider-trades-openinsider";
import loadSource, { DataSource, parseSourceStr } from "./loader";
const yargs = require('yargs');

const args = yargs
  .command('fetch-quote <tickers...>', 'Fetch a quote for each ticker')
    .string("tickers")
  .command('fetch-insider', 'Fetch insider trades')
    .string("tickers")
  .command('transform <source> <jqExpression>', 'Fetch data from a source, and transform it using jq')
    .string("source")
    .string("jqExpression")
  .command("query-file-data-source <source> <ticker> <startTime> <endTime>", "Query a data source")
    .string("source")
    .string("ticker")
    .string("startTime")
    .string("endTime")
  .command('join <sourceA> <sourceB> <joinColumn>', 'Fetch two data sources, and merge them on joinColumn')
    .string("sourceA")
    .string("sourceB")
    .string("joinColumn")
  .help()
  .argv;

const command = args._[0];
switch(command) {
  case "fetch-quote":
    fetchYahooQuote(args.tickers.flatMap(ticker => ticker.split(',')));
    break;
  case "fetch-insider-trades":
    fetchInsiderTrades();
    break;
  case "query-file-data-source":
    (async function() {
      let { source } = args;
      // Normalize source string
      if(typeof(source) === "string") {
        source = parseSourceStr(source);
      }
      // Load source
      const sourceObject = await loadSource(source);
      // Filter out values
      const filteredObject = sourceObject.filter(obj =>
        obj.quoteTicker == args.ticker &&
        new Date(obj.quoteTime).getTime() >= new Date(args.startTime).getTime() &&
        new Date(obj.quoteTime).getTime() <= new Date(args.endTime).getTime()
      )

      console.log(JSON.stringify(filteredObject));
    })();
    break;
  case "transform":
    transform(args.source, args.jqExpression)
    .then(out => console.log(JSON.stringify(out)));
    break;
  case "join":
    joinOn(args.sourceA, args.sourceB, args.joinColumn).then(out => console.log(JSON.stringify(out)));
    break;
  default:
    console.error(`Unrecognized command: ${command}`);
    break;
}

function getMonthFromString(mon){
   const num = new Date(Date.parse(mon +" 1, 2012")).getMonth()+1;
   return num < 10 ? `0${num}` : num;
}