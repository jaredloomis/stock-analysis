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
  .command("query-file-data-source <source> <ticker> <startTime> <fuzzinessMs>", "Query a data source")
    .string("source")
    .string("ticker")
    .string("startTime")
    .number("fuzzinessMs")
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
      const startTime = new Date(args.startTime).getTime() - args.fuzzinessMs;
      const endTime = startTime + args.fuzzinessMs*2;
      // Filter out values
      const filteredObject = sourceObject.filter(obj => {
        return  obj.quoteTicker == args.ticker &&
                new Date(obj.quoteTime).getTime() >= startTime &&
                new Date(obj.quoteTime).getTime() <= endTime
      })

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
