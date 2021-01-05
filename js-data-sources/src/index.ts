import fetchYahooQuote from "./fetch-yahoo-quote";
import transform       from "./transformer";
const yargs = require('yargs');

const args = yargs
  .command('fetch-quote <tickers...>', 'Fetch a quote for each ticker')
    .string("tickers")
  .command('transform <source> <jqExpression>', 'Fetch data from a source, and transform it using jq')
    .string("source")
    .string("jqExpression")
  .help()
  .argv;

//console.log(args)

const command = args._[0];
switch(command) {
  case "fetch-quote":
    fetchYahooQuote(args.tickers.flatMap(ticker => ticker.split(',')));
    break;
  case "transform":
    transform(args.source, args.jqExpression).then(out => console.log(JSON.stringify(out)));
    break;
  default:
    console.error(`Unrecognized command: ${command}`);
    break;
}
