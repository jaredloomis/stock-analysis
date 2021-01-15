import fetchYahooQuote from "./fetch-yahoo-quote";
import transform       from "./transformer";
import joinOn          from "./joiner";
const yargs = require('yargs');

const args = yargs
  .command('fetch-quote <tickers...>', 'Fetch a quote for each ticker')
    .string("tickers")
  .command('transform <source> <jqExpression>', 'Fetch data from a source, and transform it using jq')
    .string("source")
    .string("jqExpression")
  .command('join <sourceA> <sourceB> <joinColumn>', 'Fetch two data sources, and merge them on joinColumn')
    .string("sourceA")
    .string("sourceB")
    .string("joinColumn")
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
  case "join":
    joinOn(args.sourceA, args.sourceB, args.joinColumn).then(out => console.log(JSON.stringify(out)));
    break;
  default:
    console.error(`Unrecognized command: ${command}`);
    break;
}
