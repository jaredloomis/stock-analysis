const { Duration } = require("luxon");

const args   = process.argv.slice(2);
const number = parseFloat(args[0]);
const unit   = args[1];

console.log(Duration.fromObject({ [unit]: number }).as("milliseconds"));
