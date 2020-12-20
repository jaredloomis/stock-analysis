mod strategy;

extern crate clap;
extern crate serde_json;

use std::process::{Command, exit, Output};
use serde::{Deserialize, Serialize};
use std::path::Path;
use clap::{Arg, App, SubCommand};
use Error::JsonError;

fn main() {
    let matches = App::new("Stonks simulator")
        .version("0.1.0")
        .author("Jared Loomis <jaredloomis@protonmail.com>")
        .about("Backtest trading strategies")
        .arg(Arg::with_name("strategy")
            .required(true)
            .short("s")
            .long("strategy")
            .value_name("STRATEGY")
            .help("The strategy to simulate")
            .takes_value(true))
        .get_matches();

    let mstrategy = matches.value_of("strategy");

    if mstrategy.is_some() {
        let strategy = mstrategy.unwrap();
        example(strategy)
    }
}

#[derive(Deserialize, Serialize, Debug)]
struct Config<'a> {
    dataLoader: &'a Path
}

/**
 * For this example, we
 */
fn get_data_load_command(strategy: &str) -> Result<Command, Error> {
    let res: Result<Box<Config>, serde_json::Error> = serde_json::from_str("./config/simulator-config.json");
    // TODO parse strategy string
    // TODO create list of all indicators referenced
    // TODO create list of all queries needed
    return if res.is_ok() {
        let config = res.unwrap();
        Command::new(&config.dataLoader)
            .map_err(|e| Error::IOError(e))
    } else {
        Result::Err(Error::JsonError(res.err().unwrap()))
    }
}

enum Error {
    JsonError(serde_json::Error),
    IOError(std::io::Error),
    StringError(String)
}
