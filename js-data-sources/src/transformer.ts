/**
 * This module provides the ability to load data from a variety of sources, in a variety of format, and
 * to transform it into custom user-defined JSON formats.
 */
import fs from "fs";
import util from "util";
import neatCsv from "neat-csv";
import csv from "csv-parser";
import { spawn } from "child_process";
import { readFile, PathLike } from "fs";
import axios from "axios";

import loadSource, { DataSource, parseSourceStr } from "./loader";

export default async function transformData(source: DataSource | string, jqExpression: string): Promise<object> {
  // Normalize source string
  if(typeof(source) === "string") {
    source = parseSourceStr(source);
  }

  // Load source
  const sourceObject = await loadSource(source);

  // Spawn jq
  const child = spawn("jq", [jqExpression]);

  // Clone stderr
  child.stderr.pipe(process.stderr);

  // Send source contents to input
  child.stdin.write(JSON.stringify(sourceObject) + "\n\r\0");
  child.stdin.end();

  // Await output
  let output = "";
  return new Promise((resolve, reject) => {
    child.stdout.on('data', chunk => {
      output += chunk;
    });
    child.stdout.on('end', () => {
      resolve(JSON.parse(output));
    });
  });
}
