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

const readFileAsync = util.promisify(readFile)

type DataSource = FileSource | StringSource | HTTPGetSource;
type FileSource = {
  filePath: PathLike
};
type StringSource = {
  string: string
};
type HTTPGetSource = {
  url: string
};

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

async function loadSource(source: DataSource): Promise<object> {
  const anySource = source as any;
  if(anySource.filePath) {
    const path = anySource.filePath
    if(path.endsWith(".json")) {
      const text = await readFileAsync(path, { encoding: "utf8" });
      return JSON.parse(text);
    } else if(path.endsWith(".csv")) {
      return new Promise((resolve, reject) => {
        const results: any[] = [];
        fs.createReadStream(path)
          .pipe(csv())
          .on("data", data => results.push(data))
          .on("end", () => resolve(results))
          .on("error", reject);
      });
      //return neatCsv(text);
    }
  } else if(anySource.string) {
    if(isJSON(anySource.string)) {
      return JSON.parse(anySource.string);
    } else if(isCSV(anySource.string)) {
      return neatCsv(anySource.string);
    }
  } else if(anySource.url) {
    const response = await axios.get(anySource.url);
    return response.data;
  }
  throw `Unrecognized data source: ${source}`
}

function parseSourceStr(str: string | object): DataSource {
  if(typeof(str) === "object") {
    return { string: JSON.stringify(str) };
  } else if(isUrl(str)) {
    return { url: str };
  } else if(isFilePath(str)) {
    return { filePath: str };
  } else {
    return { string: str };
  }
}

function isUrl(str: string): boolean {
  return str.startsWith("http");
}

function isFilePath(str: string): boolean {
  return /[^\.]+\.\w{1,6}$/.test(str);
}

function isJSON(str: string): boolean {
  try {
    JSON.parse(str);
    return true;
  } catch(ex) {
    return false;
  }
}

function isCSV(str: string): boolean {
  // If text has a comma, on the first line, call it CSV
  return str.split("\n")[0].indexOf(',') !== -1;
}
