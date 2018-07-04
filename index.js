#!/usr/bin/env node
'use strict';

const fs = require('fs');
const { exec } = require('child_process');
const path = require('path');
const os = require('os');
const chalk = require('chalk');
const polyfill = require('./polyfill');
const { parseScriptWithLocation, parseModuleWithLocation } = require('shift-parser');

polyfill.load();

function loadFile(file) {
  const _ = fs.readFile(file, 'utf8', (loadingError, fileString) => {
    if (loadingError) {
      errorReporter('File error', loadingError.path, loadingError.message);
    }

    try {
      const parsetree = parseModuleWithLocation(fileString);
      createTmpFile(parsetree);

      console.log(chalk`✨  {green Looks good} {gray ${file}}`)

    } catch (parsingError) {
      errorReporter('Parsing error', file, [
        chalk`{bold ${parsingError.description}} found at ${parsingError.line}:${parsingError.column}`,
        chalk`${formatParsingError(fileString, parsingError.line, parsingError.column)}`
      ].join('\n'))
    }
  });
}

function createTmpFile (parsetree) {
  fs.mkdtemp(path.join(os.tmpdir(), 'infrared-'), (err, folder) => {
    if (err) {
      errorReporter('Tmp directory error', folder, err.message);
    }

    console.log(folder);

    exec(`ls ${folder}`, (err, stdout, stderr) => {
      if (err) throw err;

      console.log(`stdout: ${stdout}`);
      console.log(`stderr: ${stderr}`);
    })

    // Prints: /tmp/foo-itXde2 or C:\Users\...\AppData\Local\Temp\foo-itXde2
  });

  // tmp.dir({ template: 'tmp-XXXXXX' }, (err, path) => {
  //   if (err) throw err;

  //   console.log('Dir: ', path);
    // exec(`ls ${path}`, (err, stdout, stderr) => {
    //   if (err) throw err;

    //   console.log(`stdout: ${stdout}`);
    //   console.log(`stderr: ${stderr}`);
    // })
  // });
}

function formatParsingError (fileString, line, column) {
  const lines = fileString.split('\n');
  const above = line - 2;
  const current = line - 1;
  const below = line;
  const numberWidth = above.toString().length;

  return [
    '',
    chalk`{gray ${formatLine(lines[above], above + 1, numberWidth)}}`,
    chalk`${formatLine(lines[current], current + 1, numberWidth)}`,
    chalk`{grey ${' '.padStart(numberWidth)} | }{red {bold ${'^'.padStart(column)}}}`,
    chalk`{gray ${formatLine(lines[below], below + 1, numberWidth)}}`,
  ].join('\n');
}

function formatLine (line, number, padSize) {
  if (line === undefined) {
    return `${'*'.padStart(padSize)} |`;
  }
  return `${number.toString().padStart(padSize)} | ${line}`;
}

function replaceRange (str, start, end, sub) {
  return str.substring(0, start) + sub + str.substring(end);
}

function errorReporter (title = "", path = "", message) {
  console.log([
    '',
    chalk`{red ✕ ${title}} {gray ${path}}`,
    '',
    chalk`  {red ●} ${message}`,
    '',
  ].join('\n'));
  process.exit(1);
}

// @TEST
// Mock run
loadFile('./tests/experimental/index.js');
