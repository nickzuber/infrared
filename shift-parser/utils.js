'use strict';

const fs = require('fs-extra');
const path = require('path');
const chalk = require('chalk');
const moment = require('moment');
const {errorReporter} = require('./error');

const TMP_DIR = require('os').tmpdir();
const INFRARED_TMP_DIR = 'infrared-cache';
const DEBUG_FILE = 'infrared-debug.dump'

function writeToDebugFile (message) {
  const debugFile = path.join(TMP_DIR, INFRARED_TMP_DIR, DEBUG_FILE)
  fs.appendFile(debugFile, `${message}\n`)
    .then(() => {})
    .catch(e => errorReporter('Debug file writing error', debugFile, e.message))
}

function clearDebugFile () {
  const debugFile = path.join(TMP_DIR, INFRARED_TMP_DIR, DEBUG_FILE)
  fs.open(debugFile, `w+`)
    .then(() => {})
    .catch(e => errorReporter('Debug file clearing error', debugFile, e.message))
}

function formatParsingError (fileString, line, column) {
  const lines = fileString.split('\n');
  const topIndex = line - 3;
  const aboveIndex = line - 2;
  const currentIndex = line - 1;
  const belowIndex = line;
  const bottomIndex = line - 1;
  const numberWidth = (belowIndex + 1).toString().length;

  return [
    '',
    chalk`   {gray ${formatLine(lines[topIndex], topIndex + 1, numberWidth)}}`,
    chalk`   {gray ${formatLine(lines[aboveIndex], aboveIndex + 1, numberWidth)}}`,
    chalk` {red {bold >}} ${formatLine(lines[currentIndex], currentIndex + 1, numberWidth)}`,
    chalk`   {grey ${' '.padStart(numberWidth)} | }{red {bold ${'^'.padStart(column)}}}`,
    chalk`   {gray ${formatLine(lines[belowIndex], belowIndex + 1, numberWidth)}}`,
    chalk`   {gray ${formatLine(lines[bottomIndex], bottomIndex + 1, numberWidth)}}`,
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

function getTimestamp () {
  return `[${moment().format('YYYY-MM-DD')} ${moment().format('HH:MM:SS')}]\t`
}

function clearConsole () {
  if (process.env.DEBUG) return; // Don't clear console when debugging
  process.stdout.write(process.platform === 'win32' ? '\x1B[2J\x1B[0f' : '\x1B[2J\x1B[3J\x1B[H');
}

module.exports.writeToDebugFile = writeToDebugFile;
module.exports.clearDebugFile = clearDebugFile;
module.exports.formatParsingError = formatParsingError;
module.exports.formatLine = formatLine;
module.exports.replaceRange = replaceRange;
module.exports.getTimestamp = getTimestamp;
module.exports.clearConsole = clearConsole;
module.exports.TMP_DIR = TMP_DIR;
module.exports.INFRARED_TMP_DIR = INFRARED_TMP_DIR;
