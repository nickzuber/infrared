'use strict';

const chalk = require('chalk');
const moment = require('moment');

function formatParsingError (fileString, line, column) {
  const lines = fileString.split('\n');
  const aboveIndex = line - 2;
  const currentIndex = line - 1;
  const belowIndex = line;
  const numberWidth = (belowIndex + 1).toString().length;

  return [
    '',
    chalk`{gray ${formatLine(lines[aboveIndex], aboveIndex + 1, numberWidth)}}`,
    chalk`${formatLine(lines[currentIndex], currentIndex + 1, numberWidth)}`,
    chalk`{grey ${' '.padStart(numberWidth)} | }{red {bold ${'^'.padStart(column)}}}`,
    chalk`{gray ${formatLine(lines[belowIndex], belowIndex + 1, numberWidth)}}`,
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

function timestamp () {
  return `[${moment().format('YYYY-MM-DD')} ${moment().format('HH:MM:SS')}]`
}

module.exports.formatParsingError = formatParsingError;
module.exports.formatLine = formatLine;
module.exports.replaceRange = replaceRange;
module.exports.timestamp = timestamp;
