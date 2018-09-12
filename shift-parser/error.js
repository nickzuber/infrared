'use strict';

const chalk = require('chalk');
const sep = require('path').sep;

function errorReporter (title = "", path = "", message) {
  let highlightedPath = path;

  let routes = path.split(sep);
  if (routes.length > 1) {
    let end = routes.pop();
    highlightedPath = chalk`${routes.join(sep)}${sep}{white {bold ${end}}}`;
  }

  return [
    '',
    // chalk` {bgRed {bold  ${title.toUpperCase()} }} {gray ${path}}`,
    chalk`{red Failed to process.}`,
    '',
    chalk`{bgRed {bold  ${title.toUpperCase()} }} {gray ${highlightedPath}}`,
    '',
    chalk`  {red ●} ${message}`,
    '',
  ].join('\n');
}

module.exports.errorReporter = errorReporter;
