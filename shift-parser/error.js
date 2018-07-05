'use strict';

const chalk = require('chalk');

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

module.exports.errorReporter = errorReporter;
