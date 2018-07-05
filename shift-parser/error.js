'use strict';

const chalk = require('chalk');

function errorReporter (title = "", path = "", message) {
  return [
    '',
    chalk`{red ✕ ${title}} {gray ${path}}`,
    '',
    chalk`  {red ●} ${message}`,
    '',
  ].join('\n');
}

module.exports.errorReporter = errorReporter;
