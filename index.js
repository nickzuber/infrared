'use strict';

const chalk = require('chalk');
const execInfraredCore = require('./shift-parser/handoff');
const { errorReporter } = require('./shift-parser/error');
const {
  getTimestamp,
  clearConsole,
  clearDebugFile,
  writeToDebugFile,
} = require('./shift-parser/utils');
const processFiles = require('./file-processor');

function typecheckFiles (files) {
  clearConsole();
  clearDebugFile();
  processFiles(files)
    .then(files => {
      if (process.env.DEBUG) {
        writeToDebugFile(`${getTimestamp()} Finished file processing`);
      }
      execInfraredCore(files);
    })
    .catch(error => {
      clearConsole();
      process.stdout.write(error);
      process.stdout.write(chalk.bold('\nProcess stopped.\n\n'));
      process.exit(1);
    });
}

module.exports.typecheckFiles = typecheckFiles;
module.exports.errorReporter = errorReporter;
module.exports.clearConsole = clearConsole;
