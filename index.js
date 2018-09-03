'use strict';

const path = require('path');
const chalk = require('chalk');
const execInfraredCore = require('./shift-parser/handoff')
const {
  getTimestamp,
  clearConsole,
  clearDebugFile,
  writeToDebugFile,
} = require('./shift-parser/utils');
const processFiles = require('./file-processor');

const files = [];
// files.push(path.resolve(__dirname, './tests/experimental/a.js'));
// files.push(path.resolve(__dirname, './tests/experimental/b.js'));

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
      process.stdout.write(chalk.bold('\n Process stopped.\n\n'));
      process.exit(1);
    });
}

// typecheckFiles(files);

module.exports.typecheckFiles = typecheckFiles;
