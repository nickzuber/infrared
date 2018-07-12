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

// @TEST
// Mock run
clearConsole();
clearDebugFile();

const files = [];
files.push('./tests/experimental/a.js');
files.push('./tests/experimental/b.js');

processFiles(files, __dirname)
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
