'use strict';

const path = require('path');
const chalk = require('chalk');
const {timestamp, clearConsole} = require('./shift-parser/utils');
const processFile = require('./shift-parser');
const execInfraredCore = require('./shift-parser/handoff');

// @TEST
// Mock run
clearConsole();

if (process.env.DEBUG) {
  process.env.INFRARED_TIMER = +Date.now();
  console.log(`${timestamp()} Starting file processing`)
}

let promises = [];
promises.push(processFile(path.join(__dirname, './tests/experimental/index.js'), './tests/experimental/index.js'));
promises.push(processFile(path.join(__dirname, './tests/experimental/a.js'), './tests/experimental/a.js'));

Promise.all(promises).then(files => {
  if (process.env.DEBUG) {
    console.log(`${timestamp()} Finished file processing`);
  }
  execInfraredCore(files);
}).catch(error => {
  process.stdout.write(error);
  process.stdout.write(chalk.bold('\n Process stopped.\n\n'));
  process.exit(1);
});
