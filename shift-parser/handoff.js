'use strict';

const path = require('path');
const chalk = require('chalk');
const {getTimestamp, clearConsole, writeToDebugFile} = require('./utils');
const {exec} = require('child_process');

const INFRARED_CORE_DIR = 'infrared-core';
const INFRARED_COMMAND = 'check-cached';
const INFRARED_SHELL = 'infrared.native';

const wordsOfEncouragment = [
  'Looks great!',
  'All good on our end!',
  'Dang, that\'s some nice looking code.',
  'Beautiful, I\'m proud of you.',
  'Wonderful!',
];
const rand = arr => arr[Math.floor(Math.random() * arr.length)]

function execInfraredCore (files) {
  const files_as_args = files.join(' ');

  if (process.env.DEBUG) {
    writeToDebugFile(`${getTimestamp()} Running ${path.join(INFRARED_CORE_DIR, INFRARED_SHELL)} ${INFRARED_COMMAND} ${files_as_args}`);
  }

  exec(`${path.join(INFRARED_CORE_DIR, INFRARED_SHELL)} ${INFRARED_COMMAND} ${files_as_args}`, (execError, stdout, stderr) => {
    if (execError) {
      errorReporter('Infared Handoff Error', files, execError.message);
    }
    clearConsole();
    console.log(stdout);
    console.log(chalk`{green ✨ ${rand(wordsOfEncouragment)}}\n`);
    if (process.env.DEBUG) {
      writeToDebugFile(`${getTimestamp()} Done`);
      writeToDebugFile(`${getTimestamp()} Took ${(+Date.now() - process.env.INFRARED_TIMER) / 1000} seconds to run`);
    }
  });
}

module.exports = execInfraredCore;
