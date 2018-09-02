'use strict';

const path = require('path');
const chalk = require('chalk');
const {getTimestamp, clearConsole, writeToDebugFile} = require('./utils');
const {errorReporter} = require('./error');
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

  const PATH_TO_CORE =  path.resolve(__dirname, '..', INFRARED_CORE_DIR, INFRARED_SHELL);

  exec(`${PATH_TO_CORE} ${INFRARED_COMMAND} ${files_as_args}`, (execError, stdout, stderr) => {
    if (execError) {
      console.log('smething went wrong')
      const message = errorReporter('Infared Handoff Error', files.join(''), execError.message);
      throw new Error(message);
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
