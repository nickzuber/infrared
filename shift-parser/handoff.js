'use strict';

const path = require('path');
const moment = require('moment');
const {timestamp} = require('./utils');
const {exec} = require('child_process');

const INFRARED_CORE_DIR = 'infrared-core';
const INFRARED_COMMAND = 'check-cached';
const INFRARED_SHELL = 'infrared.native';

function execInfraredCore (files) {
  const files_as_args = files.join(' ');

  if (process.env.DEBUG) {
    console.log(`${timestamp()} Running ${path.join(INFRARED_CORE_DIR, INFRARED_SHELL)} ${INFRARED_COMMAND} ${files_as_args}\n`);
  }

  exec(`${path.join(INFRARED_CORE_DIR, INFRARED_SHELL)} ${INFRARED_COMMAND} ${files_as_args}`, (execError, stdout, stderr) => {
    if (execError) {
      errorReporter('Infared Handoff Error', files, execError.message);
    }
    console.log(stdout);
    if (process.env.DEBUG) {
      console.log(`${timestamp()} Done`);
      console.log(`${timestamp()} Took ${(+Date.now() - process.env.INFRARED_TIMER) / 1000} seconds to run`);
    }
  });
}

module.exports = execInfraredCore;
