'use strict';

const path = require('path');
const {exec} = require('child_process');

const INFRARED_CORE_DIR = 'infrared-core';
const INFRARED_COMMAND = 'check-cached';
const INFRARED_SHELL = 'infrared.native';

function execInfraredCore (files) {
  const files_as_args = files.join(' ');
  exec(`${path.join(INFRARED_CORE_DIR, INFRARED_SHELL)} ${INFRARED_COMMAND} ${files_as_args}`, (execError, stdout, stderr) => {
    if (execError) {
      errorReporter('Infared Handoff Error', tmpFile, execError.message);
    }
    console.log('ANALYSIS\n');
    console.log(stdout);
  });
}

module.exports = execInfraredCore;
