'use strict';

const path = require('path');
const chalk = require('chalk');
const {getTimestamp, clearConsole, writeToDebugFile} = require('../shift-parser/utils');
const parseAndPersist = require('../shift-parser');

function processFiles (files) {
  return new Promise((resolve, reject) => {
    if (process.env.DEBUG) {
      process.env.INFRARED_TIMER = +Date.now();
      writeToDebugFile(`${getTimestamp()} Starting file processing`)
    }

    // absolute file path, friendly file path
    const promises = files.map(file => parseAndPersist(file, file));

    clearConsole();
    console.log(chalk`{bold \n Starting file processing...\n}`);

    Promise.all(promises).then(files => {
      resolve(files)
    }).catch(error => {
      reject(error)
    });
  })
}

module.exports = processFiles;
