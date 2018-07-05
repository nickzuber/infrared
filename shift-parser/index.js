'use strict';

const fs = require('fs-extra');
const path = require('path');
const chalk = require('chalk');
const polyfill = require('./polyfill');
const {parseScriptWithLocation, parseModuleWithLocation} = require('shift-parser');
const {errorReporter} = require('./error');
const {formatParsingError, timestamp} = require('./utils');

const TEMP_DIR = require('os').tmpdir();
const INFRARED_TMP_DIR = 'infrared-cache';

polyfill.load();

/**
 * TODO
 * @param {string} fileName Absolute path to a file.
 */
function processFile(fileName) {
  return new Promise((resolve, reject) => {
    fs.readFile(fileName, 'utf8')
      .then(fileString => {
        if (process.env.DEBUG) {
          console.log(`${timestamp()} Parsing ${fileName}`);
        }
        try {
          const parsetree = parseModuleWithLocation(fileString);
          createTmpFile(fileName, parsetree).then(tmpFile => resolve(tmpFile));
        } catch (parsingError) {
            reject(errorReporter('Parsing Error', fileName, [
              chalk`{bold ${parsingError.description}} found at ${parsingError.line}:${parsingError.column}`,
              formatParsingError(fileString, parsingError.line, parsingError.column)
            ].join('\n')));
          }
        })
      .catch(loadingError => reject(errorReporter('File Reading Error', loadingError.path, loadingError.message)));
  });
}

function createTmpFile (fileName, parsetree) {
  return new Promise((resolve, reject) => {
    const tmpFile = path.join(TEMP_DIR, INFRARED_TMP_DIR, fileName.replace('.js', '.json'))
    if (process.env.DEBUG) {
      console.log(`${timestamp()} Created ${tmpFile}`);
    }
    fs.outputJson(tmpFile, parsetree)
      .then(() => resolve(tmpFile))
      .catch(e => reject(errorReporter('Temp File Creation Error', tmpFile, e.message)));
  });
}

module.exports = processFile;
