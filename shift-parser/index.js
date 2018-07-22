'use strict';

const fs = require('fs-extra');
const path = require('path');
const chalk = require('chalk');
const polyfill = require('./polyfill');
const {parseScriptWithLocation, parseModuleWithLocation} = require('shift-parser');
const {errorReporter} = require('./error');
const {
  formatParsingError,
  getTimestamp,
  TMP_DIR,
  INFRARED_TMP_DIR,
  writeToDebugFile
} = require('./utils');

polyfill.load();

/**
 * TODO
 * @param {string} absoluteFileName Absolute path to a file.
 * @param {string} fileName Relative path to a file.
 */
function processFile(absoluteFileName, fileName) {
  return new Promise((resolve, reject) => {
    fs.readFile(absoluteFileName, 'utf8')
      .then(fileString => {
        if (process.env.DEBUG) {
          writeToDebugFile(`${getTimestamp()} Parsing ${fileName}`);
        }
        try {
          const parsetree = parseModuleWithLocation(fileString);
          // we don't want to do this.
          // we want to persist the typed file, not the parsed file.
          // keeping this here for now, though
          createTmpFile(absoluteFileName, parsetree).then(tmpFile => resolve(tmpFile));
        } catch (parsingError) {
            reject(errorReporter('Parsing error', fileName, [
              chalk`{bold ${parsingError.description}} found at ${parsingError.line}:${parsingError.column}`,
              formatParsingError(fileString, parsingError.line, parsingError.column)
            ].join('\n')));
          }
        })
      .catch(loadingError => reject(errorReporter('File reading error', absoluteFileName, loadingError.message)));
  });
}

function createTmpFile (fileName, parsetree) {
  return new Promise((resolve, reject) => {
    const tmpFile = path.join(TMP_DIR, INFRARED_TMP_DIR, fileName.replace('.js', '.json'))
    if (process.env.DEBUG) {
      writeToDebugFile(`${getTimestamp()} Created ${tmpFile}`);
    }
    fs.outputJson(tmpFile, parsetree)
      .then(() => resolve(tmpFile))
      .catch(e => reject(errorReporter('Temp file creation error', tmpFile, e.message)));
  });
}

module.exports = processFile;
