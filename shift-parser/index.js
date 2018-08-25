'use strict';

const fs = require('fs-extra');
const path = require('path');
const chalk = require('chalk');
const polyfill = require('./polyfill');
const {parseModuleWithLocation} = require('shift-parser');
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
 * Given a node, adds its respective location information to the object if
 * such information exists. Does the same for all children.
 *
 * @param {JSON} node Current json node.
 * @param {WeakMap} locationWeakMap Map of node to location.
 */
function addLocations (node, locationWeakMap) {
  if (typeof node !== 'object') {
    return node;
  }

  const location = locationWeakMap.get(node);

  // If there's a location for this node, decorate it.
  if (location) {
    node = {
      ...node,
      location
    }
  }

  // Decorate all children nodes.
  Object.keys(node).forEach(childKey => {
    const newChild = addLocations(node[childKey], locationWeakMap);
    node[childKey] = newChild;
  });

  return node;
}

/**
 * Parses a javascript file and writes the resulting AST to a tmp file.
 *
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
          const {
            tree,
            locations: locationWeakMap
          } = parseModuleWithLocation(fileString);

          const treeWithLocations = addLocations(tree, locationWeakMap);

          // Parse and cache the files so infrared-core can pick it up
          // That is the only purpose of this right now.
          createTmpFile(absoluteFileName, treeWithLocations).then(tmpFile => resolve(tmpFile));
        } catch (parsingError) {
            console.log(parsingError)
            reject(errorReporter('Parsing error', fileName, [
              chalk`{bold ${parsingError.description}} found at ${parsingError.line}:${parsingError.column}`,
              formatParsingError(fileString, parsingError.line, parsingError.column)
            ].join('\n')));
          }
        })
      .catch(loadingError => reject(errorReporter('File reading error', absoluteFileName, loadingError.message)));
  });
}

function createTmpFile (fileName, tree) {
  return new Promise((resolve, reject) => {
    const tmpFile = path.join(TMP_DIR, INFRARED_TMP_DIR, fileName.replace('.js', '.json'))
    if (process.env.DEBUG) {
      writeToDebugFile(`${getTimestamp()} Created ${tmpFile}`);
    }
    fs.outputJson(tmpFile, tree)
      .then(() => resolve(tmpFile))
      .catch(e => reject(errorReporter('Temp file creation error', tmpFile, e.message)));
  });
}

module.exports = processFile;
