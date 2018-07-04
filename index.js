#!/usr/bin/env node
'use strict';

const fs = require('fs');
const { parseScriptWithLocation, parseModuleWithLocation } = require("shift-parser");

function loadFile(file) {
  const _ = fs.readFile(file, 'utf8', (loadingError, data) => {
    if (loadingError) {
      console.log(loadingError);
      process.exit(1);
    }

    try {
      const {
        tree: ast,
        locations,
        comments
      } = parseModuleWithLocation(data);

      console.log(JSON.stringify(ast, null, 4));
      console.log(locations.get(ast.items[0]));
    } catch (parsingError) {
      console.log(`"${parsingError.description}" at ${parsingError.line}:${parsingError.column}`)
    }
  });
}

// @TEST
// Mock run
loadFile('./tests/experimental/index.js');
