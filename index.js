'use strict';

const fs = require('fs');
const {
  parseScriptWithLocation,
  parseModuleWithLocation
} = require("shift-parser");

const file = fs.readFile('./tests/experimental/index.js', 'utf8', (err, data) => {
  if (err) throw err;

  const {
    tree: ast,
    locations,
    comments
  } = parseModuleWithLocation(data);

  console.log(JSON.stringify(ast, null, 4))
  console.log(locations.get(ast.items[0]))
});
