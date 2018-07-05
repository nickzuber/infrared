'use strict';

const path = require('path');
const processFile = require('./shift-parser');
const execInfraredCore = require('./shift-parser/handoff.js');

// @TEST
// Mock run
let promises = [];
promises.push(processFile(path.join(__dirname, './tests/experimental/index.js')));
promises.push(processFile(path.join(__dirname, './tests/experimental/a.js')));

Promise.all(promises).then(files => {
  execInfraredCore(files);
})
