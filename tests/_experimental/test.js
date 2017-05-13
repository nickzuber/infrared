'use strict';

const Lexer = require('./lexer');
const Parser = require('./parser');
const Compiler = require('./compiler');
const Promise = require('bluebird');
const ProgressBar = require('progress');
const argv = require('minimist')(process.argv.slice(2));
const chalk = require('chalk');
const fs = require('fs');
const appendFile = Promise.promisify(fs.appendFile);
const readFile = Promise.promisify(fs.readFile);
