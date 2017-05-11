#!/usr/bin/env node

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

process.env.INIT_CWD = process.cwd();

function initProgressBar () {
  var width = process.stdout.columns;
  var bar = new ProgressBar('╢:bar', {
    complete: '▓',
    incomplete: '░',
    width: width,
    total: 6
  });
  return bar;
}

const bar = {
  tick: function () {}
};

var allFiles = [];
var localDoxy = false;
var CLIOutputDest = null;
var revivedData = null;
var currentTimeReference = new Date();
var timeStamp = '['+chalk.gray(('0'+currentTimeReference.getHours()).slice(-2)+':'+('0'+currentTimeReference.getMinutes()).slice(-2)+':'+('0'+currentTimeReference.getSeconds()).slice(-2))+'] ';
var startTime = new Date();

var blackListedDirectories = [
  '.git',
  'node_modules'
];

if(Object.getOwnPropertyNames(argv).length > 1) {
  Object.getOwnPropertyNames(argv).filter(function(flag){
    if(flag === '_') return false;
    resolveFlag(flag, argv[flag]);
  });
}
continueProcess();

function continueProcess() {
  bar.tick();
  if(!argv._.length){
    if(!checkForDoxyFile()) {
      resolveFlag('h');
      process.exit(0);
    }else{
      localDoxy = true;
    }
  }
  console.log('\n'+timeStamp+'Attempting to fetch files');
  if(!localDoxy){
    argv._.map(function(path){
      var tokenize = /(\.{1,2}|[0-9a-zA-Z]+)(?:(\/))|([0-9a-zA-Z.]+\.\w+$)/ig;
      var pathTokens = [];
      var lex = null;
      while(lex = tokenize.exec(path)){
        pathTokens.push(lex[1]||lex[3]);
      }
      var isDirectory = false;
      try{
        isDirectory = !!fs.lstatSync(path).isDirectory();
      }catch(err){
        reportError('Given path does not exist or was unable to be located: \n'+err);
        if(path.indexOf('*')>-1){
          reportError('  If you\'re trying to use a wildcard (*) to relay every type of file within a directory, do not use it.');
          reportError('  Simply pass in the path to the directory and all the valid files will be compiled automatically.');
        }
        process.exit(1);
      }
      if(isDirectory){
        recurseAllFilesInDirectory(path, allFiles);
      }
      var isFile = false;
      try{
        isFile = !!fs.lstatSync(path).isFile();
      }catch(err){
        reportError('Given path does not resolve to a file or was unable to be located: \n'+err);
        process.exit(1);
      }
      if(isFile){
        if(validFilePath(pathTokens)){
          allFiles.push(path);
        }else{
          console.log('Invalid file type:');
          reportError('  '+pathTokens[pathTokens.length-1]);
          console.log('For help use the --h or --help command.');
          process.exit(0);
        }
      }
    });
    var outputDest =  CLIOutputDest ||
                      null;
    workOnFileTree(outputDest);
  }else{
    console.log(timeStamp+'Using information from '+chalk.magenta('doxyfile.json'));
    readFile('./doxyfile.json', 'utf8').then(function (data, err) {
      if(err){
        reportError("Error occured when attempting to read file: doxyfile.json\n\n"+err.message);
      }
      revivedData = JSON.parse(data);
      if(typeof revivedData.targets === 'string'){
        allFiles.push(revivedData.targets);
      }else{
        revivedData.targets.map(function(target){
          var tokenize = /(\.{1,2}|[0-9a-zA-Z]+)(?:(\/))|([0-9a-zA-Z.]+\.\w+$)/ig;
          var pathTokens = [];
          var lex = null;
          while(lex = tokenize.exec(target)){
            pathTokens.push(lex[1]||lex[3]);
          }
          var isDirectory = false;
          try{
            isDirectory = !!fs.lstatSync(target).isDirectory();
          }catch(err){
            reportError('Given path does not exist or was unable to be located: \n'+err);
            if(target.indexOf('*')>-1){
              reportError('  If you\'re trying to use a wildcard (*) to relay every type of file within a directory, do not use it.');
              reportError('  Simply pass in the path to the directory and all the valid files will be compiled automatically.');
            }
            process.exit(1);
          }
          if(isDirectory){
            recurseAllFilesInDirectory(target, allFiles);
          }
          var isFile = false;
          try{
            isFile = !!fs.lstatSync(target).isFile();
          }catch(err){
            reportError('Given path does not resolve to a file or was unable to be located: \n'+err);
            process.exit(1);
          }
          if(isFile){
            if(validFilePath(pathTokens)){
              allFiles.push(target);
            }else{
              console.log('Invalid file type:');
              reportError('  '+pathTokens[pathTokens.length-1]);
              console.log('For help use the --h or --help command.');
              process.exit(0);
            }
          }
        });
      }
      var outputDest = revivedData.output ||
                        CLIOutputDest ||
                        null;
      workOnFileTree(outputDest);
    });
  }
}

function workOnFileTree(outputDest){
  bar.tick();
  console.log(timeStamp+'Working on '+chalk.cyan(allFiles.length)+' file'+(allFiles.length > 1 ? 's' : ''));

  if (outputDest) {
    try {
      fs.writeFileSync(outputDest, '', 'utf8');
    } catch (err) {
      reportError(`Unable to access the file ${outputDest}: \n${err}`);
    }
    console.log(`${timeStamp}Cleared ${chalk.magenta(outputDest)} prepping for output`);
  }

  var finishedFiles = 0;
  allFiles.map(function(taskFile){
    fs.readFile(taskFile, 'utf8', function(err, data){
      if(err){
        reportError("Error occured when attempting to read file: "+taskFile+'\n\n'+err.message);
      }
      bar.tick();
      var tokenizer = new Lexer(taskFile, data);
      try{
        tokenizer.generateTokens();
      }catch(e){
        reportError(e);
      }
      var parser = new Parser(taskFile, tokenizer.tokenList);
      try{
        parser.generateTokenTree();
      }catch(e){
        reportError(e);
      }
      var markdownGenerator = new Compiler(parser.tokenTree);
      markdownGenerator.compile();
      bar.tick();

      if (outputDest) {
        appendFile(outputDest, markdownGenerator.output).then(function (content, err) {
          if (err) {
            reportError(`Unable to write output from ${taskFile} to ${outputDest}.`);
          }
          var segue = markdownGenerator.flagged ? 'some' : 'all';
          console.log(`${timeStamp}Successfully wrote ${segue} of ${chalk.cyan(taskFile)} documention from to ${chalk.magenta(outputDest)}`);
          if (markdownGenerator.flagged) {
            console.log(`${timeStamp}${chalk.dim.white(` └── Skipped over one or more comment blocks in ${taskFile} due to missing fields.`)}`);
          }
          if (++finishedFiles === allFiles.length) {
            finishProcess();
          }
        });
      }
      else {
        var segue = markdownGenerator.flagged ? 'some' : 'all';
        console.log(`${timeStamp}Successfully wrote ${segue} of ${chalk.cyan(taskFile)} documention from to console:\n`);
        if (markdownGenerator.flagged) {
          console.log(`${timeStamp}${chalk.dim.white(` └── Skipped over one or more comment blocks in ${taskFile} due to missing fields.`)}`);
        }
        console.log(markdownGenerator.output);
      }
    });
  });
}

function finishProcess () {
  bar.tick();
  var endTime = new Date();
  if (bar.complete) {
    console.log(`${timeStamp}Finished after ${chalk.magenta(endTime - startTime)} ms`);
  }
}

function recurseAllFilesInDirectory(path, allFiles){
  var slashIfNeeded, actualPath;
  fs.readdirSync(path).map(function(derivedFile){
    if(blackListedDirectories.indexOf(derivedFile) === -1){
      if(validFilePath(derivedFile)){
        slashIfNeeded = path[path.length-1] === '/' ? '' : '/';
        actualPath = path + slashIfNeeded + derivedFile;
        allFiles.push(actualPath);
      }else{
        slashIfNeeded = path[path.length-1] === '/' ? '' : '/';
        actualPath = path + slashIfNeeded + derivedFile;
        if(fs.lstatSync(actualPath).isDirectory()){
          recurseAllFilesInDirectory(actualPath, allFiles);
        }
      }
    }else{
      console.log(timeStamp+'Ignoring '+chalk.dim.yellow(derivedFile)+' directory');
    }
  });
}

function resolveFlag(flag, value){
  switch(flag){
    case 'h':
    case 'help':
      console.log(chalk.white('\nUsage: doxide <command>\n'));
      console.log(chalk.white('  Possible <commands> could be:\n'));
      console.log(chalk.white('  doxide                             Compiles based on your doxyfile.json'));
      console.log(chalk.white('  doxide --h                         Prompts the help screen'));
      console.log(chalk.white('  doxide --help                      Prompts the help screen'));
      console.log(chalk.white('  doxide <file>                      Compiles <file>'));
      console.log(chalk.white('  doxide <directory>                 Compiles all valid files in <directory>'));
      console.log(chalk.white('  doxide <file1> -o <file2>          Compiles <file1> and stores output in <file2>'));
      console.log(chalk.white('  doxide <directory> -o <file>       Compiles all valid files in <directory> and stores output in <file>'));
      process.exit(1);
      break;
    case 'o':
      if (typeof value !== 'boolean') {
        CLIOutputDest = value;
      }
      break;
    default:
      console.log(chalk.white('Unrecognized command: ') + chalk.bold.red(flag));
      console.log(chalk.white('Here is a list of possible <commands>:'));
      console.log(chalk.white('  --h\n  --help\n  -o\n  '));
      process.exit(1);
  }
}

function checkForDoxyFile(){
  var localFiles = fs.readdirSync('./');
  return (localFiles.indexOf('doxyfile.json') > -1);
}

function validFilePath(file){
  if(typeof file === 'string'){
    var minimalSyntax = /(([0-9a-zA-Z])+(:?\.)?)+(.js)$/ig;
    return file.match(minimalSyntax);
  }else if(file instanceof Array){
    if(!file.length){
      reportError('Error while validating file path: too few tokens');
      process.exit(1);
    }
    var _file = file[file.length-1];
    var _extension = _file.split(".").pop();
    return _extension === 'js';
  }
}

function reportError(err){
  console.log(chalk.bold.red(err));
  process.exit(2);
}