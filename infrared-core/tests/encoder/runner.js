const {exec} = require('child_process');

const files = [
  './infrared-core/tests/encoder/random.test.js ',
  './tests/experimental/a.js ',
  './tests/experimental/b.js',
];

exec((`infrared check ${files.join(' ')}`), (err, stdout, stderr) => {
  console.log(stdout, stderr)
});
