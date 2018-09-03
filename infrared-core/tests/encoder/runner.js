const {exec} = require('child_process');

exec(('infrared check ./infrared-core/tests/encoder/random.test.js'), (err, stdout, stderr) => {
  console.log(stdout, stderr)
});
