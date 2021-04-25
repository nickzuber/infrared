// const one = 1;

// function doNothingInteresting (a, b) {
//   function getNumberOne () {
//     return 1;
//   }
//   if (Math.random() > 0) {
//     return null
//   }
//   return getNumberOne();
// }
// doNothingInteresting(one, 2);
// baz(one, 2);

const foo = add1(5);

function add1 (n) {
  const five = 5;

  function inner () { return null; }

  return n + five;
}
function inner () { return null; }
