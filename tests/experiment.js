// var x;
// var x = 123;
// var [x] = 123;
// var x = 2, y = 3;
// x + 2;

const firstName = "Nick";
const age = 25;
firstName - age;
age.foo.bar;

function callParent (child) {
  const parent = getParent(child);
  return child.call(parent);
}

// env :
//   x : number
//
//
function f (x) { // x: number -> string
  var _ = x - 1; // x : number
  x = 'string' // x2 : string
  return x; // ret -> typeof x2
}
