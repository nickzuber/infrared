const age = 25;
const foo = addFive(6);

function addFive (n) {
  const five = 5;
  return n + five;
}

function doStuff (n) {
  const five = 5;

  function inner () {
    return null;
  }

  if (n > 10) {
    const ignore = undefined;
    return null;
  }

  return n + five;
}

age + 1
age

function inner () {
  return null;
}
