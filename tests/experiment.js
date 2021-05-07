const foo = add1(5);

function add1 (n) {
  const five = 5;

  function inner () { return null; }

  return n + five;
}

foo + 1

function inner () { return null; }
