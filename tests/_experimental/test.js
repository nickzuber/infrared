function shuffle(array) {
    var currentIndex = array.length, temporaryValue, randomIndex;
    while (0 !== currentIndex) {
        randomIndex = Math.floor(Math.random() * currentIndex);
        currentIndex -= 1;
        temporaryValue = array[currentIndex];
        array[currentIndex] = array[randomIndex];
        array[randomIndex] = temporaryValue;
    }
    return array;
}
var nodeDOM = document.querySelector('span.place');
var list = ['JavaScript', 'Data Structures', 'Data Streams', 'Algorithms', 'Design Patterns', 'Functional Programming', 'Type Systems', 'Compilers'];
shuffle(list);
list.unshift('OCaml');
var t = new Typed(nodeDOM, {
    words: list,
    startDelay: 3000,
    timing: 65,
    backTiming: 40,
    pause: 2500,
    typoProbability: .05,
    maxTypos: 1,
    loop: true
});