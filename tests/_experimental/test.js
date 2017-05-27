class Parent {
    constructor() {
        // implicit (from the `super` call)
        //    new.target = Child;
        // implicit (because `Parent` doesn't extend anything):
        //    this = Object.create(new.target.prototype);
        console.log(new.target) // Child!
    }
}
class Child extends Parent {
    constructor() {
        // `this` is uninitialised (and would throw if accessed)
        // implicit (from the `new` call):
        //    new.target = Child 
        super(); // this = Reflect.construct(Parent, [], new.target);
        console.log(this);
    }
}
new Child;