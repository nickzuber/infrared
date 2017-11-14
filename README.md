# Infrared <a href="#"><img src="https://travis-ci.org/nickzuber/infrared.svg?branch=master" /></a> <a href="#"><img src="https://img.shields.io/badge/project-active-brightgreen.svg" /></a> <a href="#"><img src="https://img.shields.io/badge/license-MIT%20Licence-blue.svg" /></a>

> Fast light weight inferred static type checker in real time for JavaScript

Infrared is a non-intrusive [top-down](https://en.wikipedia.org/wiki/Top-down_parsing) predictive [LL(1) parser](https://en.wikipedia.org/wiki/LL_parser) that statically analyzes [ES2017 JavaScript](https://medium.com/komenco/what-to-expect-from-javascript-es2017-the-async-edition-618e28819711). We construct an abstract syntax tree that's heavily influenced by the [Shift specifications](http://shift-ast.org/), and then infer a simple and strict type system.

## Now hold on just a minute there Jack...

Now don't get too excited yet young homie, Infrared is still under development. However, this means there's tons of room to [help out and contribute](https://github.com/nickzuber/infrared/pulls) if that's your jam.

As development continues I'll try my best to update the current state of the project here so it's easy to see where everything's at.

```
┌──────────────────────┬─────────────────────────────┐
│ Components           │ Approx. % Completed         │
├──────────────────────┼──────────────────────┬──────┤
│ Tokenizer            │ ████████████████████ │ 100% │
│ Parser               │ ████████             │  40% │
│ Type Inference       │                      │   0% │
│ JSON Transformer     │ ████████             │  40% │
│ AST Viewer           │ ████████████████████ │ 100% │
│ File Watching System │ ███████████████      │  70% │
│ Testing Framework    │ ██████████████████   │  90% │
└──────────────────────┴──────────────────────┴──────┘
```

## Why Would I Use This?

> Infrared can do a bunch of cool things for you.

<!-- ### Goals -->

## How and Why It Works — LR(k) Parsing vs LL(k) Parsing 

> Some neat implementation details and all that good stuff.

Since Infrared only cares about inferring and checking data types of your program, we're able to make some assumptions about certain things which help make us extra fast.

One interesting design choice is that we've constructed an leftmost derivation parser instead of something arguably more standard like a rightmost derivation parser. In making this decision, I had three main priorities to consider which ultimately lead me to choosing the leftmost approach:

 1. **Efficiency** — Will one implementation be more efficient than the other? Will we require more lookaheads with one approach? Which is faster practically?
 2. **Maintainability** — Is one approach easier and more straightforward to maintain? Will new developers be able to onboard relatively easily and understand what's going on?
 3. **Error Reporting** — How can we capture syntax and type errors with each approach? Which one has better error reporting?

### 1.) Efficiency and Compatibility

It really doesn't make much of a difference _in general_ when it comes to the efficiency of an LL parser and an LR parser. The LL approach has opportunities to be less performant (due to backtracking, mainly), however the [speed of both approaches are comparable](http://www.garshol.priv.no/download/text/bnf.html#id4.3.). 

Now we also need to worry about compatibility with JavaScript itself. Most JS compilers are written with a LR approach; even the AST specification we use here [implemented with a rightmost derivation approach](#). If we were to _evaluate_ JS, this would _absolutely_ make a difference. With precedence optimizations aside, imagine a simple expression of `1 - 2 + 3`. If we were to take a leftmost derivation approach, this would be evaluated as `(1 - (2 + 3)) = -4`, _however_, using a rightmost approach we'd get `((1 - 2) + 3) = 2`. 

So we know this choice matters when evaluating JS, but does it matter when inferring a type system? To be honest, I thought about this question quite a bit and had come to the conclusion that **it does not**! To put my own mind at rest, as well as any skeptics out there, I [wrote a short paper proving that this proposition holds true](proofs/binary_expression_commutativity.pdf). There might be some mistakes in there; feel free to call me out on it! The basic idea is that no matter the ordering of binary nodes in an AST, the inferred types will always propagate to the top all the same. 

### 1.1) Backtracking and Recursive Decent

Backtracking sucks when it comes to performance. We want to be super fast, so if we can avoid backtracking altogether — and luckily we can! Using [predictive parsing](http://lambda.uta.edu/cse5317/notes/node13.html), we can elimiate the need for backtracking since we can determine which path to travel down in our [recursive descent](https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm). So with this in mind, we can rest assured that the type of parser we implement won't really affect the efficiency of our program.

### 2.) Maintainability and Simplicity

Writing a leftmost derivation parser is hands down the [simplest and most straightforward approach](http://www.garshol.priv.no/download/text/bnf.html#id4.3) here. It's definitely way easier to read and understand a parser that uses a recursive and leftmost approach than it is to consider and parse through LR tables.

### 3.) Error Reporting and Understanding Context

Once again, the leftmost derivation approach wins again! Especially with a predictive strategy, it's trivial for the program to understand what it _should_ see and tell the user why something might have thrown an error. It's also pretty straightforward to [recover from an error and continue parsing](https://softwareengineering.stackexchange.com/a/19637/217663) the rest of the input if we wish, which might be helpful and is definitely a positive feature to have.

One of my main goals with this project is to produce meaningful messages. This is mainly focused with the type inference system, but this holds true for the parsing system as well. Having an error message that can provide insight to the _why_ and perhaps be able to suggest possible fixes is invaluable in my opinion; Error messages can be friends too!

## Installation

> Getting you up and running with Infrared in no time.

## Usage

> Now let's figure out how to actually use this thing.

## License

This software is free to use under the MIT License. See [this reference](https://opensource.org/licenses/MIT) for license text and copyright information.
