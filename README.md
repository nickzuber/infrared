Infrared
========

Fast light weight inferred static type checker in real time for JavaScript.

[![state](https://img.shields.io/badge/state-work%20in%20progress-yellow.svg)]()
[![Build Status](https://travis-ci.org/nickzuber/infrared.svg?branch=master)]()
[![Uptime Robot status](https://img.shields.io/uptimerobot/status/m778918918-3e92c097147760ee39d02d36.svg)]()
[![active](https://img.shields.io/badge/project-active-brightgreen.svg)]()
[![License](https://img.shields.io/badge/license-MIT%20Licence-blue.svg)]()

What Does This Do For Me?
-------------------------

Infrared is a tool that quickly and statically analyzes your JavaScript programs for strict type errors. In other words, we find places in your code where you're accidentally that we would consider to be a mistake or a type error.

For example, we consider implicit coersions to be type errors.

```js
function add1 (n) {
  return n + 1
}

add1('73')  // type error: add1 expects a number
```

```js
add1('73')

function add1 (n) {
  return n + 1   // type error: tried to add 1 to a string
}
```

Notice how the ordering of the function definition and the function call make a difference when a type is inferred. Since Infrared infers the type system all on its own, it makes assumptions about what it sees when it sees it. 

In the first example, we see that there's a function named `add1` that takes a single argument called `n`. While looking at the contents of `add1`, we see that you're trying to add 1 to `n` so Infrared assumes that `n` is a number. In the second example, we see the function call of `add1('73')`, so Infrared thinks that the function `add1` takes a single argument of a string. So when we get to `return n + 1`, we think you're trying to add 1 to a string which would implicitly coerce the `1` to a string, which we already said is a type error as far as Infrared is concerned. 

Things to Know
--------------

It's important to realize that Infrared is a different tool than something like [Flow](https://github.com/facebook/flow). 

### Shortcuts and Tricks

Since Infrared only cares about inferring and checking data types of your program, we're able to make some assumptions about certain things which help make Infrared extra fast.

#### Greedy optimizations

One thing that we do is **greedy grouping**. When we're parsing a file and we encounter a situation where we need to group nodes in a certain order, we take the greedy approach and group nodes left to right. This is different than how most JavaScript interpreters operate, which usually group nodes from right to left. Consider the following example comparing how the AST specification Shift groups nodes in a binary expression verses how we do it in Infrared.

<p align="center"><img src="/.github/ast_example_text.png"></p>

<img align="right" width="400px" src="/.github/ast_example_shift.png" />
<img width="400px" src="/.github/ast_example_infrared.png" />

<br /><br /><br /><br /><br />

Using this greedy approach, we avoid having to make any lookahead operations. Instead of looking for the ending nodes and grouping backwards, Infrared just uses the context of the operation to march on forward, anticipating and grouping things as we go along. We sacrifice proper ordering for the speed of moving quickly. However, since all we care about are type comparisons, we can totally get away with doing this in some cases.

We only would ever change the ordering of a node if it is a **binary expression**, such that it is a node that takes some nodes on the left and on the right. In the specific case of a binary expression, it's totally fine to be able to change the ordering of its left and right nodes if we want to. 

Overview
--------

Blah.

Installation
------------

Blah.

Usage
-----

Blah.

License
-------

This software is free to use under the MIT License. See [this reference](https://opensource.org/licenses/MIT) for license text and copyright information.
