Infrared
========

Fast light weight inferred static type checker in real time for JavaScript.

[![state](https://img.shields.io/badge/state-work%20in%20progress-yellow.svg)]()
[![Build Status](https://travis-ci.org/nickzuber/infrared.svg?branch=master)]()
[![Uptime Robot status](https://img.shields.io/uptimerobot/status/m778918918-3e92c097147760ee39d02d36.svg)]()
[![active](https://img.shields.io/badge/project-active-brightgreen.svg)]()
[![License](https://img.shields.io/badge/license-MIT%20Licence-blue.svg)]()

Why use this?
-------------

Blah.

Things to Know
--------------

### Shortcuts and Tricks

Since Infrared only cares about inferring and checking data types of your program, we're able to make some assumptions about certain things which help make Infrared extra fast.

#### Greedy optimizations

One thing that we do is **greedy grouping**. When we're parsing a file and we encounter a situation where we need to group nodes in a certain order, we take the greedy approach and group nodes left to right. This is different than how most JavaScript interpreters operate, which usually group nodes from right to left. 

<p align="center"><img src="/.github/ast_example_text.png"></p>

<img align="right" width="400px" src="/.github/ast_example_infrared.png" />
<img width="400px" src="/.github/ast_example_shift.png" />

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
