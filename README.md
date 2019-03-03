# <img width="28" src="assets/logo-light.png" /> Infrared <a href="#"><img src="https://travis-ci.org/nickzuber/infrared.svg?branch=master" /></a> <a href="#"><img src="https://img.shields.io/badge/project-active-brightgreen.svg" /></a> <a href="#"><img src="https://img.shields.io/badge/license-MIT%20Licence-blue.svg" /></a>

> Blazing fast, light-weight, inferred static type checker for JavaScript.

Infrared is an configurable & efficient static type checker for JavaScript. This is done by statically inferring a [fluid type system](#what-is-a-fluid-type-system) onto your entire program – tracking the types of your variables as they change – and raising type errors before they happen at runtime.

## What is a Fluid Type System?

A fluid type system is no foreign idea – its a forgiving set of typing rules that _change_ as your program changes. This means no variables are ever committed to a single type – if that variable changes its type somewhere in the program, Infrared keeps track of it.

JavaScript is a dynamic language, and Infrared doesn't want to change that. While other type systems out there want you to change the way you program (and change your JavaScript into something else), Infrared does the opposite.

To understand the way Infrared fits into the typing ecosystem, consider the following:

 - If you're looking for a superset type system that tries to promote soundness, you should use [Flow](https://flow.org/).
 - If you're looking for a superset type system that encourages unsoundness, you should use [TypeScript](https://www.typescriptlang.org/).
 - If you don't want to change anything in your project, but still want to know the types of things in your program + have some type safety at compile time, you should use Infrared.

## Planning, Roadmap, and What Lies Ahead

Infrared is a really big project, so naturally it's a pretty good idea to make sure we plan things out carefully to avoid a janky-mess.

I'm using Figma to organize the different sections and responsibilities of each part of Infrared (parser, compiler, server, etc.). Feel free to follow along and check out [what I have mapped out so far](https://www.figma.com/file/VLacrQPUdTH19kJSiGy5zCzu/workflow?node-id=0%3A1).

Unfortunately, Figma only reflects the finalized parts of the development _roadmap_. This means there's a lot of cool work – like typing rules, reduction strategies, discrete proofs, etc – that aren't in this document.

Since those bits are written in a physical notebook, it's hard for me to share publically online. The good news is that I plan on writing a white paper once this project is finished, and all of the cool stuff will be included in there.

Until then, I'm more than happy to chat with anybody who's interested to learn more – feel free to reach out on [Twitter](https://twitter.com/nick_zuber/).

## FAQ

**Q** – Can I use this?

**A** – Not yet. This project is still under development, but expect an alpha release soon(_ish_)!

## License

This software is free to use under the MIT License. See [this reference](https://opensource.org/licenses/MIT) for license text and copyright information.
