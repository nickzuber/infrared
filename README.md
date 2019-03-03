# Infrared <a href="#"><img src="https://travis-ci.org/nickzuber/infrared.svg?branch=master" /></a> <a href="#"><img src="https://img.shields.io/badge/project-active-brightgreen.svg" /></a> <a href="#"><img src="https://img.shields.io/badge/license-MIT%20Licence-blue.svg" /></a>

> Blazing fast, light-weight, inferred static type checker for JavaScript.

Infared is a [static analysis tool](https://stackoverflow.com/questions/49716/what-is-static-code-analysis) that eliminates 99% of [runtime type errors](https://techterms.com/definition/runtime_error). This is done by statically inferring a [fluid type system](#) onto your program, and identifying any potential type errors that can occur, before they happen.

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
