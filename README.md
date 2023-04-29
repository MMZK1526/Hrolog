# Hrolog

TODO:
1. [ ] Support function terms and their unification
   - [x] Support function terms and parse them
   - [ ] Write tests on function term parsing
   - [x] Implement function unification (without considering recursive usage)
   - [x] Use topological sort to detect recursive usage
   - [ ] Write tests on unification with function terms
   - [ ] Add example programs with function terms
2. [ ] Optimise with tabling
3. [ ] Use Haskeline for better REPL experience
4. [ ] Documentation

## Introduction
Hrolog is a [logic programming language](https://en.wikipedia.org/wiki/Logic_programming) that highly resembles [Prolog](https://en.wikipedia.org/wiki/Prolog). It will support both SLDNF-deduction and abduction as well as some logic-based learning algorithms. In the future, it may also support a Clingo-like Answer Set Programming semantics.

Note that Hrolog is NOT Prolog: they have different syntaxes, and Hrolog will only implement a subset of Prolog specification while adding some unique features.

A simple example of using the CLI is `TODO: here`. Check out [here](/src/Test/programs/) for more Hrolog code examples.

## Syntax
Use `cabal run Hrolog` to start the REPL.

TODO
