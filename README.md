# Hrolog

## Checklist
1. [x] Support function terms and their unification
   - [x] Support function terms and parse them
   - [x] Write tests on function term parsing
   - [x] Implement function unification (without considering recursive usage)
   - [x] Use topological sort to detect recursive usage
   - [x] Write tests on unification with function terms
   - [x] Add example programs with function terms
2. [ ] Refactor `PP` to use `Text` rather than `String` for better performance
3. [ ] Optimise with tabling
   - [ ] Check for atom subsumption
   - [ ] Implement logic that determines proven and proving atoms
4. [ ] Use Haskeline for better REPL experience
5. [ ] Add a timeout to each query. Allow user to set the timeout or quit with `:k`.
6. [ ] Documentation
7. [ ] Support negation
    - [ ] Implement negation as failure
    - [ ] Write tests on negation

## Introduction
Hrolog is a [logic programming language](https://en.wikipedia.org/wiki/Logic_programming) that highly resembles [Prolog](https://en.wikipedia.org/wiki/Prolog). It will support both SLDNF-deduction and abduction as well as some logic-based learning algorithms. In the future, it may also support a Clingo-like Answer Set Programming semantics.

Note that Hrolog is NOT Prolog: they have different syntaxes, and Hrolog will only implement a subset of Prolog specification while adding some unique features.

A simple example of using the CLI is `TODO: here`. Check out [here](/src/Test/programs/) for more Hrolog code examples.

## Syntax
Use `cabal run Hrolog` to start the REPL.

TODO
