# Hrolog

## Checklist
1. [x] Support function terms and their unification
   - [x] Support function terms and parse them
   - [x] Write tests on function term parsing
   - [x] Implement function unification (without considering recursive usage)
   - [x] Use topological sort to detect recursive usage
   - [x] Write tests on unification with function terms
   - [x] Add example programs with function terms
2. [x] Refactor `PP` to use `Text` rather than `String` for better performance
3. [x] Optimise with map instead of list of clauses
5. [x] More test on the CLI
6. [x] Make a DSL builder of Hrolog programs
7. [x] Use Haskeline for better REPL experience
   - [x] Migrate Monad stack to include `InputT`
   - [x] Support auto-completing commands
   - [x] Only auto-complete for file names if the command is `:l`
8. [ ] Add a timeout to each query. Allow user to set the timeout or quit with `:k`
9.  [ ] Documentation
10. [ ] Support negation
    - [ ] Implement negation as failure
    - [ ] Write tests on negation

## Introduction
Hrolog is a [logic programming language](https://en.wikipedia.org/wiki/Logic_programming) that highly resembles [Prolog](https://en.wikipedia.org/wiki/Prolog). It will support both SLDNF-deduction and abduction as well as some logic-based learning algorithms. In the future, it may also support a Clingo-like Answer Set Programming semantics.

Note that Hrolog is NOT Prolog: they have different syntaxes, and Hrolog will only implement a subset of Prolog specification while adding some unique features.

A simple example of using the CLI is `TODO: here`. Check out [here](/src/Test/programs/) for more Hrolog code examples.

## Syntax
Use `cabal run Hrolog` to start the REPL.

TODO
