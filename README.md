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

A simple example of using the CLI is [here](#quick-example). Check out [here](/src/Test/programs/) for more Hrolog code examples.

If you discover any bugs or have any suggestions, please feel free to open an issue or contact me.

## Quick Example
Use `cabal run Hrolog` to start the interactive shell.

Once inside, you can load a Hrolog program with `:l <file_name>`. For example, the following instruction loads the example program [simpleNumbers.hrolog](/src/Test/programs/simpleNumbers.hrolog):
```
> :l src/Test/programs/simpleNumbers.hrolog
Program src/Test/programs/simpleNumbers.hrolog loaded:
succ(1, 0).
succ(2, 1).
succ(3, 2).
gt(X, Y) <- succ(X, Y).
gt(X, Y) <- succ(X, Z), gt(Z, Y).

```
Then you can query the program with `<- <query>`. For example, the following query asks for a number greater than 1:
```
<- gt(X, 1).

Solution:
X = 2;

Enter ';' to look for the next solution.
```

It finds a solution `X = 2`. You can press `;` to look for the next solution:
```
> ;

Solution:
X = 3;

Enter ';' to look for the next solution.
> ;
No more solutions.
```

Since we only defined the numbers 0, 1, 2, and 3, it cannot find any number greater than 3:
```
> gt(X, 3). # Note that the `<-` can be omitted. Also, the `#` starts a comment, which is legal even in one-liner queries.
No solution.
```

See the [Syntax](#syntax) section for more details on the syntax of Hrolog, both in the program and in the query.

## Syntax
Use `cabal run Hrolog` to start the REPL.

TODO
