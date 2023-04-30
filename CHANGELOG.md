# Revision history for Hrolog

## 0.1.0.2 -- 2023-04-30

* Support functional terms.
* Write tests on their parsing and unification.
* Add an example "peanoNumbers.hrolog" to demonstrate the use of functional terms.


## 0.1.0.1 -- 2023-04-08

* Basic functionality for Prolog-style logic programming.
* Only supports a very small subset of Prolog syntax for now:
  * Can write facts and rules without functions.
  * Can write constraints, although they now play no role in the actual evaluation of the program.
* Make a CLI REPL that can load programs from files and evaluate user-input queries, with somewhat nice error messages.
