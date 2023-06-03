# Revision history for Hrolog


## 0.1.0.5 -- 2023-06-03

* Support negation as failure.


## 0.1.0.4 -- 2023-05-26

* Use Haskeline for better REPL experience.
  * Support arrow keys to access history.
  * Support limited tab completion: can complete for command names and filenames.


## 0.1.0.3 -- 2023-05-14

* Introduce a DSL builder for writing Hrolog programs in Haskell.
* Translate all provided examples into the DSL in `Hrolog.Builder.Examples`.


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
