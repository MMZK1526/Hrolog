# Hrolog

## Checklist
* [x] Support function terms and their unification
   - [x] Support function terms and parse them
   - [x] Write tests on function term parsing
   - [x] Implement function unification (without considering recursive usage)
   - [x] Use topological sort to detect recursive usage
   - [x] Write tests on unification with function terms
   - [x] Add example programs with function terms
* [x] Refactor `PP` to use `Text` rather than `String` for better performance
* [x] Optimise with map instead of list of clauses
* [x] More test on the CLI
* [x] Make a DSL builder of Hrolog programs
* [x] Use Haskeline for better REPL experience
   - [x] Migrate Monad stack to include `InputT`
   - [x] Support auto-completing commands
   - [x] Only auto-complete for file names if the command is `:l`
*  [x] Documentation
* [x] Support negation
   - [x] Implement negation as failure
   - [x] Write tests on negation
* [x] Support negation
   - [x] Implement negation as failure
   - [x] Write tests on negation
* [ ] Support printing out each step of the derivation
   - [x] Add setting to turn on/off printing
   - [x] Better printing format
   - [x] Print one step at a time

## Introduction
Hrolog is a [logic programming language](https://en.wikipedia.org/wiki/Logic_programming) that highly resembles [Prolog](https://en.wikipedia.org/wiki/Prolog). It will support both SLDNF-deduction and abduction as well as some logic-based learning algorithms. In the future, it may also support a Clingo-like Answer Set Programming semantics.

Note that Hrolog is NOT Prolog: they have different syntaxes, and Hrolog will only implement a subset of Prolog specification while adding some unique features.

A simple example of using the CLI is [here](#quick-example). Check out [here](/src/Test/programs/) for more Hrolog examples.

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

Finally, you can quit the REPL with `:q`.

## Syntax
This section covers the syntax of Hrolog's program, query, and REPL.

It only covers the syntax for *deduction*. In the future I will add *abduction* syntax.

### Program

The syntax of Hrolog is similar to that of Prolog, but with several variations and limitations.
1. In Hrolog, we use `<-` instead of `:-` to delimit the head and body of a clause.
2. Hrolog does not support variables starting with `_`.
3. The negation syntax in Hrolog is `!` instead of `\+`.
4. Hrolog can contain constraints, namely a clause with no head. For example, `<- a.` is a valid clause in Hrolog. However constraints are currently ignored by the interpreter.
5. Hrolog does not allow the usage of undeclared predicates and functions. Constants can be used without declaration.
6. Hrolog's inline comments start with `#` (compare to `%` in Prolog) and end with the end of the line. Hrolog does not have block comments.
7. Hrolog does not have built-in operators, lists, and tuples. However, they can be defined in a prefix manner. See [here](/src/Test/programs/peanoNumbers.hrolog) for an example of addition and subtraction.
8. Due to the lack of operators in Hrolog, digital constants does not have special meanings. They are treated as normal constants. For example, `0` is a constant, not a number.

A Hrolog program consists of **clauses**, which consists of at most one **head** and zero or more **bodies**. Each of the heads and bodies is a **term**. There are three types.
1. **Fact**: a fact is a clause with one head and no body. Its syntax is `<term> .` For example, `a.` is a fact. The semantics is that `a` is true.
2. **Rule**: a rule is a clause with one head and one or more bodies. Its syntax is `<term> <- <term> [, <term>]* .` For example, `a <- b, c.` is a rule. The semantics is that `a` is true if `b` and `c` are true.
3. **Constraint**: a constraint is a clause with no head and one or more bodies. Its syntax is `<- <term> [, <term>]* .` For example, `<- a, b.` is a constraint. Currently constraints are ignored by the interpreter.

In the examples above, all terms are literals. We can also introduce predicates, which describes the relationship between terms. For example, `father(anakin, luke)` is a predicate term. Here, `father` is the **predicate symbol**, and `anakin` and `luke` are the **arguments**. It could have a semantic meaning of "Anakin is the father of Luke".

We use names starting with lowercase letters or digits for predicate symbols and arguments, this is to differentiate between **variables** which starts with uppercase letters. Informally speaking, if a variable appears in the head, it represents any possible value; if it appears in the body, it represents the "existence" of a value that satisfies the body.
For example, `father(X, luke).` means "Anyone is the father of Luke", while `son(luke, X) <- father(X, luke).` means "If Luke has a father, then Luke is the son of that father".

With this, we could build somewhat meaningful rules such as:
```
father(anakin, luke).
father(anakin, leia).
mother(shmi, anakin).

ancestor(X, Y) <- father(X, Y).
ancestor(X, Y) <- mother(X, Y).
ancestor(X, Y) <- father(X, Z), ancestor(Z, Y).
ancestor(X, Y) <- mother(X, Z), ancestor(Z, Y).
```

Semantically, it first gives three examples of parenthood. Then it introduces rules for the predicate `ancestor`.

Note that literals are considered as special predicates that has zero arguments. Therefore, the literal `a` can also be written as `a()`.

The arguments to predicates can also be function terms. For example, the following program defines addition between Peano numbers:
```
add(0, Y, Y).
add(s(X), Y, s(Z)) <- add(X, Y, Z).
```

Here `s(X)` means the successor of `X`. Therefore, `s(s(0))` represents the number 2. The semantics of the program is that `add(X, Y, Z)` is true if `Z` is the sum of `X` and `Y`. For example, we can query for the result of 2 + 3 by `<- add(s(s(0)), s(s(s(0))), X).` and it will return `X = s(s(s(s(s(0)))))`.

### Query
A **query** is a special type of clause that has no head. It has the same syntax as a constraint, namely `<- <term> [, <term>]* .` except that `<-` can be omitted. For example, `<- a.` is a query. The semantics is that it is asking whether `a` is true.

Let's look at the ancestor example again
```
father(anakin, luke).
father(anakin, leia).
mother(shmi, anakin).

ancestor(X, Y) <- father(X, Y).
ancestor(X, Y) <- mother(X, Y).
ancestor(X, Y) <- father(X, Z), ancestor(Z, Y).
ancestor(X, Y) <- mother(X, Z), ancestor(Z, Y).
```

Here, if we query `<- ancestor(shmi, luke)`, it will return "Valid". If we query `<- ancestor(anakin, X)`. It will return `X = luke` and (on the second search) `X = leia`.

We can also have a query with multiple bodies. For example, `<- ancestor(X, Y), ancestor(Y, luke).` is looking for an ancestor of `luke` as well as an ancestor of ancestor of `luke`. It will return `X = shmi` and `Y = anakin`.

### REPL
Command            | Shorthand        | Description
------------------ | ---------------- | ---------------------------------
`:load <filename>` | `:l  <filename>` | Load the program from the file.
`:reload`          | `:r`             | Reload the program from the file.
`<- <query>`       | `<query>`        | Query the program with the query.
`:help`            | `:h`             | Show the help message.
`:quit`            | `:q`             | Quit the REPL.
`:set <setting>`   | `:s <setting>`   | Set the setting. See below for details.

`<setting>: [(+|-) <settingName>]+`

Setting Name | Description
------------ | -----------
`oneAnswer`  | Only compute one answer for each query. Default is `false`.
`showSteps`  | Show each step of the evaluation. Default is `false`.
