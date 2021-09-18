# Assignment 1—Formula Manipulator

## Introduction

In this assignment you will write a command-line interface program to
manipulate formulas containing variables, constant values, multiplications,
and additions. We have created a Haskell data type for these formulas, `Expr`,
that you have to use in this assignment. 

You find the definition of `Expr` in the file
[`src/ExprLanguage.hs`](src/ExprLanguage.hs) together with the function
`parseExpr` to translate a String to an `Expr`.  In that source file you can
find [Haddock](https://haskell-haddock.readthedocs.io/en/latest/index.html)
documentation explaining how to use this module. 

**Tip** Run `cabal haddock` to generate nice HTML formatted documentation.

**Note** The `Expr` type in this assignment is **different** from the
expression tree type you have seen in Chapter 8 of *Learn Haskell*, [Expression
Trees](http://learn.hfm.io/expressions.html). 

## Assignment

This assignment consists of six programming
exercises. In the end you should have a working program with a command-line
interface as specified in Exercise 6.

In the six programming exercises you do not have
to calculate a definition nor give a definition in mathematical notation.
However, you have to explicitly add a type declaration to every function you
define in Haskell. As part of these six programming exercises, you also have
to write tests in [`test/Spec.hs`](test/Spec.hs).

### Split over two weeks

Exercises 2 through 6 build on your solution of exercise 1 and its application
in exercise 2. To prevent you from failing the whole assignment because you
fail to complete exercise 1 or don't understand how to apply it in exercise 2,
we run this assignment in two parts.

- In the first week, you focus on exercises 1 and 2. You submit your project
  before the Monday lab session starts.

  During the Monday lab session, we discuss the solution to these two
  exercises. You can use what you learn during the lab session while solving
  the other exercises in the assignment. 

  Of course, you can continue with exercises 3 through 6 already in the first
  week.

- In the second week, you finish the complete assignment. You submit your whole project.

### Grading

We grade you on exercises 1 and 2 based on your submission in the first week.
We grade you on exercises 3- 6 based on your submission in the second week.


### Getting started

1.  Clone this repository. It contains a Cabal project. Do not *manually* add
    files to or remove files from this project, nor change any file names. All
    you have to do is changing the contents of
    [`src/FormulaManipulator.hs`](src/FormulaManipulator.hs),
    [`src/FormulatorCLI.hs`](src/FormulatorCLI.hs), and [`test/Spec.hs`](test/Spec.hs).

2.  Verify that the project works by running `cabal test`. All tests pass.

3.  Do the exercises in this README. Invariant: All tests pass.

    "All tests pass" means that your project should build. **If your project
    does not build, you fail this assignment**. If you are unable to get
    something to work, comment out the problematic parts to keep the project
    working.

4.  In completing this assignment, you deliver three files:

    * The Haskell file [`src/FormulaManipulator.hs`](src/FormulaManipulator) for
      exercises 1 through 5;
    * The Haskell file [`src/FormulatorCLI.hs`](src/FormulatorCLI.hs) for exercise 6;
    * The Haskell file [`test/Spec.hs`](test/Spec.hs) for exercises 1 through 6;
      and

    Do not forget to have your names, student numbers, and date visible and at the
    top of all files you edit.

## Exercises

### Program the core functionality in module `FormulaManipulator`

#### Exercise 1

Just like we have implemented catamorphism factories `foldN` and `foldL` in
Haskell for the natural numbers and lists, we can also implement a
catamorphism factory for the `Expr` type.

Remember, the catamorphism factory `foldL` can be implemented in Haskell as
follows:

```haskell
foldL :: b -> (a -> b -> b) -> [a] -> b
foldL n c = rec
            where
              rec []      = n             -- nil  case
              rec (x:xs)  = c x (rec xs)  -- cons case
```

Analogous to `foldL`, implement the catamorphism factory for the type `Expr`
called `foldE` in [`src/FormulaManipulator.hs`](src/FormulaManipulator.hs). 

Properly document the function with
[Haddock](https://www.haskell.org/haddock/); run `cabal haddock` to generate
and read your own documentation.

Convince yourself and us that your implementation works by writing tests for
this function in [`test/Spec.hs`](test/Spec.hs).
  
#### Exercise 2

Using the function `foldE`, implement function `printE` in
[`src/FormulaManipulator.hs`](src/FormulaManipulator.hs) that pretty-prints an
expression. For all expression it must hold that if you parse the
pretty-printed expression you get the original expression back. Stated in
Haskell terms: for all expressions `e` in `Expr` it holds `e == ((\(Right x)
-> x) . parseExpr . printE) e`

Properly document the function with
[Haddock](https://www.haskell.org/haddock/); run `cabal haddock` to generate
and read your own documentation.

Convince yourself and us that your implementation works by writing tests for
this function in [`test/Spec.hs`](test/Spec.hs).

#### Exercise 3

Using `foldE`, implement an evaluator function `evalE` in
[`src/FormulaManipulator.hs`](src/FormulaManipulator.hs) to calculate the
value of an expression. The type of this function is `(a -> Integer) -> (Expr
a Integer) -> Integer`.

The first parameter to `evalE` is a lookup table function that maps variables
to values. For example,

```{haskell}
evalE 
  (\v -> if v == "x" then 4 else error "unknown variable") 
  (Mult (Var "x") (Const 3))
```
  
should evaluate to `12`.

Properly document the function with
[Haddock](https://www.haskell.org/haddock/); run `cabal haddock` to generate
and read your own documentation.

Convince yourself and us that your implementation works by writing tests for
this function in [`test/Spec.hs`](test/Spec.hs).


#### Exercise 4

Using `foldE`, implement function `simplifyE` in
[`src/FormulaManipulator.hs`](src/FormulaManipulator.hs) that simplifies an
expression using the following simple rules:

* Handling units with multiplication and addition:
  
  * (∀ `x`: `Num x`: 0 + `x` = `x`)
  * (∀ `x`: `Num x`: 0 * `x` = 0)
  * (∀ `x`: `Num x`: 1 * `x` = `x`)

* Simplify constant expressions by evaluating them, for example

  * 3 * 15 can be simplified to 45 and
  * 7 + 12 can be simplified to 19

Properly document the function with
[Haddock](https://www.haskell.org/haddock/); run `cabal haddock` to generate
and read your own documentation.

Convince yourself and us that your implementation works by writing tests for
this function in [`test/Spec.hs`](test/Spec.hs).

#### Exercise 5

Using `foldE`, implement function `diffE` in
[`src/FormulaManipulator.hs`](src/FormulaManipulator.hs) to differentiate an
expression for a given variable. For example, `(printE . diffE "x") (Mult (Var
"x") (Const 2))`  should return `"((x * 0) + (1 * 2))"`.

Let `'` represents the derivative for `x`. Implement the following
differentiation rules:

* Constant rule: (∀ `c`: `Num c` : `c'` = 0)
* `y'` = 0 if `y` ≠ `x`
* `x'` = 1
* Sum rule: (`f(x)` + `g(x))'` = `f'(x)` + `g'(x)`
* Product rule: (`f(x)` * `g(x)`)`'` = `f(x)` * `g'(x)` + `f'(x)` * `g(x)`
  
**Hint**  Using a catamorphism on `Expr` directly will not work. To apply the
product rule, you need access to both the original functions and their
derivatives.  There was a similar problem writing a function for `n!` as a
catamorphism (see Lecture 6). The solution was to use tupling of `(fac n, n)`,
for which we were able to use a catamorphism. This is called a *paramorphism*.
The same solution can be applied in this exercise: tuple `(Expr, Expr)`.

Properly document the function with
[Haddock](https://www.haskell.org/haddock/); run `cabal haddock` to generate
and read your own documentation.

Convince yourself and us that your implementation works by writing tests for
this function in [`test/Spec.hs`](test/Spec.hs).
  
### Program a command-line interface in module `FormulatorCLI`

#### Exercise 6
  
Create a simple command-line interface. We already have written a suitable
`main :: IO ()` function in the file [`app/Main.hs`](app/Main.hs):

```haskell
main :: IO ()
main = do
        as <- getArgs
        print (processCLIArgs as)
```

This `main` function uses the IO monad, which you do not already (need to)
know. In the `main` function the command-line arguments are read into the list
`as` by the `getArgs` function. This list with command-line arguments is then
processed by the function `processCLIArgs`. The output of
`processCLIArgs` is printed to the console.

Implement this function `processCLIArgs :: [String] -> String` in
[`src/FormulatorCLI.hs`](src/FormulatorCLI.hs). You are allowed, but not required, to use the
following modules:

- [`Data.List.Split`](https://hackage.haskell.org/package/split-0.1.1/docs/Data-List-Split.html)
  to split lists based on some delimeter.
- [`Data.Either`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Either.html)
  for all kinds of `Either` related functionality.

These modules are already imported in [`src/FormulatorCLI.hs`](src/FormulatorCLI.hs) and added
to the Cabal project.

Compile and run your project as follows:

```bash
cabal build
cabal exec formulator -- OPTION EXPR
```

Where `EXPR` is an expression that can be parsed with
the function `parseExpr` and `OPTION` is one of the following options:

* `--print` or `-p`: pretty-print the expression
* `--simplify` or `-s`: simplify **and** pretty-print the expression
* `--differentiate <VAR>` or `-d <VAR>`: differentiate
    expression for `<VAR>` **and** simplify **and** pretty-print the result
* `--evaluate <LOOKUP>` or `-e <LOOKUP>`: evaluate the
    expression given the `<LOOKUP>` table. The lookup table is a
    String containing a list of `<VAR>=<VALUE>` pairs
    separated by semicolons. For example, `"x=4;y=5"` should give
    `x` the value `4` and `y` the value
    `5`.
* `--help` or `-h` should give a short help
    message on how to use your program.
  
Your program should also give helpful error messages when things go wrong.

For example:

```bash
cabal exec formulator -- --evaluate "f=1;g=7" "3 + 5 * g * f"   
  => "38"
cabal exec formulator -- -d "f" "3 + 5 * 5 * f * f"             
  => "((25 * f) + (25 * f))"
cabal exec formulator -- --simplify "3 + 1 * x + (x * 0) + 45"  
  => "((x + 3) + 45)"
cabal exec formulator -- -p "3 + 1 * x + (x * 0) + 45"          
  => "(((3 + (1 * x)) + (x * 0)) + 45)"
```

Properly document the function with
[Haddock](https://www.haskell.org/haddock/); run `cabal haddock` to generate
and read your own documentation.

Convince yourself and us that your implementation works by writing tests for
the function `processCLIArgs` in [`test/Spec.hs`](test/Spec.hs).
