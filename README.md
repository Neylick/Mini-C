This small project is an 3rd year double degree project in compilation made to typecheck and detect errors in small C programs with a few implemented functionalities.

Here are the details

# Implementation
## Mandatory implementations :
### Variables (globals or locals)
- int taille = 64;
### Functions 
- int pgcd(int a,int b) { ... }
### Instuctions
- ASCII display `putchar(n);`
- Value assignements to variables `x = n;`
- If statement `if(c) { s1 } else { s2 }`
- While loop `while (c) { s }`
- Return in functions `return (c);`
- expressions
### Expressions
- Constants `42`, `true`... 
- Operators (boolean, integers) `+`, `*`, `<` ... 
- Variables `x` 
- Function calls `f(x1,x2...)` 
### Types
- Integers `int` 
- Boolean `bool` 
- Void `void`	
### Fonctionnalites
- Declaration & init at once `int n = 0;` 
- Lexical analysis
- Syntaxical analysis
- Typecheck (Return, parameters, Variable assignment)
## Details :
- Hashmap are used to emulate environment changes.
## Ajouts :
### Fonctionnalites
- Integer constants : hexa `0xeFF1c4cE`, binary `0b0_1_0_1_0_1` et octals `0432213` (= `0ox432213` in Caml)
- Comments : `//` line & `/*` bloc `*/`
- Loops `do while` & `for`
- Switch statement `switch`, with the associated instructions : `break`, `default`, `case`.
- Instruction `continue` (no real effect in typechecker)
- Instructions `--` & `++` 
- Alternative `return` forms : `return n;` & `return;` & `return(n);`
- Alternative `if` forms : `if(c) { s1 }` `if(c) i1 else i2` `if(c) i1`
- Alternative `while` forms : `while(c);` looping forever if c is true
- Typechecking multiple files at once
### Errors
- Asserts `main` exists
- Assets a `return` statement is placed in every branch of any non `void` function
- Order of definitions for **global variables** & **functions** : *error when calling undefined variables*
- Double declaration (functions and variables).
## Details
- Error details giving line and column of the error
- The program is now just instructions followed by one another, instead of variables and functions declaration lists.
