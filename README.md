# Calculator

Simple mathematical infix expression evaluator.

Converts expression to reverse polish notation using the shunting-yard algorithm, then evaluates and outputs.

## Features
- It's a calculator
- Also pretty colours

## Usage

```
$ ./calculator --help
Mathematical infix expression evaluator
Author: Freddy Cansick, https://github.com/freddycansic
Version: 1.0

Usage: calculator [OPTIONS] -- <INFIX>

Arguments:
  <INFIX>  

Options:
      --print-tree  Print the expression tree
      --print-rpn   Print the infix expression in reverse polish notation
  -h, --help        Print help
  -V, --version     Print version
```

```
$ ./calculator -- "(3 * 3.0) / 6. + 1.5 + (9 % 2) + 2^4 + .1"
[+] Expression evaluated to: 20.1
```
