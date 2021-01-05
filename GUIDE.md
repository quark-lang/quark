# Quark Lang's beginners guide

Quark is an interpreted lisp like programming language written in Typescript ; if you are familiar with lisps like Scheme or Common Lisp, learning this language won't be that hard, you can skip the introduction. But if you are a complete beginner in the lispy world, I recommend you to read the intro.

## Introduction

Quark's syntax is highly inspired from lisp, there are a few lisp concepts that differs from traditional programming.

### Prefixed notation in calculations

In everyday's life, we use infixed notation. E.g. if we want to add 5 and 6, we'll write `5 + 6`. But Lisps don't work like that, they use 
**prefixed** notation. This same operation will be written as `+ 5 6`. The operator will be placed at the beginning of our operation.

Parenthesized operations like this: `6 * (5 + 3)` would look like that: `* 6 (+ 5 3)` in prefixed notation.

### Prefixed notation in functions calls

In python for example, if I want to call print to display a message, I'll do `print("Hello, World !")`.

But in Lisps, prefixed notation is also used, then to do the same thing, you'll have to do `(print "Hello, World !")`.

Parentheses are used to mark the beginning and the end of a block.

To use a function with multiple arguments, in python for example, you'll do `print("Hello,", user)`, arguments are separated by a comma.
In Lisps, arguments are separated by a space, then, in order to do the same thing as above, you'll need to write `(print "Hello, " user)`

## Hello, World !

To stay traditional, we'll start with your first Quark hello world:

```lisp
{ # Creates a new block and scope
  (print "Hello, World !") # calls the print function with one argument: `"Hello, World !"`
}
```

## Conditions

In Quark, a conditionnal statement is structured like that:

```
(if (condition)
  (what to do if condition is true)
  (what to do if condition is false)
)
```

E.g.:

```lisp
{
  (let username "Wafelack")
  (if (= username "Wafelack")
    (print "Hello Wafelack !")
    (if (= username "Thomas") # If username is not Wafelack, then check if user is thomas
      (print "Hello Thomas !")
      (print "Hello," username) # If username is not Thomas and not Wafelack, it displays a generic message
    ))
}
```

## Functions

In Quark, functions are structured like that: 
```
(fn (argument_a argument_b) {
  function_body
})
```

Simple example with a factorial function: 
```lisp
{
  (let factorial (fn (n) {
    (if (< n 1) (return 1))
    (return (* n (factorial (- n 1))))
  }))
}
```

## Loops 

In Quark, loops are structured as the following : 
```
(while (condition) {
  body
})
```

E.g.: 
```lisp
{
  (let i 0)
  (while (< i 10) {
    (print i)
    (let i (+ i 1)) # increments i
  })
}
```