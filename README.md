# lambda-playground

A simple REPL to experiment with the untyped lambda calculus I made in one night. I made it for me to play with it and understand it better.

I'm planning to improve it and add some more features.

## Syntax
The syntax is very simple. There are only three types of terms, which are **variables**, **abstractions** and **applications**:

To write a **variable**, simply put a name, composed of letters only:
```
x
```

To write an **abstraction**, write the function but instead of the lambda, use the `\\` symbol:

```
\x.x
\x.\y. x y
```

To write an **application**, simply put the two terms one after the other:

```
(\x.x) (\y.y)
```

You can use parenthesis to indicate how to apply terms.

## Usage

The interpreter will show, step by step, how the expresion is evaluated, until reach the normal form:

```
λ> (\x.x) (\y.(a b c (c d)))
(λx.x) (λy.a b c (c d))
(λy.a b c (c d))
λ> (\x.x) (\y.y) (\z.z) (\a.a)  
(λx.x) (λy.y) (λz.z) (λa.a)
(λy.y) (λz.z) (λa.a)
(λz.z) (λa.a)
(λa.a)
```

