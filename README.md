# lambda-playground

A simple REPL to experiment with the untyped lambda calculus. I made it for me to play with it and understand it better while reading the *Types and Programming Languages* book.

## Syntax and usage

The syntax is very simple. There are only three types of terms, which are **variables**, **abstractions** and **applications**, and two types of statements, **evaluations** and **assignements**.

**Variables**

```
x
foo
bar
```

**Abstractions**

```
\x.x
\x.x a -- Equals to \x.(x a)
\x.\y.x y
```

**Applications**

```
(\x.x) a
(\x.x) (\y.y)
a (b c)
a b c -- Equals to (a b) c
```

Note that you can use parenthesis to indicate how the interpreter should evaluate terms.

### Evaluations

To evaluate a term, simply write it directly. The interpreter will evaluate it until reaching the normal form

```
λ> (\x.x) a
(λx.x) a
a
```

### Assignments

You can also assign names to terms, to avoid repeating yourself. Names can be used inside terms, and they will be converted to the corresponding term before evaluation, as long as the name is not bound by an outer abstraction.

```
λ> id = \x.x
λ> zero = \s.\z.s z
λ> id zero
(λx.x) (λs.λz.z)
λs.λz.z
```

There are already some builtin names, and I will add some more in the future:

```
id    = λx.x
true  = λt.λf.t
false = λt.λf.f
zero  = λs.λz.z
one   = λs.λz.s z
two   = λs.λz.s (s z)
succ  = λn.λf.λx.f (n f x)
```

## Instalation

You must install some Haskell related tools, specifically `stack`. If you have it already installed, open a terminal and do the following steps:

``` sh
git clone https://github.com/Suguivy/lambda-playground.git
cd lambda-playground
stack build
```

If you want to have it installed in your environment to run it easily as a command (storing it in `/home/<user>/bin`), run `stack install`. After that run the interpreter with the command `lambda-playground`.

If you only want to run it, run `stack run` in the cloned folder.

## References
- Rob Nederpelt, Herman Geuvers. Type Theory and Formal Proof, an introduction.
- Benjamin C. Pierce. Types and Programming Languages.
