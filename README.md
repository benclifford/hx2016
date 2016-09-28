0. Before the start

TODO: get some kind of stack hello world.
TODO: get a reddit account

1. intro:

i'm going to talk us through building a bot that talks to reddit. you can
follow along on your laptop.

1.1 first let's find out what level of experience people have so I can try
to adjust the pace.

put your hand up who:

  1.1.1. has never touched any haskell code ever before

  1.1.2. can write this program:

main = do
  putStrLn "Hello world"

  1.1.3 can write something more complex

1.2 I'm going to have two things on my screen: some text notes, and
an interactive haskell environment 'ghci', and I'll be switching
back and forth between them. You should be able to get the same
set up and we'll do that in a minute.

1.3 About copy-and-paste - there will be a lot of code in this file
that you can, if you want, copy and paste into files. But I
recommend against it - instead I recommend thinking about what you
are trying to do and typing it yourself, or perhaps typing a
variant (changing variable names and things like that) so that you
actually think about what is going through...

2. hello world

so lets get started.

we can get 'ghci' running like this:
```
$ stack ghci
Run from outside a project, using implicit global project config
Using resolver: lts-6.10 from implicit global project's config file: /home/benc/.stack/global-project/stack.yaml
Configuring GHCi with the following packages: 
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
Prelude> putStrLn "hello world!"
hello world!
```

pretty easy. we can do numbers too!


```
Prelude> 6 * 7
42
```

We can also make an executable. We already ran `stack setup` in step 0, hopefully.

Let's start a new project using stack - it is a bit overkill for hello world
but it will be useful later.

```
$ stack new hw simple

$ cd hw
$ ls
hw.cabal  LICENSE  Setup.hs  src  stack.yaml
$ cat src/Main.hs 
module Main where

main :: IO ()
main = do
  putStrLn "hello world"

```

Ignore everything apart from src/Main.hs for now. You can see the template
has made a hello world program automatically with some boilerplate.

We can build it:

```
$ stack build
```

and run it:

```
$ stack exec hw
hello world
```

So what we have is some boilerplate around a command that we could type into ghci.

line by line:

```
module Main where
```

each haskell file defines a "module", and this one is called Main. the filename matches
the module name.

main has two parts to its definition:

```
main :: IO ()
```

The is a type declaration. It says function `main` has type `IO ()`. Types
are one of the most important but most complicated parts of Haskell.  Often
you'll get type errors where other languages might instead give you a
different sort of error - a runtime or syntax error, for example.

Types in Haskell can encode more than simply what is passed into a function
and what is returned by it.

This type, for example, specifies that the main function
retuns nothing - `()` is a bit like
a Haskell void type. But `IO` means that main might do some IO (in this
case, print things)

Reload `stack ghci` and we can look at types interactively:

```
*Main> :t main
main :: IO ()

*Main> :t putStrLn
putStrLn :: String -> IO ()
```

So `putStrLn` takes a string and returns nothing, maybe doing some IO.

We can take the types of arbitrary expressions too:

*Main> :t (putStrLn "hello")
(putStrLn "hello") :: IO ()

That says putStrLn "hello"  doesn't take any arguments (because we've applied the argument it wanted)
and does some IO.

and what's important here? the type of putStrLn "hello" is the same as main.
Otherwise we'd get a type checking error.

For example, you could try compiling 

main = putStrLn

with no parameter and see what error you get.



