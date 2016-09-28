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
Prelude> print "hello world!"
"hello world!"
```

pretty easy


```
Prelude> 6 * 7
42
```
