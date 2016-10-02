0. Before the start

TODO: get some kind of stack hello world.
TODO: get a reddit account

1. intro:

i'm going to talk us through building a bot that talks to reddit. you can
follow along on your laptop. I'm not going to dig too much into the
exact rules for what is going on or the exact details of the libraries
that we're going to use, but hopefully you'll have a working
program which which to experiment which, or go read about later
(or if we have time, I can explain some bits in more depth).

Also, you'll find things introduced in an order which works for the
tutorial, which is not at all like the order you'd see things in
a haskell textbook... and I'll miss out things that I didn't need that
are pretty useful in haskell

It's based on a real life bot that works on r/LondonSocialClub.

So this is going to feel very in-at-the-deep-end.

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


3. Getting something over HTTP

So that's pretty boring.

Let's do something over the internet.

A library I like for http is [wreq](https://hackage.haskell.org/package/wreq) - there's a tutorial linked from that page if you want to read more later.

```
*Main> import Network.Wreq

<no location info>: error:
    Could not find module ‘Network.Wreq’
    It is not a module in the current program, or in any known package.
*Main> 
```

edit hw.cabal, the `build-depends:` line:

```
  build-depends:       base >= 4.7 && < 5, wreq
```

and when you run `stack ghci` again, we will have it available.
stack will download anything it needs to make wreq available.
(note to self: might install these as part of the
ch0 setup so that we have them locally)

and now we can:

```
*Main> import Network.Wreq
*Main Network.Wreq> 
```

This gives us some functions for accessing HTTP. For example:


```
*Main Network.Wreq> get "http://www.google.com/"

...
[massive dump of data]
```

What type is get?

```
*Main Network.Wreq> :t get
get
  :: String
     -> IO
          (Response
             bytestring-0.10.8.1:Data.ByteString.Lazy.Internal.ByteString)
```

We feed in a string, and this will do some IO and return a Reponse.

Types can have parameters too! in this case, this long ByteString type. We'll ignore that bit for now...

Let's talk to reddit, and ask it for JSON, and
let's store the response in a variable so that we can access it later:

```
*Main Network.Wreq>  r <- get "http://www.reddit.com/r/haskell.json"
*Main Network.Wreq> :t r
r :: Response
       bytestring-0.10.8.1:Data.ByteString.Lazy.Internal.ByteString
```

(and if you type just `r` you'll get a big dump again)

You can load [that url](http://www.reddit.com/r/haskell.json) in your browser, and compare it with
the reddit [human readable page](http://www.reddit.com/r/haskell).

Now we can see we have a big data structure - we can use some other libraries to dig into it.
So add these as dependencies in `hw.cabal` just like you did for wreq:
`lens` and `aeson` and `lens-aeson` and go back into ghci.

```
*Main> import Network.Wreq
*Main Network.Wreq> import Control.Lens
```

`lens` provides a way to focus on a portion of a Haskell value - for example fields in a data structure.
`aeson` is a JSON library.

We can use `lens` to ask for the response status, stored in the Response value:

```
*Main Network.Wreq Control.Lens> r <- get "http://www.reddit.com/.json"
*Main Network.Wreq Control.Lens> r ^. responseStatus
Status {statusCode = 200, statusMessage = "OK"}
```

or we can dig down a bit further, like this:

r ^. responseStatus . statusMessage

and we can go further into the response, and dig straight into fields in the JSON, like this:
(with more imports...)

TODO: need overloaded strings here - can introduce language extensions - do the thing, then explain afterwards.


*Main Network.Wreq Control.Lens Data.Aeson.Lens Data.Aeson> 
 r ^. responseBody . key "kind" 

whoops, it didn't work...

but this does...

r ^? responseBody . key "kind" 
Just (String "Listing")

so we got this first bit of the response body

```
"{\"kind\": \"Listing\", \"dat ...
```

back.

TODO: note to self, do I want to introduce only ^? at this point, and not ^. ? Swap that complexity
out for introducing Maybe at this point, which is probably better value


Look at the types: the first will *definitely* return a status value
:t (r ^. responseStatus)
(r ^. responseStatus) :: Status

but this one might maybe will return a status (or maybe will return nothing)
:t (r ^? responseStatus)
(r ^? responseStatus) :: Maybe Status

(in fact it always will in this case - which is why ^. works... but this is a bit beyond
introductory...)

so lets get all the posts, as a vector:

> r ^. responseBody . key "data" . key "children"  . _Array

(this gives us a big pile of stuff still... awkward to read)

but we can say things like:

:t r ^. responseBody . key "data" . key "children"  . _Array
(r ^. responseBody . key "data" . key "children"  . _Array)
  :: vector-0.11.0.0:Data.Vector.Vector Value

This says this is a "vector" where the contents are (JSON) values.

A vector has the property that we can find out how long it is, like this:

> length (r ^. responseBody . key "data" . key "children"  . _Array)
25

and see there are 25 posts in the returned data.

'length' is another standard Haskell function, that will tell you how big collections of data are (there is a more formal definition, that it gives you the length of 'Foldable' structures - but we won't address exactly what that means here)

The standard type you'll see in haskell tutorials is a list, not a Vector is what the aeson lens library happens to return. You can call 'length' on lists too, like this:

```
> length [6,7,8]
3
```

We can assign the posts to a variable, like this:

```
let posts = r ^. responseBody . key "data" . key "children"  . _Array
:t posts
length posts
25

```

We use 'let x = y' here not 'x <- y'. Why?

Well this way usually lies the rathole of attempting to explain monads... which I'm not going to really try to do.

Let's look at the types:

```
> :t r ^. responseBody . key "data" . key "children"   . _Array
r ^. responseBody . key "data" . key "children"   . _Array
  :: vector-0.11.0.0:Data.Vector.Vector Value
*Main Control.Lens Data.Aeson.Lens Network.Wreq Data.Aeson> :t posts
posts :: vector-0.11.0.0:Data.Vector.Vector Value
```

posts has the same type as the expression on the right side - that's variable assignment that looks fairly
normal compared to other programming languages. We aren't doing anything apart from taking some values
we already have and computing something purely from those values - that's a *pure computation@
in Haskell. If we run it lots of times, or on different computers, or tomorrow, the result
will (should) always be the same.

but go back to when we made an HTTP request.

That isn't a pure value - whenever we make a request, we expect to get whatever is on reddit right now.
The whole point of it is that - and tomorrow when we run it, we absolutely expect that we'll
get a different answer.

That's reflected in the type system, like we saw at the start.

```
>  r <- get "http://www.reddit.com/r/haskell.json"

> :t get "http://www.reddit.com/r/haskell.json"
get "http://www.reddit.com/r/haskell.json"
  :: IO
       (Response
          bytestring-0.10.6.0:Data.ByteString.Lazy.Internal.ByteString)

> :t r
r :: Response
       bytestring-0.10.6.0:Data.ByteString.Lazy.Internal.ByteString

```

`IO a` means this an action that knows how to do some IO and return a value of type 'a',
and the syntax

r <- action

means to actually run that action, and assign the returned value.

'IO' is a bit of a bag of dirty laundry(?) for Haskell - we spend a lot
of time talking about functional purity and all sorts of benefits that
arise from it, but then sometimes just to be practical, you can shove
all sorts into 'IO'. But try to avoid it if you can, because it limits
your ability to reason about what the code will actually do.

When people talk about monads, they're basically talking about
stuff you can use with this kind of syntax. 'IO' is an example, but
there is a range of different ones that capture different effects,
perhaps more nuanced than IO, or perhaps not possible in IO.


Let's consolidate everything so into our hello-world program, to give this output:

```
$ stack exec hw
Counting posts
Response status:
Status {statusCode = 200, statusMessage = "OK"}
Number of posts:
25

```


You can try doing it yourself, or look at the program below.


Two things to note are the use of 'print' to print out things that the
'ghci' command line prints for us already, and how to specify the
OverloadedStrings language extension.

```
$ cat src/Main.hs 
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens 
import Network.Wreq
import Data.Aeson
import Data.Aeson.Lens

main :: IO ()
main = do
  putStrLn "Counting posts"
  r <- get "http://www.reddit.com/.json"
  putStrLn "Response status:"
  print (r ^. responseStatus)
  let posts = r ^. responseBody . key "data" . key "children"  . _Array
  putStrLn "Number of posts:"
  print (length posts)
```



-- TODO: some data structures?

-- TODO: typeclasses / abstractions over type?
