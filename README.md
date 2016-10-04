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
to adjust the pace:

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

```
*Main> :t putStrLn "hello"
(putStrLn "hello") :: IO ()
```

That says putStrLn "hello"  doesn't take any arguments (because we've applied the argument it wanted)
and does some IO.

and what's important here? the type of 'putStrLn "hello"' is the same as main.
Otherwise we'd get a type checking error.

For example, you could try compiling 

```
main = putStrLn
```

with no parameter and see what error you get.


3. Getting something over HTTP

So that's pretty boring.

Let's do something over the internet.

A library I like for http is [wreq](https://hackage.haskell.org/package/wreq) - there's a tutorial linked from that page if you want to read more later.

```
*Main> import Network.Wreq as WR

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
*Main> import Network.Wreq as WR
*Main Network.Wreq> 
```

This gives us some functions for accessing HTTP. For example:


```
*Main Network.Wreq> WR.get "http://www.google.com/"

...
[massive dump of data]
```

What type is get?

```
*Main Network.Wreq> :t WR.get
WR.get
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
*Main Network.Wreq> r <- WR.get "http://www.reddit.com/r/haskell.json"
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
*Main Network.Wreq Control.Lens> r ^. responseStatus
Status {statusCode = 200, statusMessage = "OK"}
```

or we can dig down a bit further, like this:

```
r ^. responseStatus . statusMessage
```

and we can go further into the response, and dig straight into fields in the JSON, like this:
(with more imports...)

TODO: need overloaded strings here - can introduce language extensions - do the thing, then explain afterwards.

```
*Main Network.Wreq Control.Lens Data.Aeson.Lens Data.Aeson> 
 r ^. responseBody . key "kind" 
```

whoops, it didn't work...

but this does...

```
r ^? responseBody . key "kind" 
Just (String "Listing")
```

so we got this first bit of the response body

```
"{\"kind\": \"Listing\", \"dat ...
```

back.

TODO: note to self, do I want to introduce only ^? at this point, and not ^. ? Swap that complexity
out for introducing Maybe at this point, which is probably better value


Look at the types: the first will *definitely* return a status value

```
:t (r ^. responseStatus)
(r ^. responseStatus) :: Status
```

but this one might maybe will return a status (or maybe will return nothing)

```
:t (r ^? responseStatus)
(r ^? responseStatus) :: Maybe Status
```

so lets get all the posts, as a vector:

```
> r ^. responseBody . key "data" . key "children"  . _Array
```

(this gives us a big pile of stuff still... awkward to read)

but we can say things like:

```
:t r ^. responseBody . key "data" . key "children"  . _Array
(r ^. responseBody . key "data" . key "children"  . _Array)
  :: vector-0.11.0.0:Data.Vector.Vector Value
```

This says this is a "vector" where the contents are (JSON) values.

A vector has the property that we can find out how long it is, like this:

```
> length (r ^. responseBody . key "data" . key "children"  . _Array)
26
```

and see there are 26 posts in the returned data.

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
we already have and computing something purely from those values - that's a *pure computation*
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

```
r <- action
```

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

next
====

where are we?

we have a Vector of posts that we've fetched live from reddit,
and we've counted how many posts have come back. if we look
at any one of those posts, they're still each individually a
big awkward pile of JSON.

We could dig into them with further lenses to pull out
individual fields, such as the title and the author, but
maybe it would be nice to have a proper Haskell data
structure that can represent a post.

Back in ghci, lets define a data type that can store
some information about our post - we'll start with
just two fields, and get bigger later.

We need to add in a new package, 'text' so that we can
refer to the 'Text' type.

```
> import Data.Text as T
```

now we can define a `Post` data type with a couple of
fields. In `ghci` this has to go all on one line.

```
data Post = P { author :: T.Text, title :: T.Text } deriving Show
```

Now we can make Post structures at the ghci prompt:

```
> P "ben" "a post"
P {author = "ben", title = "a post"}

> :t P "ben" "a post"
P "ben" "a post" :: Post
```

So what we want to do is convert that `Vector Value`,
a vector of json values, in `Vector Post`, a vector of our
Posts.

We need two tools to do this:

i) a function that converts a single Value into a
single Post;

and

ii) a function that applies another function to
every position in a vector.

Let's get a JSON 'Value' that contains a post by replacing _Array (which gives us a vector) with `nth 0` with gives us the 0th post:

```
> let post = p ^? responseBody . key "data" . key "children"  . nth 0
> post
[... you should see this is just one post ...]
> :t post
post :: Maybe Value
```

It's a (Maybe Value) - because we might be on a subreddit that has no posts, and this is reflected in the type system! it might not have any post to be the 0th post.


=== functor interlude ===

We have two "containers" of 'Value's here - where the type looks
like *F* Value: Maybe Value and Vector Value - one of them
contains Nothing or Just a single Value; the other can have 0 or
more Values. And we can stick in any type there, for example
'Maybe Integer', or 'Vector T.Text'.

It happens that these are both 'Functor's - which are data
structures where we can apply some function to all of the
"contained" data.

We can apply functions using 'fmap'. An example is
a function (const 1) that always returns 1, no matter what
you pass in.

For example:

```
> (const 1) 5
1
> (const 1) "hello"
1
```

That's a bit useless on its own, but we can say:

```
> length posts
26
> fmap (const 1) posts
[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
> length (fmap (const 1) posts)
26
```

So we've replaced everything in the Vector with the value 1,
but there are still 26 positions, like there were in the
input. We still got a vector out.

Same for a Maybe:

```
> fmap (const 1) Nothing
Nothing
> fmap (const 1) (Just "hello") 
Just 1
```

We still got a Maybe out, but now it's a Maybe with
a number in it, instead of a String.

So with 'fmap' can change the values and the type of
values of the "inside" but we are forced to keep
the "shape" of the outside.

===

So lets consolidate what we have: we've got a structure
of 0 or more JSON posts (type 'Vector Value') - actually
it's 26 posts but we can't tell that from the type;
and we've got a structure that might contain the first
post if it exists, or it might contain Nothing
(type 'Maybe Value'), and now we know if we have a
function that operates on Values, we can apply it to the
insides of either of those structures, using fmap.


So, can we make up a function that we can apply with fmap, that
will turn a Value into a Post? so that we can have a bit
more type safety...

Haskell can do some of this conversion for us, using a feature called
Generics:

Turn on the new language features we need:
```
> :set -XDeriveGeneric
> :set -XDeriveAnyClass
> import GHC.Generics
```

and declare Post again, with more 'deriving' clauses:

```
data Post = P { author :: T.Text, title :: T.Text } deriving (Show, Generic, FromJSON)
```

The 'Generics' bit is some necessary plumbing, but the
'FromJSON' bit says we want the aeson package to figure out
how to parse JSON into a Post structure for us by matching
up field titles.

This won't work with the JSON values we've got because
they have too much wrapping. We'll need to unwrap one more
layer to get rid of this "type"/"data" stuff and get to the
fields we want.

This is a longer definition so lets turn on multiline mode:

Pay attention to the indentation here!

```
> :set +m
> let unwrap v = case (v ^? key "data") of
>       Nothing -> error "no data"
>       Just x -> x
> <newline to end>
```

So now we can use this on post or posts, to unwrap one or all of the
posts:

```
> fmap unwrap post
> fmap unwrap posts
```


and we can use 'fromJSON' alongside unwrap, like this:

```
> fmap (fromJSON . unwrap) post :: Maybe (Result Post)
```

So (fromJSON . unwrap) is function composition: it
puts unwrap and fromJSON together and makes them into
a single function - we can use that wherever we can
use a function, eg as the parameter to fmap.

This is a difference from some more traditional languages
where all functions have names - in haskell you can make
up functions from building blocks.

The other thing here is we have to say 'Maybe (Result Post)'
because ghci doesn't know what you want to convert
the JSON into -- sometimes it can figure out the types but
not always.

Let's get Data.Vector imported and we can convert all of
'posts' ... add 'vector' as a dependency in hw.cabal.

Probably at this point we should get more of our definitions
put into our source file. (TODO)


Note that FromJSON wraps results up in a Result type - 
it returns, not a Post, but a Result Post. This is
defined as being either a "Success Post" - indicating
that we've successfully got a post value; or
an 'Error String' indicating that we've got an error,
and carrying along an error message.

This is how it is defined in the aeson library
- a bit like our earlier data definition, but 
using '|' to represent alternatives - we can
pick either to have an Error or a Success.

```
data Result a = Error String | Success a 
```

So this is a bit like a Maybe value - it can represent
that there's is a value, or that "something went wrong"
so a Success is like the 'Just' case of Maybe. But the
Nothing equivalent, Error, is richer.

A pretty common pattern in haskell libraries is this
either a success or some error, and use 'case' as
before to choose between the behaviour when we
succeed or fail.

Now for all of our posts, we've got this sort of awkward
structure -- we have a vector, and that vector contains
some Successes with associated Posts and some Errors without
a post...

Let's have another syntactic form, then - comprehensions.
If you've used list comprehensions in python, these are
very similar to that.

```
> :set -XMonadComprehensions

> let ps = [p | x <- posts, (Success p) <- return $ (fromJSON . unwrap) x] :: Vector Post
<<newline>>
> :t ps
ps :: Vector Post
```

We can do a pattern match here so that only things matching Success get matched... the Errors disappear into the void.

(It turns out you can use this syntax in the same places you can use
'do' notation - this is another way to use the infamous monads, and
also create similar effects to fmap... that doesn't matter here but
there is a connection underneath)

Now we have a nice vector of just the valid posts. When we print it,
it's all still pretty hard to read though. Maybe we can do something
better...

```
for ps $ \p -> print p
```

That gives us one per line... it goes over each element in the post
vector, and uses 'print' that we've seen before on that post.
(we can print Posts because Post is an instance of Show)

We can make this prettier, by making our own print routine that
specially knows about printing posts:

```
let printPost :: Post -> IO ()
    printPost p = do
      print (title p)
      putStr "    by "
      print (author p)

for ps $ \p -> printPost p
[... output ...]
```

Check r/haskell and see if the posts match up... they should.

Now get this put in the file so that when you run the
main program, it pretty-prints a list of post titles/authors.

Next add in some more fields (perhaps url? number of votes?)
making sure to choose sensible types - most likely
Text and Integer.

As long as the field names match up between 'data Post' and
the reddit JSON, the fields will automatically become
available thanks to generics.


TODO: insert a summary conclusion of topics covered so far

end of part 1.

part 2
======

We're successfully accessing reddit in a read-only mode now,
using a small pile of different haskell libraries.

What can we do next? The real bot this tutorial is based of
processes the titles of posts, and makes calls to reddit
after authenticating.



{- TODO 
so: have a look at what is inside the post structure -
extract title, author and permalink,
date and votes
perhaps?

do this using a data definition. maybe we'll have described
Maybe earlier on?

this is a bit more the traditional "functional programming"
bread and butter.
note that most of these can happen as pure functions,
aside from printing - comment that a way of structuring
code is you try to keep as much out of IO as possible.

filter on author, or on title

sort by votes; sort by date; print the top 3 of each.
iterate using 'for' to print)

make functions: printBest ps, printRecent ps
(use 'ps' and note idiomatic style of 'p' for post, 'ps' for
several posts)

-}


-- TODO: some data structures?

-- TODO: typeclasses / abstractions over type?
