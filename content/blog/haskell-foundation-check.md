+++
title = "Foundation's testing framework"
description = "When testing becomes part of your library"
date = 2017-11-30

[extra]
fa = "fa-check-square"
+++

Being a haskell library provider means you have a contract toward your user:
_you provide them with a minimum degree of confidence your code work_.

This is quite important. And thankfully Haskell maintainers are quite good with
that. The language provides the abstraction to avoid the common mistakes;
the semantic is checked at compile time. But what about the implementation
itself? Or the properties? Is it correct? Is your functor actually a functor?
Will it behave like everyone is expecting? Is your JSON parser parsing the data
as expected?

<!-- more -->

There is an Haskell library for that: **Quickcheck**. We all know about it, most
of us are using it. It has been doing the job okay so far.

Now, other languages like **Rust**, have decided to include testing as a core
feature. I believe it is a good idea to include the testing
framework as early as possible in the core library; or within the language
itself.

# Foundation's Check

Some would rather like a more fragmented libraries. At the
[Haskell Foundation](https://github.com/haskell-foundation) we believe it is
better to unify, to bring together core libraries, to facilitate the
standardisation of haskell libraries and have inter operational types.
The [foundation](https://www.stackage.org/package/foundation) unifies modern
Haskell concepts and real word programming. And because
[foundation](https://www.stackage.org/package/foundation) is one of the core
library if not _the_ core library. This is why it comes with a complete
testing framework.

## Foundation's Arbitrary

Foundation's types come with instances of
[`Arbitrary`](https://www.stackage.org/haddock/lts-9.14/foundation-0.0.17/Foundation-Check.html#t:Arbitrary).

Just like you would find in Quickcheck (Foundation does not aim to re-invent
the wheel):

```Haskell
class Arbitrary a where
    arbitrary :: Gen a
```

Having `Arbitrary` available at the foundation of the ecosystem is really
useful and it prevents from seeing the most infamous compilation warning to see:
**Ophan Instance**.
If you are using Quickcheck (and didn't want to see the all its dependencies
added to your package) you have certainly seen this compiler message.

With Foundation's Check you can add `Arbitrary` to your types. Allowing you to
use it in your tests **but** also allowing _the users of your library to re-use
the instance of `Arbitrary` in their own tests_.

## Foundation's test framework DSL

Foundation is not re-inventing the wheel here. It is mostly what you
would have with Quickcheck, except that in Foundation you use the `Test` type
constructor instead of specific functions. In Foundation, it is called
[`Test`](https://www.stackage.org/haddock/lts-9.14/foundation-0.0.17/Foundation-Check.html#t:Test)

```haskell
data Test where
    Group     :: String -> [Test] -> Test
    Property  :: IsProperty prop => String -> prop -> Test
    CheckPlan :: String -> Check () -> Test
```

so we have 3 constructors:

* `Group` will allow you to build some sort of hierarchy within your tests;
* `Property` test any kind of property (see below);
* `CheckPlan` check like `Property` will allow you to validate properties but
  unlike `Property` it allows you to acquire resources first.

### `Group`

Well this is easy:

```haskell
main :: IO ()
main = defaultMain $ Test "Properties"
  [ Test "Functor" []
  , Test "Applicative" []
  , Test "Monad" []
  ]
```

### `Property`

This constructor regroup the 2 different kind of test you will want to do
most of the cases:

* a unit test: test that `convertToBase Base64 "" === ""`
* a property test: test that `convertFromBase Base64 . convertToBase Base64 === id`

And to do that, no need for different constructor, the
[`IsProperty`](https://www.stackage.org/haddock/lts-9.14/foundation-0.0.17/Foundation-Check.html#t:IsProperty)
class will help you.

#### Unit Test

To do a test once, and only once without generating data (unit test):

```haskell
main :: IO ()
main = defaultMain $
  Property "base64 of nothing is nothing"
           (convertToBase Base64 "" == "")
```

The type check understood this is a `Bool` type. The test has no parameter,
it only need to run once.

#### Property Tests

When you want to test a property is true for any arbitrary value of a given type
you will want to write a **Property Test**:

```haskell
main :: IO ()
main = defaultMain $
  Property "unbase64 . base64 == id" $ \d ->
            Right d === convertFromBase Base64 (convertToBase Base64 d)

```

The type checker see that you need a parameter, checks it is an instance of
`Arbitrary` and generate it for you.

Foundation provides handy function and operator. Here `===` will provide you
with a nice little error display in case of something is different.

```
Prime
  Common
    ✗ Time failed after 1 test
       ✗ parseJSON . fromJSON === id failed after 1 test
         use param: --seed 14494984503458014416
parameter 1 : 1999-09-03T06:46:42+00:00
parameter 2 : 1997-02-12T23:17:46+00:00
Property `a == b' failed where:
    a = Right 1999-09-03T06:46:42+00:00 :: Either [Char] Time
                 ^  ^ ^^ ^^ ^^  ^
    b = Right 1997-02-12T23:17:46+00:00 :: Either [Char] Time
                 ^  ^ ^^ ^^ ^^  ^
```

In this case the display is really neat. It won't be perfect all the time but it
can provide a useful hint in case of error. Foundation's Check does not only
fail the test, it provides you with details that can help you same time here.


Below is the code that generated the error above:

```Haskell
main :: IO ()
main = defaultMain $
  Property "fromJSON . toJSON == id" $ \a (b :: Time) ->
      Right a === parseJSON (toJSON b)
```

# That's it

really no magic here. You can check out
[*sharesafe-lib*'s tests](https://github.com/primetype/sharesafe-lib) for
examples or even
[Foundation's tests](https://github.com/haskell-foundation/foundation) for more
examples.

So now, this is not a finished business. Foundation's community is still
building up. But the progress so far are astonishing and it is a pleasure
to use advanced haskell in real world project.
