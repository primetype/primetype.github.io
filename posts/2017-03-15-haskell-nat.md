---
title: Haskell's Type Literal
description: A concrete problem solved with Type Literals
author: Nicolas Di Prima
---

I came across a problem that needed to use Haskell's
[Type Literals](//www.stackage.org/haddock/lts-8.5/base-4.9.1.0/GHC-TypeLits.html).

Haskell's _Type Literals_ enable a new world of possibility in APIs and help
solve day to day developer issues.

In a recent project, we needed to generate a `Blake2` hash of 8 bytes to fit in the
customer's protocol. Unfortunately, there was no straight forward way to
generate a Blake2 digest of 64 bits. The work around could be:

```haskell
blakeb_160 :: Data -> Digest (Blake2b_160)

hash64 :: Data -> Bytes
hash64 = take 8 . toBytes . blakeb_160
```

This is will do, but it loses a lot of information. I would have to create my
own data type to wrap the digest of 8 bytes in order to give it the necessary
properties. **But** this is Haskell, we can do better.

## Blake2

Blake2b is a cryptographic hash (with
[interesting properties](//tools.ietf.org/html/rfc7693)). One of them is that
it can generate Digests from 8 bits long to 512bits long, providing it is
divisible by 8 ; i.e. digests of 1 byte to 64 bytes.

Cryptonite legacy APIs provide a [set of known digests](https://www.stackage.org/haddock/lts-8.5/cryptonite-0.21/Crypto-Hash-Algorithms.html#t:Blake2b_512)
but ideally, we would like to enable every digest size possible, without
having to create 64 different type.

## TL;DR

You can have a look at the
[PR in cryptonite](//github.com/haskell-crypto/cryptonite/pull/140).

## The other languages

### Rust

In **Rust** we would create a generic struct.

```rust
struct Digest<S: usize>([u8;S]);

impl Digest<S: usize> {
  fn generate(r: &mut Reader) -> Digest<S> {
    // ...
  }
}
```

And to check the validity of the given `S`. We could use
[typenum](http://paholg.com/typenum/typenum/index.html) to perform validity
check at compile time, at the type level.

### C++

In **C++** we would use templates and some `static_assert`:

```c++
template<size_t S>
struct Digest {
  // const expression evaluation is already builtin of C++
  static_assert(8 <= S && S <= 512, "invalid digest size");
  std::array<uint8_t, S / 8> digest;
};
```

## Haskell

Haskell comes with a brilliant idea, equivalent to the **C++** solution (to
a certain extent), the **Type Literals**.

```haskell
-- the type we are interested about today
data Nat :: *
-- a class to limit our type literal to
class KnownNat n
```

It allows you to write literals in your types and to
retrieve the value at runtime.

So in our case we could write:

```haskell
-- we can use the natural like we would use a type parameter.
data Blake2b (n :: Nat) = Blake2b

-- we can add conditions
class (KnownNat bitlen, 8 <= bitlen, bitlen <= 512) => Algorithm (Blake2b bitlen) where
    -- ...
```

**GHC** will fail to compile any code making use of the `Blake2b 1024` (or any
other invalid value).

The only downside is that it is still missing a proper clean error message to
help developers. This is still possible. Since `ghc 8.0` we can define a
type error.

```haskell
-- we define a new type operator that will call a helper type family
type IsAtMost (bitlen :: Nat) (n :: Nat) = IsLE bitlen n (bitlen <=? n) ~ 'True

type family IsLE (bitlen :: Nat) (n :: Nat) (c :: Bool) where
    IsLE bitlen n 'True  = 'True
    IsLE bitlen n 'False = TypeError
      (     ('Text "bitlen " ':<>: 'ShowType bitlen ':<>: 'Text " is greater than " ':<>: 'ShowType n)
      ':$$: ('Text "You have tried to use an invalid Digest size. Please, refer to the documentation.")
      )
```

Now, using an invalid literal would make **GHC** fail to compile with the
following error message:

```
~nicolas@primetype/haskell-crypto/cryptonite/tests/Hash.hs:202:23: error:
    • bitlen 1024 is greater than 512
      You have tried to use an invalid Digest size. Please, refer to the documentation.
    • In the expression: HashAlg (Blake2b :: Blake2b 1024)
      ...
```
