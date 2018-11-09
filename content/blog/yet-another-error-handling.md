+++
title = "C++: Yet Another Kind of error handling (Part 1)"
description = "Self document your code and let users decide how to handle error"
date = 2016-06-11

[extra]
fa = "fa-car-crash"
+++

I have been a C++ developer for a little while, I have seen different methods
to provide meaningful API:

* the **C-like**: using error code and returning output in the parameter;
* the **exception-like**: using exceptions.

The difficulties are to:

* guarantee that the error handling is documented;
* ensure it does not change (requires a lot of regression tests);
* keep undefined behaviours as "exceptional".

That's it: exceptions are for exceptional situations. And again, it is not
easy to provide up to date documentation of the kind of exceptions
your function may throw.

So, let's try something else. Let's see what other languages have done to
try to solve the issue of non self documented APIs.

<!-- more -->

# Haskell

Certainly my favourite language. In Haskell there are different methods
to return error information to the user. The simplest
is **Either** or **Maybe**.

```haskell
-- define an error type
data Error = IOError
           | UTF8Error

-- just provide the Function signature to show you how
-- self documented it is
readFile :: FilePath -> Either Error Text
```

So the function is named: **readFile**, it takes a **FilePath** as parameter
and it returns either an **Error** or a **Text**.

This design provides you with some information about the kind of error handled
by this function. I don't say this function won't throw an exception: but the
signature of the function tells you what you can expect.

Now, you know how you can use it:

**If you want to interrupt your program now:**

```haskell
main = do
   case readFile "document.txt" of
      Left err -> error $ "error in readFile: " ++ show err
      Right txt -> printLn txt
```

**If you want to fail quietly**

```haskell
main = do
   either
      (return ())
      print
      (readFile "document.txt")
```

# Rust

The latest language I have been looking into. Full of
[promise](http://groveronline.com/2016/06/why-rust-for-low-level-linux-programming/)
for the System Developers.

The error management is also very simple and self documented:
[Result](https://doc.rust-lang.org/std/result/enum.Result.html).

```rust
enum Error {
  IOError,
  UTF8Error
}

fn readfile(f: Path) -> Result<Error, String>;
```

And now you can, the same way you would do in Haskell:

**throw the error**

```rust
// unwrap() will throw an exception (panic!)
let txt = readFile("document.txt").unwrap();
println!("{}", txt);
```

**or fail quietly**

```rust
// only execute the lamba in the function succeed
readFile("document.txt")
  .and_then(|txt| println!("{}", txt));
```

# Conclusion

Personally, I like these kind of APIs. They provide a typed error
code and users have the choice to handle errors the way they want.
I believe it should be possible to do the same in C++.
In a second post, I will try to implement a **Rust** like *Result* in C++11.
