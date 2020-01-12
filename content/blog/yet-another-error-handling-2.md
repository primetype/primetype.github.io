+++
title = "C++: Yet Another Kind of error handling (Part 2)"
description = "Self document your code and let users decide how to handle error"
date = 2016-06-29

[extra]
fa = "fa-car-crash"
+++

In a [previous post](@/blog/yet-another-error-handling.md) I
demonstrated how handy error handling in **Haskell** and in **Rust** is.

It is possible to do the same in **C++**.

Too often I come along the problem of error handling in **C++**. Some believe
we *must* use `exceptions`, others prefer the old **C** style error code.

I believe both are good options and have valid concerns about the other. I
believe we **should** do both.

<!-- more -->

# Concrete examples

## C's free function

If you were to open a file, in **C** you would use the following function:

```C
int open(char const* path, int oflag, ...);
```

`open` returns either `-1` **or** a valid _file descriptor_. If you have an
error and you want to find out what happened you will need to check `errno`
then try to guess if `EACCES` is the one you are concerned about.

It is not easy to use but it has been working for the last couple of decades.

## C++'s Standard library

**C++** comes with some wrapper around the **C** function `open`. There is a
*class* `fstream` which provides the method
[open](http://www.cplusplus.com/reference/fstream/fstream/open/).

```C++
void open (const char* filename,
           ios_base::openmode mode = ios_base::in | ios_base::out);
```

If the function has failed, you will need to look at the
[ios::good](http://www.cplusplus.com/reference/ios/ios/good/) to find out if the
function succeeded or otherwise the opened `fstream` would throw an exception on
the next operation, _fair enough_. The best practice is clearly to check for
`ios::good` function.

# Can't we do better ?

I think we can do much better. Ideally, we would like the signature of the
function to be self explanatory, to describe precisely what users of this
function should expect.

## Boost

[Boost](//boost.org) provides a data structure: `boost::variant`. It is an
enhanced version of the `C`'s `unions`, with strong typing.

```c++
// you can create an object which is either an `int` or a `std::string`
boost::variant<int, std::string> obj = 10;

// you can get it's content knowing its type
int v = boost::get<int>(obj); // OK

// you can mutate the content
obj = "Prime Type Ltd"; // Ok

// and access its value with its type
std::string p = boost::get<std::string>(obj); // OK

// but you cannot get it wrong:
boost::get<int>(obj); // obj has been set to a std::string...
```

This is really similar to what I described in my
[previous post](2016-06-11-yet-another-error-handling.html).

## The idea

We will wrap `boost::variant` in a structure and provide **Rust**-like
method to map the content of the value, chain with other function etc...

```c++
template<class result_type, class error_type>
struct Result {
  boost::variant<result_type, error_type> variant_;
};
```

And then we could do the following:

```C++
Result<std::fstream, std::runtime_error> open( char const* filename
                                             , ios_base::openmode mode = ios_base::in
                                                                       | ios_base::out
                                             );
```

> This is it !

We now have a function, which explicitly tells us what the function is doing.
It is a function which `open` the file, takes a `filename` and `mode`, and
returns either the successfully opened `fstream` or a `runtime_error`.

## The `Result`

You can find the complete source code with the test
[on github](//github.com/nicolasdp/result).

## Example of usage

Now we can do pretty interesting thing with this:

We can throw an exception if we don't want to handle the error right now:

```C++
// either open succeed or throw the `std::runtime_error`
auto fs = open("/invalid/file.txt").unwrap();
```

Or we can recover:

```C++
// if we fail to open the file we would execute the given lambda.
auto fs = open("/invalid/file.txt")
            .or_else([](std::runtime_error&& )
              {
                return std::move(open("/valid/file.txt"));
              }
            );
```

Or even more complex yet interesting chaining:

```C++
std::string line = open("example.txt")
  .and_then([](std::fstream&& fs) { return readline(fs); })
  .and_then([](std::string&& s) { return parseline(s); })
  .or_else([](std::runtime_error&&) {return open("test.txt")})
  .and_then([](std::fstream&& fs) { return readline(fs); })
  .and_then([](std::string&& s) { return parseline(s); })
  .expect("cannot parse line of both example.txt and test.txt");
```
