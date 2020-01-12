+++
title = "Cardano Wallet everywhere"
description = "The output of wasm-pack on IOHK's cardano wallet"
date = 2019-02-25

[extra]
fa = "fa-universal-access"
+++

It has been a while since my last blog post here. I have been working on a new
project for a blockchain company [IOHK](https://iohk.io). My team has been
working on the project to broader the customer targets by allowing the
development of 3rd party software to work on many different platforms.

<!-- more -->

How would you do that? First you will need to make the list of what you need
and then you see what can fit these requirements.

# Why we chose Rust

The list of requirements was simple:

1. We need to be able to control the memory footprint of the applications;
2. it needs to work on small embedded device;
3. it needs to work in the browser on a cheap computer;
4. it needs to leverage the platforms features to provide the best performances;
5. developers and integrators need not to have a hard time utilising it.

Initially the cardano protocol is written in haskell. It is a cute language
with really interesting language property. Some may argue that
it is possible to make it fit all the requirements above, but there is
still the requirement number **5.** : it needs to be easy to integrate in
the developer environment.

So we took rust. It is a system programming language, you can control the
memory footprint of your software, you can cross compile to so much
different platform (including wasm, intel, powerpc, arm...). You can find
a lot of reasons
[here](https://medium.com/mozilla-tech/why-rust-is-the-most-loved-language-by-developers-666add782563),
[there](https://www.reddit.com/r/rust/comments/4l44z3/why_should_i_use_rust/)
or [there](https://medium.com/paritytech/why-rust-846fd3320d3f).

# One core language to support them all

Not everything will be written in Rust. It is a common mistake to believe that
there can be one language above them all. While Rust is very powerful and
has a rich ecosystem, it is still easier to create an iOS mobile app in native
Swift, an android mobile app in Java or a WebApp in JavaScript. Where Rust is
great is that it can easily integrate in these languages.

One recent project is the [`rust-cardano`](https://github.com/input-output-hk/rust-cardano)
project. Most of the code is written in Rust. Yet it was easy and cheap to
generate a [C library](https://github.com/input-output-hk/rust-cardano/tree/master/cardano-c)
or [JavaScript bindings](https://github.com/input-output-hk/js-cardano-wasm).

This is the same JS Bindings I talk about in [this blog post](@/blog/cardano-wallet.md)
