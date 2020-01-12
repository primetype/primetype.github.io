+++
title = "The Cardano Wallet"
description = "The output of wasm-pack on IOHK's cardano wallet"
date = 2019-03-01

[extra]
fa = "fa-wallet"
+++

I am so proud to announce the `1.0.0` release of
[cardano-wallet](https://www.npmjs.com/package/cardano-wallet). A fully
object oriented **JavaScript** Library built on top of the
[**Cardano SDK**](https://github.com/input-output-hk/rust-cardano).

<!-- more -->

The Rust Team at IOHK has been working hard to finally provide a stable and
meaningful APIs for developers to write their own wallet applications. Now
there is a package that is object oriented, that has better error inputs and
that have amazing performances even though it has to translate some values
between JavaScript and Rust.

# What is it about

It all started with the [Icarus](https://iohk.io/blog/iohk-release-icarus-to-the-cardano-community/)
project last year. At the time we wrote a first version of the crypto graphic
library in rust and provided hand-written bindings from rust to JS. While it
worked well to a certain point it was not a great API to utilise.

Before you would have to do this:

```JS
const CardanoCrypto = require('OldCardanoCrypto.js');

const SEED = new Uint8Array([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,]);
const root_xprv = CardanoCrypto.HdWallet.fromSeed(seed);
const xpub = CardanoCrypto.HdWallet.toPublic(xprv);
const known_address = CardanoCrypto.HdWallet.publicKeyToAddress(xpub);
```

While very _raw_ it also comes without type script annotations.

Now you can:

```js
const MNEMONICS = "crowd captain hungry tray powder motor coast oppose month shed parent mystery torch resemble index";
const PASSWORD = "Cardano Rust for the winners!";

let settings = Wallet.BlockchainSettings.mainnet();

let entropy = Wallet.Entropy.from_english_mnemonics(MNEMONICS);
let wallet = Wallet.Bip44RootPrivateKey.recover(entropy, PASSWORD);

let account = wallet.bip44_account(Wallet.AccountIndex.new(0 | 0x80000000));
let account_public = account.public();

let key_prv = account.address_key(false, Wallet.AddressKeyIndex.new(0));
let key_pub = account_public.address_key(false, Wallet.AddressKeyIndex.new(0));

let address = key_pub.bootstrap_era_address(settings);
```
