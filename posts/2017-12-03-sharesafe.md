---
title: ShareSafe -- ready to use secret sharing
description: Command line interface for Publicly Verifiable Secret Sharing
author: Nicolas Di Prima
---

# TL;DR

[`ShareSafe`](https://github.com/primetype/sharesafe-lib) is available and
ready to use. It allows users to leverage Publicly Verifiable Secret Sharing
(PVSS).

## What it is

It is a library (first at all) that provides simple interfaces to make use of
PVSS to encrypt/decrypt data.

A command line interface (CLI) is provided with the package and it is what we
will be talking about here.

## With what

The library is built on top of the [`pvss`](https://github.com/input-output-hk/pvss-haskell)
library built and maintained by [**IOHK**](https://iohk.io) (the company behind
cardano-sl and the cryptocurrency **ADA**) and on top of
[`cryptonite`](https://github.com/haskell-crypto/cryptonite).

# sharesafe: simple secret sharing

## What is Publicly Verifiable Secret Sharing

Quickly, PVSS is a protocol to allow users to create (and verify) a secret made
by `n` participants. These `n` participants only shares their `n` public keys
to create a secret. They have decided that `m` participants (0 < `m` <= `n`)
are needed to retrieve the Secret.

Very simple : `n` participants only share their public keys. All or a subset are
needed to recover the secret.

## What's with `sharesafe` ?

The generated/retrieved secret can be used for multiple purpose. One of them,
the one used in `sharesafe`, is for symmetric encryption.

i.e. if one want to send private data, it only needs the public key of the
recipient. So now you can say that there is already asymmetric encryption for
that and you would be right. PVSS here is interesting because you can add
yourself in the scheme. The sender can set itself as a participant of share
and can retrieve the **same** secret.

This is interesting because it provides us with a way to verify who participated
to a given secret/share.

You can checkout the use case described in the [README.md of `sharesafe`](https://github.com/primetype/sharesafe-lib#sharing-deployment-files-on-repositories)

## How to use the CLI to generate private key

### create the keypair and share the public key

```bash
# generate to the standard output
sharesafe key new
# generate in a file
sharesafe key new -o <generated-key-pair>
```

by default we are not setting a password for the secret. If you want to set a
password, use `--password`. By default the password is the empty string.

To export the public key:

```bash
# by default `sharesafe` will read the keypair from the standard input
# and will write the exported public key to the standard output.
sharesafe key export-public -i <generated-key-pair> -o <exported-public-key>
```

### Create an encryption key

This command will create the shares for every participants, the commitments and
the encryption key.

* the `shares` will be needed to recover the secret later on. Only the secret
  key associated to the share can unlock the share;
* you will need the same amount as the `threshold`  of unlocked shares to
  recover the secret;
* the `commitments` can be used to check the shares;
* `encryption-key` is the key generated from the secret;

This command will create a locked `share` for every given `<public-key>`: the
name will be the input file name with the suffix replaced to `share`.

```bash
sharesafe pvss new --participant public-key1 \
                   --participant public-key2 \
                   --threshold=1 \
                   --commitments commitments \
                   -o encryption-key
```

Once you have generated the shares, you can simply share them Publicly (or not).
Only the private key associated to the public key used to generate the share
can unlock the share.

### Verify a share against the commitments

```bash
sharesafe pvss verify -s <locked-share> -c commitments
```

### Unlock the share

```bash
sharesafe pvss open-share -s <share-file> -k <private-key> -o <opened-share>
```

### Recover the encryption key

```bash
sharesafe pvss recover -s $(cat <opened-share>) \
                       -o encryption-key.recovered
```

## How to use the CLI for encryption

The `encryption-key` is generated using Diffie-Hellman derivation from the
PVSS secret. The encryption algorithm provided is `ChaChaPoly1305`.
It provides a default authentication of the ciphered content.

### Encrypt

```bash
cat input-file | \
  sharesafe cipher encrypt -k $(cat encryption-key) > input-file.encrypted
```

### Decrypt

```bash
cat input-file.encrypted | \
  sharesafe cipher decrypt -k $(cat encryption-key) > input-file
```
