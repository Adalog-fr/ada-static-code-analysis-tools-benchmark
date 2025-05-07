# Introduction

Ada Keystore is a library and tool to store information in secure wallets
and protect the stored information by encrypting the content.
It is necessary to know one of the wallet password to access its content.
Ada Keystore can be used to safely store passwords, credentials,
bank accounts and even documents.

Wallets are protected by a master key using AES-256 and the wallet
master key is protected by a user password.
The wallet defines up to 7 slots that identify
a password key that is able to unlock the master key.  To open a wallet,
it is necessary to unlock one of these 7 slots by providing the correct
password.  Wallet key slots are protected by the user's password
and the PBKDF2-HMAC-256 algorithm, a random salt, a random counter
and they are encrypted using AES-256.

Values stored in the wallet are protected by their own encryption keys
using AES-256.  A wallet can contain another wallet which is then
protected by its own encryption keys and passwords (with 7 independent slots).
Because the child wallet has its own master key, it is necessary to known
the primary password and the child password to unlock the parent wallet
first and then the child wallet.

![AKT Overview](images/akt-overview.png)

The data is organized in 4K blocks whose primary content is encrypted
either by the wallet master key or by the entry keys.  The data block is
signed by using HMAC-256.  A data block can contain several values but
each of them is protected by its own encryption key.  Each value is also
signed using HMAC-256.

The keystore uses several encryption keys at different levels to protect
the content.  A document stored in the keystore is split in data fragment
and each data fragment is encrypted by using its own key.
The data fragments are stored in specific data blocks so that they
are physically separated from the encryption keys.

The data fragment encryption keys are stored in the directory blocks
and they are encrypted by using a specific key.

![AKT Keys](images/akt-keys.png)

This document describes how to build the tool and library and how you can use
the different features to protect your sensitive data.

