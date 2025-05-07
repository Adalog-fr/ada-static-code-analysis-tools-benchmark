hmac 
[![License](https://img.shields.io/github/license/AntonMeep/hmac.svg?color=blue)](https://github.com/AntonMeep/hmac/blob/master/LICENSE.txt)
[![Alire crate](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/hmac.json)](https://alire.ada.dev/crates/hmac.html)
[![GitHub release](https://img.shields.io/github/release/AntonMeep/hmac.svg)](https://github.com/AntonMeep/hmac/releases/latest)
=======

HMAC implemented in Ada, no external dependencies. For the
ease of use, both generic interface and an Ada.Streams-compatible one are
provided. Implementation has been tested against RFC 2202 and RFC 4231
test vectors.

This crate allows for easy set up of HMAC with your hashing functions, most
used HMAC-SHA-1, HMAC-SHA-224, HMAC-SHA-256, HMAC-SHA-384, HMAC-SHA-512, are
already prepared for you.
