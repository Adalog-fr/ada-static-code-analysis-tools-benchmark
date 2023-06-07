base64 
[![License](https://img.shields.io/github/license/AntonMeep/base64.svg?color=blue)](https://github.com/AntonMeep/base64/blob/master/LICENSE.txt)
[![Alire crate](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/base64.json)](https://alire.ada.dev/crates/base64.html)
[![GitHub release](https://img.shields.io/github/release/AntonMeep/base64.svg)](https://github.com/AntonMeep/base64/releases/latest)
=======

Base64 encoding and decoding routines for Ada. Generic packages are provided to simplify integration into already existing projects where you might already have defined own types to represent bytes and byte arrays.

This implementation does not utilize dynamic memory allocation, therefore amount of data it can process at once is limited by available stack space.
