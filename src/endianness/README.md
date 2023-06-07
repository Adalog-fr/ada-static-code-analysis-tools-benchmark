endianness 
[![License](https://img.shields.io/github/license/AntonMeep/endianness.svg?color=blue)](https://github.com/AntonMeep/endianness/blob/master/LICENSE.txt)
[![Alire crate](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/endianness.json)](https://alire.ada.dev/crates/endianness.html)
[![GitHub release](https://img.shields.io/github/release/AntonMeep/endianness.svg)](https://github.com/AntonMeep/endianness/releases/latest)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/AntonMeep/endianness/Default)](https://github.com/AntonMeep/endianness/actions)
=======

**endianness** is a set of convenience subprograms for Ada, that allow for easy
conversion between big/little endianness. This package provides generic interfaces
to simplify integration with own types. Please note that only types, whose
`T'Size` is equal to 8, 16, 32, or 64, are supported. Because of that it is
recommended to use this crate in pair with standard `Interfaces` package and its
defined `Integer_*`, `Unsigned_*` types.

# Usage

```ada
with Interfaces; use Interfaces;

with Endianness; use Endianness;

procedure Main is
    function Swap is new Swap_Endian (Integer_32);
    function To_Big is new Native_To_Big_Endian (Unsigned_64);
    function To_Little is new Native_To_Little_Endian (Unsigned_64);

    A : aliased Integer_32 := 16#000000FF#;
begin
    A := Swap (A); --  Now A is equal to 16#FF000000#

    To_Big (Unsigned_64 (16#12345678ABCDEF00#));
    --  Yields an array of 16#12#, 16#34#, 16#56#, etc.

    To_Little (Unsigned_64 (16#12345678ABCDEF00#));
    --  Yields an array of 16#00#, 16#EF#, 16#CD#, etc.
end Main;
```
