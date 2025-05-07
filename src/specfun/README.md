[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/specfun.json)](https://alire.ada.dev/crates/specfun.html)

# What is this?

This is a small _native_ Ada library with the most common special functions (Gamma, Beta, Incomplete Beta, etc.).  As funny as it may seems, at the best of my knowledge there is no standard Ada library with that.

Although it was possible to write a binding to a FORTRAN library, I decided for the longer path of translating the FORTRAN code to Ada in order to make the librry self-contained without any external dependence. Translating turned out less "automatic" than what I expected mostly because the original code was in old FORTRAN with just `DO` loops and `IF ... GOTO ...` control structures.

The code has been examined with SPARK, but proving assence of run time exceptions has not be done since it seems to be tricky with floating points (e.g., proving that the argument of `Log` is always positive). Maybe later.

## Library content 

At the moment I just translated the functions I needed, therefore the library is far from being complete. Currently available functions are

* `Beta_Incomplete`
* `Log_Beta`
* `Log_Gamma`
* `Gamma`

and few "technical" functions among which maybe the most useful is

* `Eval_Chebyshev` that is used to compute a Chebyshev interpolation with a given set of coefficients.

# How do I use it? 

1. Install it
   * You can use `alire` to `with` this library
   * Alternatively, just place the files in src/ where your compiler will find them
2. Use it
   * The package `Specfun.Generic_Special_Functions` works like the `Ada.Numerics` packages: it is a generic package that needs a floating point type `Float_Type` to be instantiated.
   * The function interface is pretty obvious

# Examples

Maybe you could want to check the `test/` directory



