# What is this?
This is a simple Ada library with a generic package providing a function for sorting using a *partial order*, that is an ordering function `<` that satisfies

1. If `A=B` then neither `A<B` nor `B<A` (*anti-reflexive*)
2. If `A<B` and `B<C` then `A<C` (*transitivity*)

However, it is not necessarily true that if `A/=B` then it must be `A<B` or `B<A`. If you want some example of suitable ordering, just think about
* Set ordered by inclusion (`A<B` if A is a subset of B) 
* Integers ordered by divisibility (`A<B` if A divides B)
* Files in a *well-formed* Makefile ordered by dependency (`A<B` if B depends on A)

# Installation

Just put the files where your compiler will find them.

# How do I use it?

It is very simple. The comments inside the spec package should suffice.
