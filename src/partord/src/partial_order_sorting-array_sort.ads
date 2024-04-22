--
-- This package provides a function that sorts an array using a
-- **partial order** "<", that is, if A /= B it is not necessarily true that
-- A < B or B < A.  It is only required that
--
-- 1. If A = B then both A < B and B < A are false
-- 2. "<" is transitive, that is, if A < B and B < C, then A < C
--
-- Think about ordering sets by inclusion (A < B if A is a subset of B) or
-- integers by divisibility (A < B if A divides B).
--
-- A note on computational complexity: at the moment (2021-05-28) the algorithm
-- constructs a graph where there is an edge from A to B if B < A and
-- then travers the graph "make"-like, that is, starting from nodes that
-- have no antecedent and moving down.  The graph construction algorithm
-- compares every element with every other element, so the cost is quadratic.
-- Also the graph is redundant since if A < B < C there are edges from C to B
-- and from C to A, but the second is not necessary because of transitivity.
--
-- I wrote initially this because I needed to order small sets, so the
-- complexity was not an issue, but could it be done more efficiently?
--
generic
   type Index_Type is range <>;
   type Element_Type is private;

   type Container_Type is array (Index_Type range <>) of Element_Type;
   with function "<" (X, Y : Element_Type) return Boolean is <>;
   --  with function Image (X : Element_Type) return String;
package Partial_Order_Sorting.Array_Sort is
   --
   -- Sort the elements in Item in "decreasing" order in the sense that
   -- if Sort'result(I) < Sort'result(j) then it must be I > J.  Yes,
   -- it looks like a usual ordering, but in this case "<" can be partial,
   -- that is, it is not necessarily true that if A /= B then it must
   -- be A<B or A > B.  Think about, for example, sets ordered by inclusion.
   --
   function Sort (Item : Container_Type) return Container_Type
     with
       Post => (for all I in Sort'Result'Range =>
                  (for all J in Sort'Result'Range =>
                     (if Sort'Result (I) < Sort'Result (J) then J < I)))
     and (Sort'Result'Length = Item'Length);

   --
   -- Same as above, but working "in place".  Yeah, syntactic sugar.
   --
   procedure Sort (Item : in out Container_Type);

end Partial_Order_Sorting.Array_Sort;
