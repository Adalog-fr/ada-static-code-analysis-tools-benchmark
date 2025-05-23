with Ada.Exceptions;                    use Ada.Exceptions;

with Sparse.Vector_Ops; -- Basic Linear Algebra routines, with possible binding to BLAS

package body Sparse is

  package Sparse_BLA is new Sparse.Vector_Ops(Real, Index, Vector);
  use Sparse_BLA;

-- "exported":

  function Defined_symmetric( A: CRS_matrix ) return Boolean is
  begin
    return A.symmetric;
  end Defined_symmetric;

  function Rows( A: CRS_matrix ) return Index is
  begin
    return A.rows;
  end Rows;

  procedure Mult( A: in CRS_matrix; u: Vector; w: in out Vector ) is

--     calcule w=a*u

--     A.nnz: nb de coeff non nuls dans la matrice a
--     A.col_ind: indices de ligne pour la matrice a, de long. A.nnz
--     A.row_ptr: indices de colonne pour la matrice a, de long A.rows+1,
--           avec A.row_ptr(A.rows+1) = A.nnz+1

--     u: vecteur donne
--     w: vecteur resultat, w=a*u

--     a: matrice du systeme memorisee sous forme creuse, les lignes
--        sont memorisee de maniere contigue
--
--       par exemple la matrice
--
--               11    0    0   14   15
--                0   22    0    0    0
--                0    0   33    0    0
--               41    0    0   44   45
--               51    0    0   54   55
--
--        est representee sous forme non symetrique par
--
--            a(*)    =   [11, 14, 15, 22, 33, 41, 44, 45, 51, 54, 55]
--            A.col_ind(*)   =   [1, 4, 5, 2, 3, 1, 4, 5, 1, 4, 5]
--            A.row_ptr(*)   =   [1, 4, 5, 6, 9, 12]
--
--         et sous forme symetrique
--
--            a(*)    =   [11, 14, 15, 22, 33, 44, 45, 55]
--            A.col_ind(*)   =   [1, 4, 5, 2, 3, 4, 5, 5]
--            A.row_ptr(*)   =   [1, 4, 5, 6, 8, 9]

   deb, fin, jaj: Index;
   ui, wi, wi_sum: Real;
   val: Vector renames A.val;
   col_ind: Index_array renames A.col_ind;
   row_ptr: Index_array renames A.row_ptr;

   begin

      if  not A.symmetric  then
         -- *** la matrice est memorisee sous forme non symetrique

         for i in 1 .. A.rows loop
            deb := row_ptr(i);
            fin := row_ptr(i + 1) - 1;
            wi_sum := 0.0;
            for j in deb .. fin loop
               wi_sum := wi_sum + val(j) * u(col_ind(j));
            end loop;
            w(i) := wi_sum;
         end loop;

      else
         -- *** la matrice est memorisee sous forme symetrique

         for i in 1 .. A.rows loop
            w(i) := 0.0;
         end loop;
         for i in 1 .. A.rows loop
            deb := row_ptr(i);
            fin := row_ptr(i + 1) - 1;
            ui := u(i);
            wi := w(i);
            for j in deb .. fin loop
               jaj := col_ind(j);
               wi     := wi     + val(j) * u(jaj);
               w(jaj) := w(jaj) + val(j) * ui;
            end loop;
            w(i) := wi;
         end loop;
      end if;

   end Mult;

--  function "*"( A: CRS_matrix; u: vector ) return vector is
--    w: vector(1..A.rows);
--    begin
--      Mult( A, u, w );
--      return w;
--    end;

   --- GdM 29.III.1999

   procedure Put( A: in out CRS_matrix; i,j: Index; value: Real ) is
     row_begin, row_end, ci: Index;
   begin
     row_begin := A.row_ptr(i);
     row_end   := A.row_ptr(i+1)-1;

     for e in row_begin .. row_end loop
       ci:= A.col_ind(e);
       if ci = j then
         A.val(e):= value;
         return;
       end if;
     end loop;

     Raise_Exception(
       position_not_found_in_sparse_matrix'Identity,
       " i=" & Index'Image(i) & ", j=" & Index'Image(j)
     );

   end Put;

   procedure Add( A: in out CRS_matrix; i,j: Index; value: Real ) is
     row_begin, row_end, ci: Index;
   begin
     row_begin := A.row_ptr(i);
     row_end   := A.row_ptr(i+1)-1;

     for e in row_begin .. row_end loop
       ci:= A.col_ind(e);
       if ci = j then
         A.val(e):= A.val(e) + value;
         return;
       end if;
     end loop;

     Raise_Exception(
       position_not_found_in_sparse_matrix'Identity,
       " i=" & Index'Image(i) & ", j=" & Index'Image(j)
     );

   end Add;

   function  Get( A: in    CRS_matrix; i,j: Index ) return Real is
     row_begin, row_end, ci: Index;
   begin
     row_begin := A.row_ptr(i);
     row_end   := A.row_ptr(i+1)-1;

     for e in row_begin .. row_end loop
       ci:= A.col_ind(e);
       if ci = j then
         return A.val(e);
       end if;
     end loop;

     Raise_Exception(
       position_not_found_in_sparse_matrix'Identity,
       " i=" & Index'Image(i) & ", j=" & Index'Image(j)
     );

  end Get;

  procedure Copy( u: in Vector; v: out Vector )
    renames Sparse_BLA.Copy;

  function "*"(u,v: Vector) return Real
    renames Sparse_BLA."*";

  procedure Add_scaled( factor: Real; u: in Vector; v: in out Vector )
    renames Sparse_BLA.Add_scaled;

  procedure Scale( factor: Real; u: in out Vector )
    renames Sparse_BLA.Scale;

end Sparse;
