with text_io;                            use text_io;
with Standard_Integer_Numbers;           use Standard_Integer_Numbers;
with Standard_Integer_Numbers_io;        use Standard_Integer_Numbers_io;
with Standard_Floating_Numbers;          use Standard_Floating_Numbers;
with Double_Double_Numbers;              use Double_Double_Numbers;
with DoblDobl_Complex_Numbers_Polar;
with DoblDobl_Complex_Series_io;         use DoblDobl_Complex_Series_io;

package body DoblDobl_Complex_Algebraic_Series is

  function sqrt ( c : Series; i : natural32;
                  verbose : boolean := false ) return Series is

    res : Series(c.deg) := Create(0,c.deg);
    cpc : constant Series(c.deg) := c;
    wrk,dx : Series(c.deg);
    one : constant double_double := create(integer32(1));
    two : constant double_double := create(integer32(2));
    half : constant double_double := one/two;
    fac : constant Complex_Number := Create(half);
    tol : constant double_float := 1.0E-24;

  begin
    if AbsVal(cpc.cff(0)) < tol then
      res := Create(0,c.deg);
    else
      res := Create(0,c.deg);
      res.cff(0) := DoblDobl_Complex_Numbers_Polar.Root(cpc.cff(0),2,i);
      for i in 0..c.deg loop
        wrk := res*res - cpc;
        dx := fac*wrk/res;
        if verbose then
          put("evaluation at degree = "); put(res.deg,1);
          put_line(" :"); put(wrk);
          put("update dx at degree = "); put(res.deg,1);
          put_line(" :"); put(dx);
        end if;
        res := res - dx;
      end loop;
    end if;
    return res;
  end sqrt;

  function Root ( c : Series; n,i : natural32;
                  verbose : boolean := false ) return Series is

    res : Series(c.deg) := Create(0,c.deg);
    cpc : constant Series(c.deg) := c;
    wrk,dx : Series(c.deg);
    one : constant double_double := create(1.0);
    ddn : constant double_double := create(n);
    denominator : constant double_double := one/ddn;
    fac : constant Complex_Number := Create(denominator);

  begin
    res.cff(0) := DoblDobl_Complex_Numbers_Polar.Root(cpc.cff(0),n,i);
    for i in 0..c.deg loop
      wrk := res**integer(n) - cpc;
      dx := fac*wrk/(res**integer(n-1));
      if verbose then
        put("update dx at degree = "); put(res.deg,1);
        put_line(" :"); put(dx);
      end if;
      res := res - dx;
    end loop;
    return res;
  end Root;

  function Poly_Eval ( p : Vector; z : Series ) return Series is

    res : Series(z.deg) := Create(p(p'last),z.deg);

  begin
    for i in reverse 0..p'last-1 loop
      res := res*z;
      res.cff(0) := res.cff(0) + p(i);
    end loop;
    return res;
  end Poly_Eval;

  function Poly_Diff ( p : Vector; z : Series ) return Series is

    pdg : Complex_Number := Create(p'last);
    res : Series(z.deg) := Create(pdg*p(p'last),z.deg);

  begin
    for i in reverse 1..p'last-1 loop
      res := res*z;
      pdg := Create(i);
      res.cff(0) := res.cff(0) + pdg*p(i);
    end loop;
    return res;
  end Poly_Diff;

  function Poly_Root ( p : Vector; z0 : Complex_Number; c : Series; 
                       verbose : boolean := false ) return Series is

    res : Series(c.deg) := Create(z0,c.deg);
    y,dy,dx : Series(c.deg);

  begin
    for i in 0..c.deg loop
      y := Poly_Eval(p,res) - c;
      dy := Poly_Diff(p,res);
      dx := y/dy;
      if verbose then
        put("update dx at degree = "); put(i,1);
        put_line(" :"); put(dx);
      end if;
      res := res - dx;
    end loop;
    return res;
  end Poly_Root;

end DoblDobl_Complex_Algebraic_Series;
