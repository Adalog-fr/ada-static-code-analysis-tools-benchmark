with Standard_Integer_Numbers;          use Standard_Integer_Numbers;

package body Standard_Witness_Solutions is

-- DATA :

  topdimension : integer32; -- top dimension of the start of the cascade

  embpolysys : Standard_Complex_Poly_Systems.Link_to_Array_of_Poly_Sys;
  emblaursys : Standard_Complex_Laur_Systems.Link_to_Array_of_Laur_Sys;

  embsols : Standard_Complex_Solutions.Link_to_Array_of_Solution_Lists;

-- CONSTRUCTORS :

  procedure Initialize ( topdim : in natural32 ) is

    use Standard_Complex_Poly_Systems;
    use Standard_Complex_Laur_Systems;
    use Standard_Complex_Solutions;

  begin
    topdimension := integer32(topdim);
    embpolysys := new Array_of_Poly_Sys(0..topdimension);
    emblaursys := new Array_of_Laur_Sys(0..topdimension);
    embsols := new Array_of_Solution_Lists(0..topdimension);
  end Initialize;

  procedure Save_Embedded_System
              ( p : in Standard_Complex_Poly_Systems.Poly_Sys;
                k : in natural32 ) is

    use Standard_Complex_Poly_Systems;

    q : Poly_Sys(p'range);

  begin
    Copy(p,q);
    embpolysys(integer32(k)) := new Poly_Sys'(q);
  end Save_Embedded_System;

  procedure Save_Embedded_System
              ( p : in Standard_Complex_Laur_Systems.Laur_Sys;
                k : in natural32 ) is

    use Standard_Complex_Laur_Systems;

    q : Laur_Sys(p'range);

  begin
    Copy(p,q);
    emblaursys(integer32(k)) := new Laur_Sys'(q);
  end Save_Embedded_System;

  procedure Save_Witness_Points
              ( s : in Standard_Complex_Solutions.Solution_List;
                k : in natural32 ) is

    use Standard_Complex_Solutions;

  begin
    Copy(s,embsols(integer32(k)));
  end Save_Witness_Points;

-- SELECTORS :

  function Load_Embedded_System
              ( k : natural32 )
              return Standard_Complex_Poly_Systems.Link_to_Poly_Sys is

    res : constant Standard_Complex_Poly_Systems.Link_to_Poly_Sys
        := embpolysys(integer32(k));

  begin
    return res;
  end Load_Embedded_System;

  function Load_Embedded_System
              ( k : natural32 )
              return Standard_Complex_Laur_Systems.Link_to_Laur_Sys is

    res : constant Standard_Complex_Laur_Systems.Link_to_Laur_Sys
        := emblaursys(integer32(k));

  begin
    return res;
  end Load_Embedded_System;

  function Load_Witness_Points ( k : natural32 )
              return Standard_Complex_Solutions.Solution_List is

    res : constant Standard_Complex_Solutions.Solution_List
        := embsols(integer32(k));

  begin
    return res;
  end Load_Witness_Points;

-- DESTRUCTOR :

  procedure Clear is

    use Standard_Complex_Poly_Systems;
    use Standard_Complex_Laur_Systems;
    use Standard_Complex_Solutions;

  begin
    topdimension := -1;
    if embpolysys /= null
     then Clear(embpolysys);
    end if;
    if emblaursys /= null
     then Clear(emblaursys);
    end if;
    if embsols /= null
     then Clear(embsols);
    end if;
  end Clear;

end Standard_Witness_Solutions;
