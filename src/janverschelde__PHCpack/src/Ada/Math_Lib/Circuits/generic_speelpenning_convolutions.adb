with unchecked_deallocation;
with Exponent_Indices;

package body Generic_Speelpenning_Convolutions is

  function Exponent_Maxima
             ( c : Circuits; dim : integer32 )
             return Standard_Integer_Vectors.Vector is

    res : Standard_Integer_Vectors.Vector(1..dim)
        := Exponent_Indices.Maxima(c(c'first).xps);

  begin
    for k in c'first+1..c'last loop
      declare
        mxe : constant Standard_Integer_Vectors.Vector(1..dim)
            := Exponent_Indices.Maxima(c(k).xps);
      begin
        for i in mxe'range loop
          if mxe(i) > res(i)
           then res(i) := mxe(i);
          end if;
        end loop;
      end;
    end loop;
    return res;
  end Exponent_Maxima;

  function Create ( c : Circuits; dim,deg : integer32 ) return System is

    neq : constant integer32 := c'last;
    res : System(neq,neq+1,dim,dim+1,deg);

  begin
    res.crc := c;
    res.mxe := Exponent_Maxima(c,dim);
    res.pwt := Allocate(res.mxe,deg);
   -- res.yd := Allocate_Coefficients(neq+1,deg);
    res.yd := Allocate_Coefficients(dim+1,deg);
    res.vy := Linearized_Allocation(neq,deg);
    res.yv := Allocate_Coefficients(neq,deg);
    res.vm := Allocate_Coefficients(neq,dim,deg);
    return res;
  end Create;

  function Create ( c : Circuits;
                    dim,deg : integer32 ) return Link_to_System is

    res_rep : constant System(c'last,c'last+1,dim,dim+1,deg)
            := Create(c,dim,deg);
    res : constant Link_to_System := new System'(res_rep);

  begin
    return res;
  end Create;

  function Create ( x : VecVecs.VecVec;
                    d : Standard_Integer_Vectors.Vector )
                  return Link_to_VecVecVec is

    res : Link_to_VecVecVec;
    pwt : VecVecVec(x'range);

  begin
    for i in x'range loop
      if d(i) > 2 then
        declare
          xpw : constant VecVecs.VecVec(1..d(i)-2)
              := Allocate_Coefficients(d(i)-2,x(i)'last);
        begin
          Multiply(x(i),x(i),xpw(1));
          for k in 2..(d(i)-2) loop
            Multiply(xpw(k-1),x(i),xpw(k));
          end loop;
          pwt(i) := new VecVecs.VecVec'(xpw);
        end;
      end if;
    end loop;
    res := new VecVecVec'(pwt);
    return res;
  end Create;

  function Allocate ( mxe : Standard_Integer_Vectors.Vector;
                      deg : integer32 )
                    return Link_to_VecVecVec is

    res : Link_to_VecVecVec;
    pwt : VecVecVec(mxe'range);

  begin
    for i in mxe'range loop
      if mxe(i) > 2 then
        declare
          xpw : constant VecVecs.VecVec(1..mxe(i)-2)
              := Allocate_Coefficients(mxe(i)-2,deg);
        begin
          pwt(i) := new VecVecs.VecVec'(xpw);
        end;
      end if;
    end loop;
    res := new VecVecVec'(pwt);
    return res;
  end Allocate;

  procedure Copy ( v_from : in Link_to_VecVecVec;
                   v_to : out Link_to_VecVecVec ) is

    use VecVecs;

  begin
    Clear(v_to);
    if v_from /= null then
      declare
        pwt : VecVecVec(v_from'range);
      begin
        for k in v_from'range loop
          if v_from(k) /= null then
            declare
              vv : VecVecs.VecVec(v_from(k)'range);
            begin
              VecVecs.Copy(v_from(k).all,vv);
              pwt(k) := new VecVecs.VecVec'(vv);
            end;
          end if;
        end loop;
        v_to := new VecVecVec'(pwt);
      end;
    end if;
  end Copy;

-- COPY WITH DEGREE SPECIFICATIONS :

  function Copy ( v : Vectors.Vector; deg : integer32 )
                return Vectors.Vector is

    res : Vectors.Vector(0..deg);

  begin
    for i in v'range loop
      exit when (i > res'last);
      res(i) := v(i);
    end loop;
    if v'last < res'last then
      for i in v'last+1..res'last loop
        res(i) := Ring.zero;
      end loop;
    end if;
    return res;
  end Copy;

  function Copy ( v : Vectors.Link_to_Vector; deg : integer32 )
                return Vectors.Link_to_Vector is

    use Vectors;
    res : Link_to_Vector;

  begin
    if v /= null then
      declare
        vcp : constant Vectors.Vector(0..deg) := Copy(v.all,deg);
      begin
        res := new Vectors.Vector'(vcp);
      end;
    end if;
    return res;
  end Copy;

  function Copy ( v : VecVecs.VecVec; deg : integer32 )
                return VecVecs.VecVec is

    res : VecVecs.VecVec(v'range);

  begin
    for k in v'range loop
      res(k) := Copy(v(k),deg);
    end loop;
    return res;
  end Copy;

  function Copy ( v : Link_to_VecVecVec; deg : integer32 )
                return Link_to_VecVecVec is

    res : Link_to_VecVecVec;

    use VecVecs;

  begin
    if v /= null then
      declare
        pwt : VecVecVec(v'range);
      begin
        for k in v'range loop
          if v(k) /= null then
            declare
              vv : constant VecVecs.VecVec(v(k)'range) := Copy(v(k).all,deg);
            begin
              pwt(k) := new VecVecs.VecVec'(vv);
            end;
          end if;
        end loop;
        res := new VecVecVec'(pwt);
      end;
    end if;
    return res;
  end Copy;

  function Copy ( c : Circuit; deg : integer32 ) return Circuit is

    res : Circuit(c.nbr,c.dim,c.dim1,c.dim2);

  begin
    Standard_Integer_VecVecs.Copy(c.xps,res.xps);
    Standard_Integer_VecVecs.Copy(c.idx,res.idx);
    Standard_Integer_VecVecs.Copy(c.fac,res.fac);
    res.cff := Copy(c.cff,deg);
    res.cst := Copy(c.cst,deg);
    res.forward := Copy(c.forward,deg);
    res.backward := Copy(c.backward,deg);
    res.cross := Copy(c.cross,deg);
    res.wrk := Copy(c.wrk,deg);
    res.acc := Copy(c.acc,deg);
    return res;
  end Copy;

  function Copy ( c : Link_to_Circuit; deg : integer32 )
                return Link_to_Circuit is

    res : Link_to_Circuit;

  begin
    if c /= null then
      declare
        crc : constant Circuit(c.nbr,c.dim,c.dim1,c.dim2) := Copy(c.all,deg);
      begin  
        res := new Circuit'(crc);
      end;
    end if;
    return res;
  end Copy;

  function Copy ( c : Circuits; deg : integer32 ) return Circuits is

    res : Circuits(c'range);

  begin
    for k in c'range loop
      res(k) := Copy(c(k),deg);
    end loop;
    return res;
  end Copy;

  function Copy ( s : System; deg : integer32 ) return System is

    res : System(s.neq,s.neq1,s.dim,s.dim1,deg);

  begin
    res.crc := Copy(s.crc,deg);
    res.mxe := s.mxe;
    res.pwt := Copy(s.pwt,deg);
    res.yd := Copy(s.yd,deg);
    res.vy := Linearized_Allocation(s.neq,deg);
    for k in s.vy'range loop
      exit when (k > deg);
      res.vy(k) := new Vectors.Vector'(s.vy(k).all);
    end loop;
    res.yv := Copy(s.yv,deg);
    res.vm := Allocate_Coefficients(s.neq,s.dim,deg);
    for k in s.vm'range loop
      exit when (k > deg);
      res.vm(k) := new Matrices.Matrix'(s.vm(k).all);
    end loop;    
    return res;
  end Copy;

  function Copy ( s : Link_to_System; deg : integer32 )
                return Link_to_System is

    res : Link_to_System;

  begin
    if s /= null then
      declare
        scp : constant System(s.neq,s.neq1,s.dim,s.dim1,deg)
            := Copy(s.all,deg);
      begin
        res := new System'(scp);
      end;
    end if;
    return res;
  end Copy;

-- COPY WITHOUT DEGREE SPECIFICATIONS :

  procedure Copy ( c_from : in Circuit; c_to : out Circuit ) is

    use Vectors;

  begin
    Standard_Integer_VecVecs.Copy(c_from.xps,c_to.xps);
    Standard_Integer_VecVecs.Copy(c_from.idx,c_to.idx);
    Standard_Integer_VecVecs.Copy(c_from.fac,c_to.fac);
    VecVecs.Copy(c_from.cff,c_to.cff);
    if c_from.cst /= null
     then c_to.cst := new Vectors.Vector'(c_from.cst.all);
    end if;
    VecVecs.Copy(c_from.forward,c_to.forward);
    VecVecs.Copy(c_from.backward,c_to.backward);
    VecVecs.Copy(c_from.cross,c_to.cross);
    if c_from.wrk /= null
     then c_to.wrk := new Vectors.Vector'(c_from.wrk.all);
    end if;
    if c_from.acc /= null
     then c_to.acc := new Vectors.Vector'(c_from.acc.all);
    end if;
  end Copy;

  procedure Copy ( c_from : in Link_to_Circuit; c_to : out Link_to_Circuit ) is
  begin
    Clear(c_to); -- if c_from = null, then c_to becomes null as well
    if c_from /= null then
      declare
        crc : Circuit(c_from.nbr,c_from.dim,c_from.dim1,c_from.dim2);
      begin
        Copy(c_from.all,crc);
        c_to := new Circuit'(crc);
      end;
    end if;
  end Copy;

  procedure Copy ( c_from : in Circuits; c_to : out Circuits ) is
  begin
    for k in c_from'range loop
      Copy(c_from(k),c_to(k));
    end loop;
  end Copy;

  procedure Copy ( s_from : in System; s_to : out System ) is
  begin
    Copy(s_from.crc,s_to.crc);
    s_to.mxe := s_from.mxe;
    Copy(s_from.pwt,s_to.pwt);
    VecVecs.Copy(s_from.yd,s_to.yd);
    VecVecs.Copy(s_from.vy,s_to.vy);
    VecVecs.Copy(s_from.yv,s_to.yv);
    VecMats.Copy(s_from.vm,s_to.vm);
  end Copy;

  procedure Copy ( s_from : in Link_to_System; s_to : out Link_to_System ) is
  begin
    Clear(s_to);
    if s_from /= null then
      declare
        s : System(s_from.neq,s_from.neq1,s_from.dim,s_from.dim1,s_from.deg);
      begin
        Copy(s_from.all,s);
        s_to := new System'(s);
      end;
    end if;
  end Copy;

  procedure Compute ( pwt : in Link_to_VecVecVec;
                      mxe : in Standard_Integer_Vectors.Vector;
                      x : in Vectors.Vector ) is

    xpw : VecVecs.Link_to_VecVec;
    p,q : Vectors.Link_to_Vector;

    use Ring;

  begin
    for i in x'range loop
      if mxe(i) > 2 then
        xpw := pwt(i);
        p := xpw(1);
        p(0) := x(i)*x(i);
        for k in 2..(mxe(i)-2) loop
          p := xpw(k); q := xpw(k-1);
          p(0) := x(i)*q(0);
        end loop;
      end if;
    end loop;
  end Compute;

  procedure Compute ( pwt : in Link_to_VecVecVec;
                      mxe : in Standard_Integer_Vectors.Vector;
                      x : in VecVecs.VecVec ) is

    xpw : VecVecs.Link_to_VecVec;

  begin
    for i in x'range loop
      if mxe(i) > 2 then
        xpw := pwt(i);
        Multiply(x(i),x(i),xpw(1));
        for k in 2..(mxe(i)-2) loop
          Multiply(xpw(k-1),x(i),xpw(k));
        end loop;
      end if;
    end loop;
  end Compute;

-- DEALLOCATORS :

  procedure Clear ( pwt : in out VecVecVec ) is
  begin
    for k in pwt'range loop
      VecVecs.Deep_Clear(pwt(k));
    end loop;
  end Clear;

  procedure Clear ( pwt : in out Link_to_VecVecVec ) is

    procedure free is new unchecked_deallocation(VecVecVec,Link_to_VecVecVec);

  begin
    if pwt /= null then
      Clear(pwt.all);
      free(pwt);
    end if;
  end Clear;

  procedure Clear ( pwt : in out VecVecVec_Array ) is
  begin
    for k in pwt'range loop
      Clear(pwt(k));
    end loop;
  end Clear;

  procedure Clear ( c : in out Circuit ) is
  begin
    Standard_Integer_VecVecs.Clear(c.xps);
    Standard_Integer_VecVecs.Clear(c.idx);
    Standard_Integer_VecVecs.Clear(c.fac);
    VecVecs.Clear(c.cff);
    Vectors.Clear(c.cst);
    VecVecs.Clear(c.forward);
    VecVecs.Clear(c.backward);
    VecVecs.Clear(c.cross);
    Vectors.Clear(c.wrk);
    Vectors.Clear(c.acc);
  end Clear;

  procedure Clear ( c : in out Link_to_Circuit ) is

    procedure free is
      new unchecked_deallocation(Circuit,Link_to_Circuit);

  begin
    if c /= null then
      Clear(c.all);
      free(c);
    end if;
  end Clear;

  procedure Clear ( c : in out Circuits ) is
  begin
    for k in c'range loop
      Clear(c(k));
    end loop;
  end Clear;

  procedure Clear ( c : in out Link_to_Circuits ) is

    procedure free is
      new unchecked_deallocation(Circuits,Link_to_Circuits);

  begin
    if c /= null then
      Clear(c.all);
      free(c);
    end if;
  end Clear;

  procedure Clear ( s : in out System ) is
  begin
    Clear(s.crc);
    Clear(s.pwt);
    VecVecs.Clear(s.yd);
    VecVecs.Clear(s.vy);
    VecVecs.Clear(s.yv);
    VecMats.Clear(s.vm);
  end Clear;

  procedure Clear ( s : in out Link_to_System ) is

    procedure free is new unchecked_deallocation(System,Link_to_System);

  begin
    if s /= null then
      Clear(s.all);
      free(s);
    end if;
  end Clear;

  procedure Clear ( s : in out System_Array ) is
  begin
    for k in s'range loop
      Clear(s(k));
    end loop;
  end Clear;

-- ALLOCATORS :

  function Allocate_Coefficients
             ( deg : integer32 ) return Vectors.Link_to_Vector is

    cff : constant Vectors.Vector(0..deg) := (0..deg => Ring.zero);
    res : constant Vectors.Link_to_Vector := new Vectors.Vector'(cff);

  begin
    return res;
  end Allocate_Coefficients;

  function Allocate_Coefficients
             ( dim,deg : integer32 ) return VecVecs.VecVec is

    res : VecVecs.VecVec(1..dim);

  begin
    for k in 1..dim loop
      res(k) := Allocate_Coefficients(deg);
    end loop;
    return res;
  end Allocate_Coefficients;

  function Linearized_Allocation
             ( dim,deg : integer32 ) return VecVecs.VecVec is

    res : VecVecs.VecVec(0..deg);

  begin
    for k in 0..deg loop
      declare
        cff : constant Vectors.Vector(1..dim) := (1..dim => Ring.zero);
      begin
        res(k) := new Vectors.Vector'(cff);
      end;
    end loop;
    return res;
  end Linearized_Allocation;

  function Allocate_Coefficients
             ( nbq,nvr,deg : integer32 ) return VecMats.VecMat is

    res : VecMats.VecMat(0..deg);

  begin
    for k in res'range loop
      declare
        mat : Matrices.Matrix(1..nbq,1..nvr);
      begin
        for i in 1..nbq loop
          for j in 1..nvr loop
            mat(i,j) := Ring.zero;
          end loop;
        end loop;
        res(k) := new Matrices.Matrix'(mat);
      end;
    end loop;
    return res;
  end Allocate_Coefficients;

-- AUXILIARY COMPUTATIONAL PROCEDURES :

  procedure Update ( values : in Vectors.Link_to_Vector;
                     inc : in Vectors.Link_to_Vector ) is

    use Ring;

  begin
    for i in values'range loop
      exit when (i > inc'last);
      values(i) := values(i) + inc(i);
    end loop;
  end Update;

  procedure Multiply ( first,second,product : in Vectors.Link_to_Vector ) is

    deg : constant integer32 := first'last;

    use Ring;

  begin
    product(0) := first(0)*second(0);
    for k in 1..deg loop
      product(k) := first(0)*second(k);
      for i in 1..k loop
        product(k) := product(k) + first(i)*second(k-i);
      end loop;
    end loop;
  end Multiply;

-- PLAIN EVALUATION AT A NUMBER :

  function Eval ( c : Circuit; x : Vectors.Vector ) return Ring.number is

    use Ring,Vectors;

    res,val : number;
    pwr : Standard_Integer_Vectors.Link_to_Vector;
    pcf : Vectors.Link_to_Vector;

  begin
    if c.cst /= null
     then Copy(c.cst(0),res);
     else Copy(zero,res);
    end if;
    for k in 1..c.nbr loop
      pwr := c.xps(k);
      pcf := c.cff(k);
      Copy(pcf(0),val);
      for i in pwr'range loop
        for j in 1..pwr(i) loop
          Mul(val,x(i));
        end loop;
      end loop;
      Add(res,val);
    end loop;
    return res;
  end Eval;

  function Eval ( c : Circuit; x : Vectors.Vector; t : Ring.number )
                return Ring.number is

    use Ring,Vectors;

    res,val : number;
    pwr : Standard_Integer_Vectors.Link_to_Vector;
    pcf : Vectors.Link_to_Vector;

  begin
    if c.cst = null then
      Copy(zero,res);
    else
      Copy(c.cst(c.cst'last),res);
      for k in reverse 0..c.cst'last-1 loop
        Mul(res,t);
        Add(res,c.cst(k));
      end loop;
    end if;
    for k in 1..c.nbr loop
      pcf := c.cff(k);
      Copy(pcf(pcf'last),val);
      for k in reverse 0..pcf'last-1 loop
        Mul(val,t);
        Add(val,pcf(k));
      end loop;
      pwr := c.xps(k);
      for i in pwr'range loop
        for j in 1..pwr(i) loop
          Mul(val,x(i));
        end loop;
      end loop;
      Add(res,val);
    end loop;
    return res;
  end Eval;

  function Eval ( c : Link_to_Circuit; x : Vectors.Vector )
                return Ring.number is
  begin
    if c = null
     then return Ring.zero;
     else return Eval(c.all,x);
    end if;
  end Eval;

  function Eval ( c : Link_to_Circuit; x : Vectors.Vector; t : Ring.number )
	        return Ring.number is
  begin
    if c = null
     then return Ring.zero;
     else return Eval(c.all,x,t);
    end if;
  end Eval;

  function Eval ( c : Circuits; x : Vectors.Vector ) return Vectors.Vector is

    res : Vectors.Vector(c'range);

  begin
    for i in c'range loop
      res(i) := Eval(c(i),x);
    end loop;
    return res;
  end Eval;

  function Eval ( c : Circuits; x : Vectors.Vector; t : Ring.number )
                return Vectors.Vector is

    res : Vectors.Vector(c'range);

  begin
    for i in c'range loop
      res(i) := Eval(c(i),x,t);
    end loop;
    return res;
  end Eval;

-- FIRST DERIVATIVE AT A NUMBER :

  function Diff ( x : Vectors.Vector;
                  e : Standard_Integer_Vectors.Vector; i : integer32 )  
                return Ring.number is

    use Ring;

    res : number := zero;

  begin
    if e(i) >= 1 then
      res := Create(integer(e(i)));
      for k in 1..e(i)-1 loop
        res := res*x(i);
      end loop;
      for k in e'range loop
        if k /= i then
          for j in 1..e(k) loop
            res := res*x(k);
          end loop;
        end if;
      end loop;
    end if;
    return res;
  end Diff;

  function Diff ( c : Circuit; x : Vectors.Vector; i : integer32 )  
                return Ring.number is

    use Ring;

    res : Ring.number := zero;
    d1x : Ring.number;
    lnk : Vectors.Link_to_Vector;

  begin
    for k in c.xps'range loop
      lnk := c.cff(k);
      d1x := lnk(0)*Diff(x,c.xps(k).all,i);
      Add(res,d1x);
    end loop;
    return res;
  end Diff;

  function Diff ( c : Link_to_Circuit; x : Vectors.Vector; i : integer32 )  
                return Ring.number is
  begin
    if c = null
     then return Ring.zero;
     else return Diff(c.all,x,i);
    end if;
  end Diff;

-- SECOND DERIVATIVE AT A NUMBER :

  function Diff ( x : Vectors.Vector;
                  e : Standard_Integer_Vectors.Vector; i,j : integer32 )  
                return Ring.number is

    use Ring;

    res : number := zero;
    fac : integer32;

  begin
    if i = j then
      if e(i) >= 2 then
        fac := e(i)*(e(i)-1);
        res := Create(integer(fac));
        for k in 1..e(i)-2 loop
          res := res*x(i);
        end loop;
        for k in e'range loop
          if k /= i then
            for j in 1..e(k) loop
              res := res*x(k);
            end loop;
          end if;
        end loop;
      end if;
    elsif ((e(i) >= 1) and (e(j) >= 1)) then
      fac := e(i)*e(j);
      res := Create(integer(fac));
      for k in 1..e(i)-1 loop
        res := res*x(i);
      end loop;
      for k in 1..e(j)-1 loop
        res := res*x(j);
      end loop;
      for k in e'range loop
        if ((k /= i) and (k /= j)) then
          for j in 1..e(k) loop
            res := res*x(k);
          end loop;
        end if;
      end loop;
    end if;
    return res;
  end Diff;

  function Diff ( c : Circuit; x : Vectors.Vector; i,j : integer32 )  
                return Ring.number is

    use Ring;

    res : Ring.number := zero;
    d2x : Ring.number;
    lnk : Vectors.Link_to_Vector;

  begin
    for k in c.xps'range loop
      lnk := c.cff(k);
      d2x := lnk(0)*Diff(x,c.xps(k).all,i,j);
      Add(res,d2x);
    end loop;
    return res;
  end Diff;

  function Diff ( c : Link_to_Circuit; x : Vectors.Vector; i,j : integer32 )  
                return Ring.number is
  begin
    if c = null
     then return Ring.zero;
     else return Diff(c.all,x,i,j);
    end if;
  end Diff;

-- REVERSE MODE OF ALGORITHMIC DIFFERENTIATION :

  procedure Speel ( x : in Vectors.Vector;
                    forward,backward,cross : in VecVecs.VecVec ) is

    p,q,r : Vectors.Link_to_Vector;

    use Ring;

  begin
    p := forward(1); p(0) := x(1)*x(2);
    for k in 3..x'last loop
      p := forward(k-1); q := forward(k-2); p(0) := q(0)*x(k);
    end loop;
    if x'last > 2 then
      p := backward(1); p(0) := x(x'last)*x(x'last-1);
      for k in 2..x'last-2 loop
        p := backward(k); q := backward(k-1); p(0) := q(0)*x(x'last-k);
      end loop;
      if x'last = 3 then
        p := cross(1); p(0) := x(1)*x(3);
      else
        p := cross(1); q := backward(x'last-3); p(0) := x(1)*q(0);
        for k in 2..x'last-3 loop
          r := cross(k); p := forward(k-1); q := backward(x'last-2-k);
          r(0) := p(0)*q(0);
        end loop;
        r := cross(x'last-2); p := forward(x'last-3); r(0) := x(x'last)*p(0);
      end if;
    end if;
  end Speel;

  procedure Speel ( x : in VecVecs.VecVec;
                    forward,backward,cross : in VecVecs.VecVec ) is
  begin
    Multiply(x(1),x(2),forward(1));
    for k in 3..x'last loop
      Multiply(forward(k-2),x(k),forward(k-1));
    end loop;
    if x'last > 2 then
      Multiply(x(x'last),x(x'last-1),backward(1));
      for k in 2..x'last-2 loop
        Multiply(backward(k-1),x(x'last-k),backward(k));
      end loop;
      if x'last = 3 then
        Multiply(x(1),x(3),cross(1));
      else
        Multiply(x(1),backward(x'last-3),cross(1));
        for k in 2..x'last-3 loop
          Multiply(forward(k-1),backward(x'last-2-k),cross(k));
        end loop;
        Multiply(forward(x'last-3),x(x'last),cross(x'last-2));
      end if;
    end if;
  end Speel;

  procedure Speel ( x : in Vectors.Vector;
                    idx : in Standard_Integer_Vectors.Vector;
                    forward,backward,cross : in VecVecs.VecVec ) is

    p,q,r : Vectors.Link_to_Vector;

    use Ring;

  begin
    p := forward(1); p(0) := x(idx(1))*x(idx(2));
    for k in 3..idx'last loop
      p := forward(k-1); q := forward(k-2); p(0) := x(idx(k))*q(0);
    end loop;
    if idx'last > 2 then
      p := backward(1); p(0) := x(idx(idx'last))*x(idx(idx'last-1));
      for k in 2..idx'last-2 loop
        p := backward(k); q := backward(k-1); p(0) := x(idx(idx'last-k))*q(0);
      end loop;
      if idx'last = 3 then
        p := cross(1); p(0) := x(idx(1))*x(idx(3));
      else
        p := cross(1); q := backward(idx'last-3); p(0) := x(idx(1))*q(0);
        for k in 2..idx'last-3 loop
          r := cross(k);
          p := forward(k-1); q := backward(idx'last-2-k);
          r(0) := p(0)*q(0);
        end loop;
        r := cross(idx'last-2); p := forward(idx'last-3);
        r(0) := x(idx(idx'last))*p(0);
      end if;
    end if;
  end Speel;

  procedure Speel ( x : in VecVecs.VecVec;
                    idx : in Standard_Integer_Vectors.Vector;
                    forward,backward,cross : in VecVecs.VecVec ) is
  begin
    Multiply(x(idx(1)),x(idx(2)),forward(1));
    for k in 3..idx'last loop
      Multiply(forward(k-2),x(idx(k)),forward(k-1));
    end loop;
    if idx'last > 2 then
      Multiply(x(idx(idx'last)),x(idx(idx'last-1)),backward(1));
      for k in 2..idx'last-2 loop
        Multiply(backward(k-1),x(idx(idx'last-k)),backward(k));
      end loop;
      if idx'last = 3 then
        Multiply(x(idx(1)),x(idx(3)),cross(1));
      else
        Multiply(x(idx(1)),backward(idx'last-3),cross(1));
        for k in 2..idx'last-3 loop
          Multiply(forward(k-1),backward(idx'last-2-k),cross(k));
        end loop;
        Multiply(forward(idx'last-3),x(idx(idx'last)),cross(idx'last-2));
      end if;
    end if;
  end Speel;

  procedure Speel ( idx : in Standard_Integer_VecVecs.VecVec;
                    x : in Vectors.Vector;
                    forward,backward,cross,yd : in VecVecs.VecVec ) is

    use Standard_Integer_Vectors;
    use Ring;

    idk : Standard_Integer_Vectors.Link_to_Vector;
    yptr : constant Vectors.Link_to_Vector := yd(yd'last);
    p : Vectors.Link_to_Vector;

  begin
    for k in idx'range loop
      idk := idx(k);
      if idk /= null then
        if idk'last = 1 then
          yptr(0) := yptr(0) + x(idk(1));
          p := yd(idk(1)); p(0) := p(0) + Ring.one;
        else
          Speel(x,idk.all,forward,backward,cross);
          yptr(0) := yptr(0) + forward(idk'last-1)(0);
          if idk'last = 2 then
            p := yd(idk(2)); p(0) := p(0) + x(idk(1));
            p := yd(idk(1)); p(0) := p(0) + x(idk(2));
          else -- idk'last > 2
            p := yd(idk(1)); p(0) := p(0) + backward(idk'last-2)(0);
            for j in idk'first+1..idk'last-1 loop
              p := yd(idk(j)); p(0) := p(0) + cross(j-1)(0);
            end loop;
            p := yd(idk(idk'last)); p(0) := p(0) + forward(idk'last-2)(0);
          end if;
        end if;
      end if;
    end loop;
  end Speel;

  procedure Speel ( idx : in Standard_Integer_VecVecs.VecVec;
                    x : in VecVecs.VecVec;
                    forward,backward,cross,yd : in VecVecs.VecVec ) is

    use Standard_Integer_Vectors;
    use Ring;

    idk : Standard_Integer_Vectors.Link_to_Vector;
    yptr : constant Vectors.Link_to_Vector := yd(yd'last);
    p : Vectors.Link_to_Vector;

  begin
    for k in idx'range loop
      idk := idx(k);
      if idk /= null then
        if idk'last = 1 then
          Update(yptr,x(idk(1)));
          p := yd(idk(1)); p(0) := p(0) + Ring.one;
        else
          Speel(x,idk.all,forward,backward,cross);
          Update(yptr,forward(idk'last-1));
          if idk'last = 2 then
            Update(yd(idk(2)),x(idk(1)));
            Update(yd(idk(1)),x(idk(2)));
          else -- idk'last > 2 
            Update(yd(idk(1)),backward(idk'last-2));
            for j in idk'first+1..idk'last-1 loop
              Update(yd(idk(j)),cross(j-1));
            end loop;
            Update(yd(idk(idk'last)),forward(idk'last-2));
          end if;
        end if;
      end if;
    end loop;
  end Speel;

  procedure Speel ( idx : in Standard_Integer_VecVecs.VecVec;
                    cff : in VecVecs.VecVec; x : in Vectors.Vector;
                    forward,backward,cross,yd : in VecVecs.VecVec;
                    wrk : in Vectors.Link_to_Vector ) is

    use Standard_Integer_Vectors;
    use Ring;

    idk : Standard_Integer_Vectors.Link_to_Vector;
    yptr : constant Vectors.Link_to_Vector := yd(yd'last);
    pcff,p : Vectors.Link_to_Vector;

  begin
    for k in idx'range loop
      idk := idx(k);
      if idk /= null then
        pcff := cff(k);
        if idk'last = 1 then
          yptr(0) := yptr(0) + pcff(0)*x(idk(1));
          p := yd(idk(1)); p(0) := p(0) + pcff(0);
        else
          Speel(x,idk.all,forward,backward,cross);
          wrk(0) := pcff(0)*forward(idk'last-1)(0);
          yptr(0) := yptr(0) + wrk(0);
          if idk'last = 2 then
            p := yd(idk(2)); p(0) := p(0) + pcff(0)*x(idk(1));
            p := yd(idk(1)); p(0) := p(0) + pcff(0)*x(idk(2));
          else -- idk'last > 2
            wrk(0) := pcff(0)*backward(idk'last-2)(0);
            p := yd(idk(1)); p(0) := p(0) + wrk(0);
            for j in idk'first+1..idk'last-1 loop
              wrk(0) := pcff(0)*cross(j-1)(0);
              p := yd(idk(j)); p(0) := p(0) + wrk(0);
            end loop;
            wrk(0) := pcff(0)*forward(idk'last-2)(0);
            p := yd(idk(idk'last)); p(0) := p(0) + wrk(0);
          end if;
        end if;
      end if;
    end loop;
  end Speel;

  procedure Speel ( idx : in Standard_Integer_VecVecs.VecVec;
                    cff : in VecVecs.VecVec; x : in VecVecs.VecVec;
                    forward,backward,cross,yd : in VecVecs.VecVec;
                    wrk : in Vectors.Link_to_Vector ) is

    use Standard_Integer_Vectors;

    idk : Standard_Integer_Vectors.Link_to_Vector;
    yptr : constant Vectors.Link_to_Vector := yd(yd'last);
    pcff : Vectors.Link_to_Vector;

  begin
    for k in idx'range loop
      idk := idx(k);
      if idk /= null then
        pcff := cff(k);
        if idk'last = 1 then
          Multiply(pcff,x(idk(1)),wrk);
          Update(yptr,wrk);
          Update(yd(idk(1)),pcff);
        else
          Speel(x,idk.all,forward,backward,cross);
          Multiply(pcff,forward(idk'last-1),wrk);
          Update(yptr,wrk);
          if idk'last = 2 then
            Multiply(pcff,x(idk(1)),wrk); Update(yd(idk(2)),wrk);
            Multiply(pcff,x(idk(2)),wrk); Update(yd(idk(1)),wrk);
          else -- idk'last > 2 
            Multiply(pcff,backward(idk'last-2),wrk);
            Update(yd(idk(1)),wrk);
            for j in idk'first+1..idk'last-1 loop
              Multiply(pcff,cross(j-1),wrk);
              Update(yd(idk(j)),wrk);
            end loop;
            Multiply(pcff,forward(idk'last-2),wrk);
            Update(yd(idk(idk'last)),wrk);
          end if;
        end if;
      end if;
    end loop;
  end Speel;

  procedure Multiply_Factor
              ( xpk,facidx : in Standard_Integer_Vectors.Link_to_Vector;
                x : in Vectors.Vector;
                cff,wrk,acc : in Vectors.Link_to_Vector;
                pwt : in Link_to_VecVecVec ) is

    pwx : VecVecs.Link_to_VecVec;
    lpw : Vectors.Link_to_Vector;
    powidx : integer32;

    use Ring;

  begin
    pwx := pwt(facidx(facidx'first));
    powidx := xpk(facidx(facidx'first));   -- power in power table
    if powidx = 2 then
      acc(0) := cff(0)*x(facidx(facidx'first));
    else
      lpw := pwx(powidx-2);  -- coefficients of higher powers
      acc(0) := cff(0)*lpw(0);
    end if;
    for k in facidx'first+1..facidx'last loop
      wrk(0) := acc(0);
      pwx := pwt(facidx(k));
      powidx := xpk(facidx(k));   -- power in power table
      if powidx = 2 then
        acc(0) := wrk(0)*x(facidx(k));
      else
        lpw := pwx(powidx-2);  -- coefficients of higher powers
        acc(0) := wrk(0)*lpw(0);
      end if;
    end loop;
  end Multiply_Factor;

  procedure Multiply_Factor
              ( xpk,facidx : in Standard_Integer_Vectors.Link_to_Vector;
                x : in VecVecs.VecVec;
                cff,wrk,acc : in Vectors.Link_to_Vector;
                pwt : in Link_to_VecVecVec ) is

    pwx : VecVecs.Link_to_VecVec;
    lpw : Vectors.Link_to_Vector;
    powidx : integer32;

  begin
    pwx := pwt(facidx(facidx'first));
    powidx := xpk(facidx(facidx'first));   -- power in power table
    if powidx = 2 then
      Multiply(cff,x(facidx(facidx'first)),acc);
    else
      lpw := pwx(powidx-2);  -- coefficients of higher powers
      Multiply(cff,lpw,acc);
    end if;
    for k in facidx'first+1..facidx'last loop
      for i in wrk'range loop
        wrk(i) := acc(i);
      end loop;
      pwx := pwt(facidx(k));
      powidx := xpk(facidx(k));   -- power in power table
      if powidx = 2 then
        Multiply(wrk,x(facidx(k)),acc);
      else
        lpw := pwx(powidx-2);  -- coefficients of higher powers
        Multiply(wrk,lpw,acc);
      end if;
    end loop;
  end Multiply_Factor;

  procedure Multiply_Power
              ( multiplier : in integer32;
                cff : in Vectors.Link_to_Vector ) is

    factor : constant Ring.number := Ring.create(integer(multiplier));

  begin
    for i in cff'range loop
      Ring.Mul(cff(i),factor);
    end loop;
  end Multiply_Power;

  procedure Speel ( xps,idx,fac : in Standard_Integer_VecVecs.VecVec;
                    cff : in VecVecs.VecVec; x : in Vectors.Vector;
                    forward,backward,cross,yd : in VecVecs.VecVec;
                    wrk,acc : in Vectors.Link_to_Vector;
                    pwt : in Link_to_VecVecVec ) is

    use Standard_Integer_Vectors;
    use Ring;

    idk,xpk,fck : Standard_Integer_Vectors.Link_to_Vector;
    yptr : constant Vectors.Link_to_Vector := yd(yd'last);
    pcff,p : Vectors.Link_to_Vector;
    factor : Ring.number;

  begin
    for k in idx'range loop
      idk := idx(k);           -- the k-th exponent index 
      if idk /= null then
        xpk := xps(k);         -- the k-th exponent vector
        fck := fac(k);         -- the k-th factor index
        pcff := cff(k);
        if idk'last = 1 then
          if fck = null then
            wrk(0) := pcff(0)*x(idk(1)); yptr(0) := yptr(0) + wrk(0);
            p := yd(idk(1)); p(0) := p(0) + pcff(0);
          else
            Multiply_Factor(xpk,fck,x,pcff,wrk,acc,pwt);
            wrk(0) := acc(0)*x(idk(1)); yptr(0) := yptr(0) + wrk(0);
            factor := Ring.Create(integer(xpk(idk(1))));
            Ring.mul(acc(0),factor);
            p := yd(idk(1)); p(0) := p(0) + acc(0);
          end if;
        else
          Speel(x,idk.all,forward,backward,cross);
          if fck = null then
            p := forward(idk'last-1); wrk(0) := pcff(0)*p(0);
          else
            Multiply_Factor(xpk,fck,x,pcff,wrk,acc,pwt);
            p := forward(idk'last-1); wrk(0) := acc(0)*p(0);
          end if;
          yptr(0) := yptr(0) + wrk(0);
          if idk'last = 2 then
            if fck = null then
              wrk(0) := pcff(0)*x(idk(1));
              p := yd(idk(2)); p(0) := p(0) + wrk(0);
              wrk(0) := pcff(0)*x(idk(2));
              p := yd(idk(1)); p(0) := p(0) + wrk(0);
            else -- use the common factor in acc
              wrk(0) := acc(0)*x(idk(1));
              if xpk(idk(2)) > 1 then
                factor := Ring.Create(integer(xpk(idk(2))));
                Ring.mul(wrk(0),factor);
              end if;
              p := yd(idk(2)); p(0) := p(0) + wrk(0);
              wrk(0) := acc(0)*x(idk(2));
              if xpk(idk(1)) > 1 then
                factor := Ring.Create(integer(xpk(idk(1))));
                Ring.mul(wrk(0),factor);
              end if;
              p := yd(idk(1)); p(0) := p(0) + wrk(0);
            end if;
          else -- idk'last > 2 
            if fck = null then
              p := backward(idk'last-2); wrk(0) := pcff(0)*p(0);
              p := yd(idk(1)); p(0) := p(0) + wrk(0);
              for j in idk'first+1..idk'last-1 loop
                p := cross(j-1); wrk(0) := pcff(0)*p(0);
                p := yd(idk(j)); p(0) := p(0) + wrk(0);
              end loop;
              p := forward(idk'last-2); wrk(0) := pcff(0)*p(0);
              p := yd(idk(idk'last)); p(0) := p(0) + wrk(0);
            else
              p := backward(idk'last-2); wrk(0) := acc(0)*p(0);
              factor := Ring.Create(integer(xpk(idk(1))));
              Ring.mul(wrk(0),factor);
              p := yd(idk(1)); p(0) := p(0) + wrk(0);
              for j in idk'first+1..idk'last-1 loop
                p := cross(j-1); wrk(0) := acc(0)*p(0);
                factor := Ring.Create(integer(xpk(idk(j))));
                Ring.mul(wrk(0),factor);
                p := yd(idk(j)); p(0) := p(0) + wrk(0);
              end loop;
              p := forward(idk'last-2); wrk(0) := acc(0)*p(0);
              factor := Ring.Create(integer(xpk(idk(idk'last))));
              Ring.mul(wrk(0),factor);
              p := yd(idk(idk'last)); p(0) := p(0) + wrk(0);
            end if;
          end if;
        end if;
      end if;
    end loop;
  end Speel;

  procedure Speel ( xps,idx,fac : in Standard_Integer_VecVecs.VecVec;
                    cff : in VecVecs.VecVec; x : in VecVecs.VecVec;
                    forward,backward,cross,yd : in VecVecs.VecVec;
                    wrk,acc : in Vectors.Link_to_Vector;
                    pwt : in Link_to_VecVecVec ) is

    use Standard_Integer_Vectors;

    idk,xpk,fck : Standard_Integer_Vectors.Link_to_Vector;
    yptr : constant Vectors.Link_to_Vector := yd(yd'last);
    pcff : Vectors.Link_to_Vector;

  begin
    for k in idx'range loop
      idk := idx(k);           -- the k-th exponent index 
      if idk /= null then
        xpk := xps(k);         -- the k-th exponent vector
        fck := fac(k);         -- the k-th factor index
        pcff := cff(k);
        if idk'last = 1 then
          if fck = null then
            Multiply(pcff,x(idk(1)),wrk); Update(yptr,wrk);
            Update(yd(idk(1)),pcff);
          else
            Multiply_Factor(xpk,fck,x,pcff,wrk,acc,pwt);
            Multiply(acc,x(idk(1)),wrk); Update(yptr,wrk);
            Multiply_Power(xpk(idk(1)),acc); Update(yd(idk(1)),acc);
          end if;
        else
          Speel(x,idk.all,forward,backward,cross);
          if fck = null then
            Multiply(pcff,forward(idk'last-1),wrk);
          else
            Multiply_Factor(xpk,fck,x,pcff,wrk,acc,pwt);
            Multiply(acc,forward(idk'last-1),wrk);
          end if;
          Update(yptr,wrk);
          if idk'last = 2 then
            if fck = null then
              Multiply(pcff,x(idk(1)),wrk); Update(yd(idk(2)),wrk);
              Multiply(pcff,x(idk(2)),wrk); Update(yd(idk(1)),wrk);
            else -- use the common factor in acc
              Multiply(acc,x(idk(1)),wrk);
              if xpk(idk(2)) > 1
               then Multiply_Power(xpk(idk(2)),wrk);
              end if;
              Update(yd(idk(2)),wrk); Multiply(acc,x(idk(2)),wrk);
              if xpk(idk(1)) > 1
               then Multiply_Power(xpk(idk(1)),wrk);
              end if;
              Update(yd(idk(1)),wrk);
            end if;
          else -- idk'last > 2 
            if fck = null then
              Multiply(pcff,backward(idk'last-2),wrk);
              Update(yd(idk(1)),wrk);
              for j in idk'first+1..idk'last-1 loop
                Multiply(pcff,cross(j-1),wrk); Update(yd(idk(j)),wrk);
              end loop;
              Multiply(pcff,forward(idk'last-2),wrk);
              Update(yd(idk(idk'last)),wrk);
            else
              Multiply(acc,backward(idk'last-2),wrk);
              Multiply_Power(xpk(idk(1)),wrk);
              Update(yd(idk(1)),wrk);
              for j in idk'first+1..idk'last-1 loop
                Multiply(acc,cross(j-1),wrk);
                Multiply_Power(xpk(idk(j)),wrk); Update(yd(idk(j)),wrk);
              end loop;
              Multiply(acc,forward(idk'last-2),wrk);
              Multiply_Power(xpk(idk(idk'last)),wrk);
              Update(yd(idk(idk'last)),wrk);
            end if;
          end if;
        end if;
      end if;
    end loop;
  end Speel;

  procedure EvalDiff ( c : in Circuit; x : in Vectors.Vector;
                       pwt : in Link_to_VecVecVec; yd : in VecVecs.VecVec ) is

    use Ring,Vectors;

    p : Link_to_Vector;

  begin
    Speel(c.xps,c.idx,c.fac,c.cff,x,c.forward,c.backward,c.cross,yd,
          c.wrk,c.acc,pwt);
    if c.cst /= null
     then p := yd(yd'last); p(0) := p(0) + c.cst(0);
    end if;
  end EvalDiff;

  procedure EvalDiff ( c : in Circuit; x : in VecVecs.VecVec;
                       pwt : in Link_to_VecVecVec; yd : in VecVecs.VecVec ) is

    use Vectors;

  begin
    Speel(c.xps,c.idx,c.fac,c.cff,x,c.forward,c.backward,c.cross,yd,
          c.wrk,c.acc,pwt);
    if c.cst /= null
     then Update(yd(yd'last),c.cst);
    end if;
  end EvalDiff;

  procedure EvalDiff ( c : in Circuits; x : in Vectors.Vector;
                       pwt : in Link_to_VecVecVec; yd : in VecVecs.VecVec;
                       vy : in VecVecs.VecVec; vm : in VecMats.VecMat ) is

    vleft,vright : Vectors.Link_to_Vector;
    mleft : Matrices.Link_to_Matrix;

  begin
    for i in c'range loop
      EvalDiff(c(i).all,x,pwt,yd);
      vright := yd(x'last+1);      -- the 0-th coefficient of vright is
      vleft := vy(0);              -- assigned to the 0-th vector of vy
      vleft(i) := vright(0);       -- at position i
      vright(0) := Ring.zero;      -- reset the value to zero
      for j in 1..x'last loop
        vright := yd(j);           -- vm(0) is the Jacobian matrix at x
        mleft := vm(0);            -- the row i in vm(k) is the equation
        mleft(i,j) := vright(0);   -- the column j in vm(k) is the variable
        vright(0) := Ring.zero;    -- reset the value to zero
      end loop;
    end loop;
  end EvalDiff;

  procedure EvalDiff ( c : in Circuits; x : in VecVecs.VecVec;
                       pwt : in Link_to_VecVecVec; yd : in VecVecs.VecVec;
                       vy : in VecVecs.VecVec; vm : in VecMats.VecMat ) is

    vleft,vright : Vectors.Link_to_Vector;
    mleft : Matrices.Link_to_Matrix;

  begin
    for i in c'range loop
      EvalDiff(c(i).all,x,pwt,yd);
      vright := yd(x'last+1);
      for j in vright'range loop   -- the j-th coefficient of vright is
        vleft := vy(j);            -- assigned to the j-th vector of vy
        vleft(i) := vright(j);     -- at position i
        vright(j) := Ring.zero;    -- reset the value to zero
      end loop;
      for j in 1..x'last loop
        vright := yd(j);
        for k in vm'range loop     -- k-th coefficient in matrix vm(k)
          mleft := vm(k);          -- the row i in vm(k) is the equation
          mleft(i,j) := vright(k); -- the column j in vm(k) is the variable
          vright(k) := Ring.zero;  -- reset the value to zero
        end loop;
      end loop;
    end loop;
  end EvalDiff;

  procedure Leading_Delinearize ( vy,yv : in VecVecs.VecVec ) is

    vy0 : constant Vectors.Link_to_Vector := vy(0);
    left : Vectors.Link_to_Vector;

  begin
    for i in yv'range loop  -- vy0 holds 0-th coefficient of all series
      left := yv(i);        -- so we assign to coefficients of series i
      left(0) := vy0(i);    -- at position 0 the i-th value of vyk
    end loop;
  end Leading_Delinearize;

  procedure Delinearize ( vy,yv : in VecVecs.VecVec ) is
  begin
    for k in vy'range loop
      declare
        vyk : constant Vectors.Link_to_Vector := vy(k);
        left : Vectors.Link_to_Vector;
      begin
        for i in yv'range loop  -- vyk holds k-th coefficient of all series
          left := yv(i);        -- so we assign to coefficients of series i
          left(k) := vyk(i);    -- at position k the i-th value of vyk
        end loop;
      end;
    end loop;
  end Delinearize;

  procedure Delinearize ( deg : in integer32;
                          vy,yv : in VecVecs.VecVec ) is
  begin
    for k in vy'first..deg loop
      declare
        vyk : constant Vectors.Link_to_Vector := vy(k);
        left : Vectors.Link_to_Vector;
      begin
        for i in yv'range loop  -- vyk holds k-th coefficient of all series
          left := yv(i);        -- so we assign to coefficients of series i
          left(k) := vyk(i);    -- at position k the i-th value of vyk
        end loop;
      end;
    end loop;
  end Delinearize;

  procedure EvalDiff ( s : in System; x : in Vectors.Vector ) is
  begin
    EvalDiff(s.crc,x,s.pwt,s.yd,s.vy,s.vm);
    Leading_Delinearize(s.vy,s.yv);
  end EvalDiff;

  procedure EvalDiff ( s : in Link_to_System; x : in Vectors.Vector ) is
  begin
    EvalDiff(s.crc,x,s.pwt,s.yd,s.vy,s.vm);
    Leading_Delinearize(s.vy,s.yv);
  end EvalDiff;

  procedure EvalDiff ( s : in System; x : in VecVecs.VecVec ) is
  begin
    EvalDiff(s.crc,x,s.pwt,s.yd,s.vy,s.vm);
    Delinearize(s.vy,s.yv);
  end EvalDiff;

  procedure EvalDiff ( s : in Link_to_System; x : in VecVecs.VecVec ) is
  begin
    EvalDiff(s.crc,x,s.pwt,s.yd,s.vy,s.vm);
    Delinearize(s.vy,s.yv);
  end EvalDiff;

end Generic_Speelpenning_Convolutions;
