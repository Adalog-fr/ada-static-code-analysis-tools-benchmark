--  Example of cross-dependency in bodies

with Cross_A;

with HAT;

package body Cross_B is

  procedure B (n : Natural) is
    use HAT;
  begin
    if n > 0 then
      Put_Line ("B: " & Image (n));
      Cross_A.A (n - 1);
    end if;
  end B;

end Cross_B;
