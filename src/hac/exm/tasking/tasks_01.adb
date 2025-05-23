--  Here is the simplest case of tasking you can imagine: just
--  a bunch of tasks running in parallel, without communication
--  between them.

with HAT;

procedure Tasks_01 is

  task T1;
  task T2;

  task body T1 is
  begin
    for i in 1 .. 4 loop
      HAT.Put_Line ("  [1] I am T1");
      delay 0.01;
    end loop;
  end T1;

  task body T2 is
  begin
    for i in 1 .. 6 loop
      HAT.Put_Line ("  [2]   I am T2");
      delay 0.01;
    end loop;
  end T2;

begin
  for i in 1 .. 3 loop
    HAT.Put_Line ("I am the main procedure");
    delay 0.01;
  end loop;
end Tasks_01;
