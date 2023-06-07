--  Solution to Advent of Code 2020, Day 15
-------------------------------------------
--  Rambunctious Recitation
--
--  https://adventofcode.com/2020/day/15
--
--  Full Ada version.
--  Total run time: 0.42 seconds (i5-9400 @ 2.9 GHz).
--
with Ada.Calendar, Ada.Text_IO, Ada.Integer_Text_IO;

procedure AoC_2020_15_full_Ada is

  type Preamble is array (Positive range <>) of Natural;
  --
  procedure Play (pre : Preamble) is
    use Ada.Calendar, Ada.Text_IO, Ada.Integer_Text_IO;
    stop : constant := 30_000_000;
    --  We memorize turn of spoken number (if any), indexed by spoken number.
    type Big_Mem is array (0 .. stop) of Natural;
    type P is access Big_Mem;
    mem : constant P := new Big_Mem;  --  Heap allocation in case of small default stack size...
    not_seen : constant := 0;
    prev, curr : Natural;
    T1, T2 : Time;
  begin
    T1 := Clock;
    for m of mem.all loop
      m := not_seen;
    end loop;
    for i in 1 .. pre'Last - 1 loop
      mem (pre (i)) := i;
    end loop;
    prev := pre (pre'Last);
    --
    for i in pre'Last + 1 .. stop loop
      if mem (prev) = not_seen then
        curr := 0;
      else
        curr := (i - 1) - mem (prev);  --  "Age"
      end if;
      if i = 2020 or  i = stop then
        Put (i); Put (" : "); Put (curr, 0); New_Line;
      end if;
      mem (prev) := i - 1;
      prev := curr;
    end loop;
    T2 := Clock;
    Put_Line ("----   Computation time: " & Duration'Image (T2 - T1));
    New_Line;
  end Play;

begin
  --  Examples shown on https://adventofcode.com/2020/day/15 :
  Play ((0, 3, 6));  --  2020th number is 436;  30m-th number is 175594
  Play ((1, 3, 2));  --  2020th number is 1;    30m-th number is 2578
  Play ((2, 1, 3));  --  2020th number is 10;   30m-th number is 3544142
  Play ((1, 2, 3));  --  2020th number is 27;   30m-th number is 261214
  Play ((2, 3, 1));  --  2020th number is 78;   30m-th number is 6895259
  Play ((3, 2, 1));  --  2020th number is 438;  30m-th number is 18
  Play ((3, 1, 2));  --  2020th number is 1836; 30m-th number is 362
  --  The "real" puzzle:
  Play ((15, 12, 0, 14, 3, 1));
  --  ^ 2020th number is 249; 30m-th number is 41687
end AoC_2020_15_full_Ada;
