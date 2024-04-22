pragma Ada_2022;

with Ada.Streams; use Ada.Streams;

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Caller;

with Base64.URLSafe; use Base64.URLSafe;

package body Test_URLSafe is
   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   Big_Raw : constant Stream_Element_Array :=
     (16#00#, 16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#, 16#08#,
      16#09#, 16#0a#, 16#0b#, 16#0c#, 16#0d#, 16#0e#, 16#0f#, 16#10#, 16#11#,
      16#12#, 16#13#, 16#14#, 16#15#, 16#16#, 16#17#, 16#18#, 16#19#, 16#1a#,
      16#1b#, 16#1c#, 16#1d#, 16#1e#, 16#1f#, 16#20#, 16#21#, 16#22#, 16#23#,
      16#24#, 16#25#, 16#26#, 16#27#, 16#28#, 16#29#, 16#2a#, 16#2b#, 16#2c#,
      16#2d#, 16#2e#, 16#2f#, 16#30#, 16#31#, 16#32#, 16#33#, 16#34#, 16#35#,
      16#36#, 16#37#, 16#38#, 16#39#, 16#3a#, 16#3b#, 16#3c#, 16#3d#, 16#3e#,
      16#3f#, 16#40#, 16#41#, 16#42#, 16#43#, 16#44#, 16#45#, 16#46#, 16#47#,
      16#48#, 16#49#, 16#4a#, 16#4b#, 16#4c#, 16#4d#, 16#4e#, 16#4f#, 16#50#,
      16#51#, 16#52#, 16#53#, 16#54#, 16#55#, 16#56#, 16#57#, 16#58#, 16#59#,
      16#5a#, 16#5b#, 16#5c#, 16#5d#, 16#5e#, 16#5f#, 16#60#, 16#61#, 16#62#,
      16#63#, 16#64#, 16#65#, 16#66#, 16#67#, 16#68#, 16#69#, 16#6a#, 16#6b#,
      16#6c#, 16#6d#, 16#6e#, 16#6f#, 16#70#, 16#71#, 16#72#, 16#73#, 16#74#,
      16#75#, 16#76#, 16#77#, 16#78#, 16#79#, 16#7a#, 16#7b#, 16#7c#, 16#7d#,
      16#7e#, 16#7f#);
   Big_Base64 : constant String :=
     "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMz" &
     "Q1Njc4OTo7PD0-P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdo" &
     "aWprbG1ub3BxcnN0dXZ3eHl6e3x9fn8=";

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(URLSafe) ";
   begin
      Test_Suite.Add_Test
        (Caller.Create
           (Name & "encoding empty input", Test_Encoding_Empty'Access));
      Test_Suite.Add_Test
        (Caller.Create
           (Name & "encoding data, 2 padding characters",
            Test_Encoding_Pad2'Access));
      Test_Suite.Add_Test
        (Caller.Create
           (Name & "encoding data, 1 padding character",
            Test_Encoding_Pad1'Access));
      Test_Suite.Add_Test
        (Caller.Create
           (Name & "encoding data, no padding", Test_Encoding_Pad0'Access));
      Test_Suite.Add_Test
        (Caller.Create (Name & "encoding big data", Test_Encoding_Big'Access));
      Test_Suite.Add_Test
        (Caller.Create
           (Name & "encoding RFC4648", Test_Encoding_RFC4648'Access));
      Test_Suite.Add_Test
        (Caller.Create
           (Name & "decoding empty input", Test_Decoding_Empty'Access));
      Test_Suite.Add_Test
        (Caller.Create
           (Name & "decoding data, 2 padding characters",
            Test_Decoding_Pad2'Access));
      Test_Suite.Add_Test
        (Caller.Create
           (Name & "decoding data, 1 padding character",
            Test_Decoding_Pad1'Access));
      Test_Suite.Add_Test
        (Caller.Create
           (Name & "decoding data, no padding", Test_Decoding_Pad0'Access));
      Test_Suite.Add_Test
        (Caller.Create (Name & "decoding big data", Test_Decoding_Big'Access));
      Test_Suite.Add_Test
        (Caller.Create
           (Name & "decoding RFC4648", Test_Decoding_RFC4648'Access));

      return Test_Suite'Access;
   end Suite;

   procedure Test_Encoding_Empty (Object : in out Test) is
      Input : constant Stream_Element_Array (1 .. 0) := (others => 0);
   begin
      Assert (Encode (Input) = "", "Result is not empty");
   end Test_Encoding_Empty;

   procedure Test_Encoding_Pad2 (Object : in out Test) is
      Input : constant Stream_Element_Array (1 .. 1) :=
        (others => Character'Pos ('a'));
      Result   : constant String := Encode (Input);
      Expected : constant String := "YQ==";
   begin
      Assert (Result, Expected, "Result is not correct");
   end Test_Encoding_Pad2;

   procedure Test_Encoding_Pad1 (Object : in out Test) is
      Input : constant Stream_Element_Array (1 .. 2) :=
        (others => Character'Pos ('a'));
      Result   : constant String := Encode (Input);
      Expected : constant String := "YWE=";
   begin
      Assert (Result, Expected, "Result is not correct");
   end Test_Encoding_Pad1;

   procedure Test_Encoding_Pad0 (Object : in out Test) is
      Input : constant Stream_Element_Array (1 .. 3) :=
        (others => Character'Pos ('a'));
      Result   : constant String := Encode (Input);
      Expected : constant String := "YWFh";
   begin
      Assert (Result, Expected, "Result is not correct");
   end Test_Encoding_Pad0;

   procedure Test_Encoding_Big (Object : in out Test) is
      Result : constant String := Encode (Big_Raw);
   begin
      Assert (Result, Big_Base64, "Result is not correct");
   end Test_Encoding_Big;

   procedure Test_Encoding_RFC4648 (Object : in out Test) is
   begin
      declare
         Input : constant Stream_Element_Array (1 .. 1) :=
           (others => Character'Pos ('f'));
         Result   : constant String := Encode (Input);
         Expected : constant String := "Zg==";
      begin
         Assert (Result, Expected, "Result is not correct");
      end;
      declare
         Input : constant Stream_Element_Array (1 .. 2) :=
           (Character'Pos ('f'), Character'Pos ('o'));
         Result   : constant String := Encode (Input);
         Expected : constant String := "Zm8=";
      begin
         Assert (Result, Expected, "Result is not correct");
      end;
      declare
         Input : constant Stream_Element_Array (1 .. 3) :=
           (Character'Pos ('f'), Character'Pos ('o'), Character'Pos ('o'));
         Result   : constant String := Encode (Input);
         Expected : constant String := "Zm9v";
      begin
         Assert (Result, Expected, "Result is not correct");
      end;
      declare
         Input : constant Stream_Element_Array (1 .. 4) :=
           (Character'Pos ('f'), Character'Pos ('o'), Character'Pos ('o'),
            Character'Pos ('b'));
         Result   : constant String := Encode (Input);
         Expected : constant String := "Zm9vYg==";
      begin
         Assert (Result, Expected, "Result is not correct");
      end;
      declare
         Input : constant Stream_Element_Array (1 .. 5) :=
           (Character'Pos ('f'), Character'Pos ('o'), Character'Pos ('o'),
            Character'Pos ('b'), Character'Pos ('a'));
         Result   : constant String := Encode (Input);
         Expected : constant String := "Zm9vYmE=";
      begin
         Assert (Result, Expected, "Result is not correct");
      end;
      declare
         Input : constant Stream_Element_Array (1 .. 6) :=
           (Character'Pos ('f'), Character'Pos ('o'), Character'Pos ('o'),
            Character'Pos ('b'), Character'Pos ('a'), Character'Pos ('r'));
         Result   : constant String := Encode (Input);
         Expected : constant String := "Zm9vYmFy";
      begin
         Assert (Result, Expected, "Result is not correct");
      end;
   end Test_Encoding_RFC4648;

   procedure Test_Decoding_Empty (Object : in out Test) is
   begin
      Assert (Decode ("")'Length = 0, "Result is not empty");
   end Test_Decoding_Empty;

   procedure Test_Decoding_Pad2 (Object : in out Test) is
      Input    : constant String                        := "YQ==";
      Result   : constant Stream_Element_Array          := Decode (Input);
      Expected : constant Stream_Element_Array (1 .. 1) :=
        (others => Character'Pos ('a'));
   begin
      Assert (Result'Image, Expected'Image, "Result is not correct");
   end Test_Decoding_Pad2;

   procedure Test_Decoding_Pad1 (Object : in out Test) is
      Input    : constant String                        := "YWE=";
      Result   : constant Stream_Element_Array          := Decode (Input);
      Expected : constant Stream_Element_Array (1 .. 2) :=
        (others => Character'Pos ('a'));
   begin
      Assert (Result'Image, Expected'Image, "Result is not correct");
   end Test_Decoding_Pad1;

   procedure Test_Decoding_Pad0 (Object : in out Test) is
      Input    : constant String                        := "YWFh";
      Result   : constant Stream_Element_Array          := Decode (Input);
      Expected : constant Stream_Element_Array (1 .. 3) :=
        (others => Character'Pos ('a'));
   begin
      Assert (Result'Image, Expected'Image, "Result is not correct");
   end Test_Decoding_Pad0;

   procedure Test_Decoding_Big (Object : in out Test) is
      Result : constant Stream_Element_Array := Decode (Big_Base64);
   begin
      Assert (Result = Big_Raw, "Result is not correct");
   end Test_Decoding_Big;

   procedure Test_Decoding_RFC4648 (Object : in out Test) is
   begin
      declare
         Input    : constant String                        := "Zg==";
         Result   : constant Stream_Element_Array          := Decode (Input);
         Expected : constant Stream_Element_Array (1 .. 1) :=
           (others => Character'Pos ('f'));
      begin
         Assert (Result'Image, Expected'Image, "Result is not correct");
      end;
      declare
         Input    : constant String                        := "Zm8=";
         Result   : constant Stream_Element_Array          := Decode (Input);
         Expected : constant Stream_Element_Array (1 .. 2) :=
           (Character'Pos ('f'), Character'Pos ('o'));
      begin
         Assert (Result'Image, Expected'Image, "Result is not correct");
      end;
      declare
         Input    : constant String                        := "Zm9v";
         Result   : constant Stream_Element_Array          := Decode (Input);
         Expected : constant Stream_Element_Array (1 .. 3) :=
           (Character'Pos ('f'), Character'Pos ('o'), Character'Pos ('o'));
      begin
         Assert (Result'Image, Expected'Image, "Result is not correct");
      end;
      declare
         Input    : constant String                        := "Zm9vYg==";
         Result   : constant Stream_Element_Array          := Decode (Input);
         Expected : constant Stream_Element_Array (1 .. 4) :=
           (Character'Pos ('f'), Character'Pos ('o'), Character'Pos ('o'),
            Character'Pos ('b'));
      begin
         Assert (Result'Image, Expected'Image, "Result is not correct");
      end;
      declare
         Input    : constant String                        := "Zm9vYmE=";
         Result   : constant Stream_Element_Array          := Decode (Input);
         Expected : constant Stream_Element_Array (1 .. 5) :=
           (Character'Pos ('f'), Character'Pos ('o'), Character'Pos ('o'),
            Character'Pos ('b'), Character'Pos ('a'));
      begin
         Assert (Result'Image, Expected'Image, "Result is not correct");
      end;
      declare
         Input    : constant String                        := "Zm9vYmFy";
         Result   : constant Stream_Element_Array          := Decode (Input);
         Expected : constant Stream_Element_Array (1 .. 6) :=
           (Character'Pos ('f'), Character'Pos ('o'), Character'Pos ('o'),
            Character'Pos ('b'), Character'Pos ('a'), Character'Pos ('r'));
      begin
         Assert (Result'Image, Expected'Image, "Result is not correct");
      end;
   end Test_Decoding_RFC4648;
end Test_URLSafe;
