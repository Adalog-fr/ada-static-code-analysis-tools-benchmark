with GESTE;
with GESTE.Maths_Types;
with GESTE_Config;

pragma Style_Checks (Off);
package Game_Assets is


   Palette : aliased GESTE.Palette_Type := (
      0 =>  0,
      1 =>  12548,
      2 =>  29519,
      3 =>  65535,
      4 =>  59192,
      5 =>  46291,
      6 =>  19019,
      7 =>  50611,
      8 =>  29812,
      9 =>  36181,
      10 =>  44633,
      11 =>  27170,
      12 =>  41925,
      13 =>  39748,
      14 =>  33475,
      15 =>  8424,
      16 =>  27702,
      17 =>  10602,
      18 =>  19154,
      19 =>  29942,
      20 =>  26784,
      21 =>  37348,
      22 =>  39588,
      23 =>  33188,
      24 =>  46184,
      25 =>  23409,
      26 =>  16608,
      27 =>  29317,
      28 =>  39782,
      29 =>  29188,
      30 =>  18754,
      31 =>  12448,
      32 =>  6275,
      33 =>  22950,
      34 =>  37382,
      35 =>  47977,
      36 =>  50315,
      37 =>  45672,
      38 =>  4194,
      39 =>  10534,
      40 =>  35364,
      41 =>  12418,
      42 =>  18658,
      43 =>  8322,
      44 =>  20802,
      45 =>  44598,
      46 =>  52719,
      47 =>  16579,
      48 =>  20900,
      49 =>  12548,
      50 =>  20739,
      51 =>  16907,
      52 =>  20904,
      53 =>  43529,
      54 =>  57831,
      55 =>  8453,
      56 =>  64168,
      57 =>  64455,
      58 =>  64809,
      59 =>  388,
      60 =>  4805,
      61 =>  15328,
      62 =>  4292,
      63 =>  34048,
      64 =>  25885,
      65 =>  17012,
      66 =>  23451,
      67 =>  12548,
      68 =>  10467,
      69 =>  24995,
      70 =>  14626,
      71 =>  65194,
      72 =>  31923,
      73 =>  14370,
      74 =>  16481,
      75 =>  30977,
      76 =>  54286,
      77 =>  10500,
      78 =>  34367,
      79 =>  63488,
      80 =>  22819,
      81 =>  26977,
      82 =>  39521,
      83 =>  47973,
      84 =>  54570,
      85 =>  65056,
      86 =>  50213,
      87 =>  10436,
      88 =>  21164,
      89 =>  65206,
      90 =>  44440,
      91 =>  60686,
      92 =>  39398,
      93 =>  8521,
      94 =>  22205,
      95 =>  63455,
      96 =>  23442,
      97 =>  34199,
      98 =>  10729,
      99 =>  54316,
      100 =>  21558,
      101 =>  29650,
      102 =>  59327,
      103 =>  14862,
      104 =>  44698,
      105 =>  65404,
      106 =>  35462,
      107 =>  39719,
      108 =>  44074,
      109 =>  24964,
      110 =>  29254,
      111 =>  11270,
      112 =>  19590,
      113 =>  25893,
      114 =>  840,
      115 =>  518,
      116 =>  39753,
      117 =>  43945,
      118 =>  48170,
      119 =>  50346,
      120 =>  1659,
      121 =>  1238,
      122 =>  4979,
      123 =>  10468,
      124 =>  18853,
      125 =>  14661,
      126 =>  39952,
      127 =>  35561,
      128 =>  12548,
      129 =>  10500,
      130 =>  18724,
      131 =>  22916,
      132 =>  31334,
      133 =>  27109,
      134 =>  35527,
      135 =>  2405,
      136 =>  518,
      137 =>  2789,
      138 =>  14761);

   type Object_Kind is (Rectangle_Obj, Point_Obj,
     Ellipse_Obj, Polygon_Obj, Tile_Obj, Text_Obj);

   type String_Access is access all String;

   type Object
     (Kind : Object_Kind := Rectangle_Obj)
   is record
      Name    : String_Access;
      Id      : Natural;
      X       : GESTE.Maths_Types.Value;
      Y       : GESTE.Maths_Types.Value;
      Width   : GESTE.Maths_Types.Value;
      Height  : GESTE.Maths_Types.Value;
      Str     : String_Access;
      Tile_Id : GESTE_Config.Tile_Index;
   end record;

   type Object_Array is array (Natural range <>)
      of Object;

end Game_Assets;