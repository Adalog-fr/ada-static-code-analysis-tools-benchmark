with GESTE;
with GESTE.Grid;
pragma Style_Checks (Off);
package Game_Assets.Level_1 is

   --  Level_1
   Width       : constant := 20;
   Height      : constant := 15;
   Tile_Width  : constant := 16;
   Tile_Height : constant := 16;

   --  Back
   package Back is
      Width  : constant :=  20;
      Height : constant :=  20;
      Data   : aliased GESTE.Grid.Grid_Data :=
  (( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 83, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 84, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 85, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 86, 86, 86, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 86, 86, 86, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 86, 86, 86, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 86, 86, 86, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 86, 86, 86, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 87, 88),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 87, 88))      ;
   end Back;

   --  Mid
   package Mid is
      Width  : constant :=  20;
      Height : constant :=  20;
      Data   : aliased GESTE.Grid.Grid_Data :=
  (( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 89, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 89, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 89, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 91, 90, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 91, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 91, 90, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 91, 90, 90, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 89, 90, 90, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 92, 93, 93, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 94, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 95, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 96, 0, 0, 0, 0, 0, 97, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 98, 0, 0, 0, 91, 99, 99, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 100, 0, 0, 0, 89, 90, 90, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 0, 96, 0, 0, 89, 90, 90, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 0, 100, 0, 0, 92, 93, 93, 93, 93, 93),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 101, 0, 0, 0, 102, 88),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 101, 0, 0, 0, 103, 88),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 101, 0, 0, 0, 104, 88),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 91, 99, 99, 99, 99, 99))      ;
   end Mid;

   --  Front
   package Front is
      Width  : constant :=  20;
      Height : constant :=  20;
      Data   : aliased GESTE.Grid.Grid_Data :=
  (( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 105, 0, 0),
         ( 0, 0, 83, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 84, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 85, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 106, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 107, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 108, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 109, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 110, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))      ;
   end Front;

end Game_Assets.Level_1;