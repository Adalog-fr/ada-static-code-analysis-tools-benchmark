with GESTE;
with GESTE.Grid;
pragma Style_Checks (Off);
package Game_Assets.Level_2 is

   --  Level_2
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
         ( 0, 0, 84, 0, 0, 0, 0, 0, 0, 90, 0, 0, 0, 0, 0),
         ( 0, 0, 85, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 90, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 86, 86, 86, 86, 86, 86, 86, 86, 86, 0),
         ( 0, 0, 0, 0, 0, 86, 86, 86, 86, 86, 86, 86, 86, 86, 0),
         ( 0, 0, 0, 0, 0, 86, 86, 86, 86, 86, 86, 86, 86, 86, 0),
         ( 0, 0, 0, 0, 0, 86, 86, 86, 86, 86, 86, 86, 86, 86, 0),
         ( 0, 0, 0, 0, 0, 86, 86, 86, 0, 0, 0, 86, 86, 86, 0),
         ( 0, 0, 0, 0, 0, 86, 86, 86, 0, 0, 0, 86, 86, 86, 0),
         ( 0, 0, 0, 0, 0, 86, 86, 86, 86, 90, 86, 86, 86, 86, 0),
         ( 0, 0, 0, 0, 0, 86, 86, 86, 86, 86, 86, 86, 86, 86, 0),
         ( 0, 0, 0, 0, 0, 90, 86, 86, 86, 86, 86, 86, 86, 86, 0),
         ( 0, 0, 0, 0, 90, 0, 86, 86, 86, 86, 86, 86, 90, 0, 0),
         ( 0, 0, 0, 109, 0, 0, 86, 86, 86, 86, 86, 90, 0, 0, 0),
         ( 0, 0, 0, 110, 0, 0, 86, 86, 86, 86, 90, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))      ;
   end Back;

   --  Mid
   package Mid is
      Width  : constant :=  20;
      Height : constant :=  20;
      Data   : aliased GESTE.Grid.Grid_Data :=
  (( 0, 0, 0, 0, 0, 0, 0, 0, 0, 89, 90, 90, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 89, 90, 90, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 89, 90, 90, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 91, 99, 111, 90, 90, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 89, 90, 90, 90, 90, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 91, 99, 111, 90, 90, 90, 90, 90, 90, 90),
         ( 0, 0, 0, 0, 0, 92, 93, 93, 93, 93, 93, 93, 93, 93, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 90),
         ( 0, 0, 0, 0, 0, 112, 113, 113, 99, 99, 99, 114, 0, 0, 90),
         ( 0, 0, 0, 0, 0, 111, 0, 0, 89, 90, 90, 115, 0, 0, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 89, 90, 90, 116, 0, 0, 90),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 92, 117, 116, 0, 0, 0, 90),
         ( 0, 0, 0, 0, 0, 118, 0, 0, 0, 119, 0, 0, 0, 0, 90),
         ( 0, 0, 0, 0, 91, 111, 120, 0, 0, 0, 0, 0, 91, 99, 90),
         ( 0, 0, 0, 91, 111, 90, 115, 0, 0, 0, 0, 91, 111, 90, 90),
         ( 0, 0, 0, 89, 90, 90, 115, 0, 0, 0, 91, 111, 90, 90, 90),
         ( 0, 0, 0, 89, 90, 90, 90, 99, 99, 99, 111, 90, 90, 90, 90),
         ( 0, 0, 0, 89, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90),
         ( 0, 0, 0, 89, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90))      ;
   end Mid;

   --  Front
   package Front is
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
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 121, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 122, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 105, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))      ;
   end Front;

end Game_Assets.Level_2;