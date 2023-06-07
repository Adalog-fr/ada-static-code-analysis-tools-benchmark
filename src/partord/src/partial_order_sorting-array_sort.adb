with Ada.Containers.Doubly_Linked_Lists;
--  with Ada.Text_IO; use Ada.Text_IO;

package body Partial_Order_Sorting.Array_Sort is
   package Index_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Index_Type);

   type Node_Descriptor is
      record
         N_Parents : Natural;
         Children  : Index_Lists.List;
      end record;

   type Precedence_Graph is array (Index_Type range <>) of Node_Descriptor;

   function Make_Graph (From : Container_Type) return Precedence_Graph
   is
      Result : Precedence_Graph (From'Range) :=
                 (others => Node_Descriptor'(N_Parents => 0,
                                             Children  => Index_Lists.Empty_List));

      procedure Make_Edge (Parent, Child : Index_Type) is
      begin
         Result (Parent).Children.Append (Child);
         Result (Child).N_Parents := Result (Child).N_Parents + 1;
      end Make_Edge;
   begin
      for I in From'First .. From'Last - 1 loop
         for J in I + 1 .. From'Last loop
            if From (I) < From (J) then
               Make_Edge (Parent => J, Child  => I);

            elsif From (J) < From (I) then
               Make_Edge (Parent => I, Child  => J);

            end if;
         end loop;
      end loop;

      return Result;
   end Make_Graph;

   function Heads_Of (Graph : Precedence_Graph) return Index_Lists.List
   is
      Result : Index_Lists.List;
   begin
      for I in Graph'Range loop
         if Graph (I).N_Parents = 0 then
            Result.Append (I);
         end if;
      end loop;

      return Result;
   end Heads_Of;

   function Sort (Item : Container_Type) return Container_Type
   is

      Graph  : Precedence_Graph := Make_Graph (Item);
      Heads  : Index_Lists.List := Heads_Of (Graph);

      Result : Container_Type (Item'Range);
      Cursor : Index_Type := Result'First;

      ------------
      -- Append --
      ------------

      procedure Append (Item : Element_Type) is
      begin
         Result (Cursor) := Item;
         Cursor := Cursor + 1;
      end Append;


   begin
      while not Heads.Is_Empty loop
         declare
            Current_Head : constant Index_Type := Heads.First_Element;
         begin
            Heads.Delete_First;

            Append (Item (Current_Head));

            for Child of Graph (Current_Head).Children loop
               Graph (Child).N_Parents := Graph (Child).N_Parents - 1;

               if Graph (Child).N_Parents = 0 then
                  pragma Assert (Cursor <= Result'Last);
                  Heads.Append (Child);
               end if;
            end loop;
         end;
      end loop;

      --  for X of Result loop
      --     Put_Line (Image (X));
      --  end loop;
      --
      --  for  I in Result'Range loop
      --     for J in Result'Range loop
      --        Put_Line (I'Image
      --                  & J'Image
      --                  & Image(Result (I))
      --                  & Image(Result (J))
      --                  & Boolean'Image (Result (I) < Result (J))
      --                  & " "
      --                  & Boolean'Image(if Result (I) < Result (J) then I < J));
      --     end loop;
      --  end loop;

      pragma Assert (Cursor = Result'Last + 1);
      return Result;
   end Sort;

   procedure Sort (Item : in out Container_Type)
   is
   begin
      Item := Sort (Item);
   end Sort;

end Partial_Order_Sorting.Array_Sort;
