----------------------------------------------------------------------
--  Rules.Max_Size - Package specification                          --
--                                                                  --
--  This software  is (c) The European Organisation  for the Safety --
--  of Air Navigation (EUROCONTROL) and Adalog 2004-2005.           --
--  The Ada Controller is  free software; you can  redistribute  it --
--  and/or modify it under  terms of the GNU General Public License --
--  as published by the Free Software Foundation; either version 2, --
--  or (at your option) any later version. This unit is distributed --
--  in the hope  that it will be useful,  but WITHOUT ANY WARRANTY; --
--  without even the implied warranty of MERCHANTABILITY or FITNESS --
--  FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License --
--  for more details.   You should have received a  copy of the GNU --
--  General Public License distributed  with this program; see file --
--  COPYING.   If not, write  to the  Free Software  Foundation, 59 --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.           --
--                                                                  --
--  As  a special  exception, if  other files  instantiate generics --
--  from the units  of this program, or if you  link this unit with --
--  other files  to produce  an executable, this  unit does  not by --
--  itself cause the resulting executable  to be covered by the GNU --
--  General  Public  License.   This  exception  does  not  however --
--  invalidate any  other reasons why the executable  file might be --
--  covered by the GNU Public License.                              --
----------------------------------------------------------------------

-- Ada
with
  Ada.Strings.Wide_Unbounded;

-- ASIS
with
  Asis.Elements,
  Asis.Statements,
  Asis.Text;

-- Adalog
with
  A4G_Bugs,
  Utilities;

-- AdaControl
with
  Framework.Language,
  Framework.Rules_Manager;
pragma Elaborate (Framework.Language);


package body Rules.Nothing is
   use Framework;
   use Asis.Text;

   Nb_Nodes  : Natural := Natural'First;
   Rule_Used : Boolean := False;
   Save_Used : Boolean;

   ----------
   -- Help --
   ----------

   procedure Help is
      use Utilities;
   begin
      User_Message  ("Rule: " & Rule_Id);
      User_Message  ("This rule is used to compute the AdaControl overhead. It does nothing.");
   end Help;

   procedure Add_Control (Ctl_Label : in Wide_String; Ctl_Kind  : in Control_Kinds) is
      use Framework.Language, Utilities;
   begin
      Rule_Used                      := True;
   end Add_Control;

   -------------
   -- Command --
   -------------

   procedure Command (Action : Framework.Rules_Manager.Rule_Action) is
      use Framework.Rules_Manager;
   begin
      case Action is
         when Clear =>
            Rule_Used  := False;
         when Suspend =>
            Save_Used := Rule_Used;
            Rule_Used := False;
         when Resume =>
            Rule_Used := Save_Used;
      end case;
   end Command;

   procedure Process_Element (Element : in Asis.Element) is
   begin
      Nb_Nodes := Nb_Nodes + 1;
   end Process_Element;

begin  -- Rules.Nothing
   Framework.Rules_Manager.Register (Rule_Id,
                                     Rules_Manager.Semantic,
                                     Help_CB        => Help'Access,
                                     Add_Control_CB => Add_Control'Access,
                                     Command_CB     => Command'Access);
end Rules.Nothing;
