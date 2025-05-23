--                                                                    --
--  package Test_Sequencer_Sets     Copyright (c)  Dmitry A. Kazakov  --
--  Instantiations                                 Luebeck            --
--                                                 Winter, 2008       --
--                                                                    --
--                                Last revision :  23:22 29 Sep 2017  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.Numerics.Discrete_Random;
with Generic_Random_Sequence;
with Generic_Set;

package Test_Sequencer_Sets is
   type Item is mod 100;
   type Item_Position is range 0..100;

   package Random_Items is new Ada.Numerics.Discrete_Random (Item);
   package Items_Sequence is new Generic_Random_Sequence (Item);

   package Positions_Set is new Generic_Set (Item_Position, 100);

end Test_Sequencer_Sets;
