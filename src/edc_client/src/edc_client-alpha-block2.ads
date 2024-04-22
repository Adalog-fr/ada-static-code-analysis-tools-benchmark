--===========================================================================
--
--  This package is the interface for the Alpha part of the EDC Client
--  for the second block (the block on the right)
--
--===========================================================================
--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package Edc_Client.Alpha.Block2 is

   --------------------------------------------------------------------------
   --  Shows a Single Letter
   --------------------------------------------------------------------------
   subtype Single_Letter_Positions is Integer range 1 .. 4;
   procedure Show_Single_Letter (Position : Single_Letter_Positions;
                                 Value    : Single_Letter_String)
     with Pre => Initialized;

   --------------------------------------------------------------------------
   --  Shows Double Letters
   --------------------------------------------------------------------------
   subtype Double_Letters_Positions is Integer range 1 .. 2;
   procedure Show_Double_Letters (Position : Double_Letters_Positions;
                                  Value    : Double_Letters_String)
     with Pre => Initialized;

   --------------------------------------------------------------------------
   --  Shows Four Letters
   --------------------------------------------------------------------------
   procedure Show_Four_Letters (Value : Four_Letters_String)
     with Pre => Initialized;

end Edc_Client.Alpha.Block2;
