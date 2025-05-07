--===========================================================================
--
--  This package is the interface the Alpha internal procedures/functions
--  for avoiding duplicating code
--
--===========================================================================
--
--  Copyright 2021 (C) Holger Rodriguez
--
--  SPDX-License-Identifier: BSD-3-Clause
--
private package Edc_Client.Alpha.Common is

   --------------------------------------------------------------------------
   --  Shows a Single Letter
   --------------------------------------------------------------------------
   procedure Show_Single_Letter (Position : Integer;
                                 Block    : Character;
                                 Value    : String);

   --------------------------------------------------------------------------
   --  Shows Double Letters
   --------------------------------------------------------------------------
   procedure Show_Double_Letters (Position : Integer;
                                  Block    : Character;
                                  Value    : String);

   --------------------------------------------------------------------------
   --  Shows Four Letters
   --------------------------------------------------------------------------
   procedure Show_Four_Letters (Position : Integer;
                                Block    : Character;
                                Value    : String);

end Edc_Client.Alpha.Common;
