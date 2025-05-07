with MIDI;
with MIDI.Encoder.Queue;

with System.Storage_Elements; use System.Storage_Elements;
with AAA.Strings;

package Test_Utils is

   type Message_Array is array (Natural range <>) of MIDI.Message;

   function To_Str_Vector (Arr : Message_Array) return AAA.Strings.Vector;

   function Hex_Dump (Data : Storage_Array) return AAA.Strings.Vector;

   function Diff (A, B        : Storage_Array;
                  A_Name      : String := "Expected";
                  B_Name      : String := "Output";
                  Skip_Header : Boolean := False)
                  return AAA.Strings.Vector;

   function Image (D : Storage_Array) return String;

   function To_Storrage_Array (Q : in out MIDI.Encoder.Queue.Instance)
                               return Storage_Array;

end Test_Utils;
