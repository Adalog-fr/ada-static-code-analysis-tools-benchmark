with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

with slip; use slip;

procedure slip_examples
is

   package Int_IO is new Integer_IO(Integer); use Int_IO;

   procedure with_encode_byte
     (
      data   : t_buffer;
      frame  : t_buffer;
      cursor : Natural
     )
   is
   begin
      Null;
   end with_encode_byte;

   procedure with_encode_frame
     (
      data   : in     t_buffer;
      frame  :    out t_buffer;
      cursor :    out Natural
     )
   is
   begin
      encode_frame(data, frame, cursor);
   end with_encode_frame;


   procedure with_decode_byte
     (
      data   :    out t_buffer;
      frame  : in     t_buffer;
      cursor :    out Natural
     )
   is
      slip           : t_slip;
      frame_cursor   : Natural := frame'First;
      decoded_byte   : t_decode_result;
      frame_received : Boolean;
   begin
      cursor := frame'First;
      init(slip);
      loop
         decode_byte(slip, frame(frame_cursor), decoded_byte, frame_received);

         if decoded_byte.has_byte then
            data(cursor) := decoded_byte.byte;
            cursor       := cursor + 1;
         end if;

         frame_cursor := frame_cursor + 1;

         exit when frame_received;
      end loop;
      cursor := cursor - 1;
   end with_decode_byte;

   procedure with_decode_frame
     (
      data     :    out t_buffer;
      frame    : in     t_buffer;
      end_at   :    out Natural;
      cursor   :    out Natural;
      has_data :    out Boolean
     )
   is
   begin
      decode_frame(frame, data, end_at, cursor, has_data);
   end with_decode_frame;

   procedure show_test
    (
     data : t_buffer
    )
   is
      frame          : t_buffer(data'First .. (data'Last + (data'Length * 2) + 2));
      cursor         : Natural;
      decoded        : t_buffer(data'First .. data'Last);
      decoded_cursor : Natural;
      has_data       : Boolean;
   begin
      Put("Data:          ");
      for elem in data'Range loop
         Put(Integer(data(elem)), Width => 6, Base => 16);
         Put(" ");
      end loop;
      Put_Line("");

      Put("Encoded:       ");
      with_encode_frame(data, frame, cursor);
      for elem in frame'First .. cursor loop
         Put(Integer(frame(elem)), Width => 6, Base => 16);
         Put(" ");
      end loop;
      Put_Line("");

      Put("Decoded:       ");
      with_decode_byte(decoded, frame, decoded_cursor);
      for elem in decoded'First .. decoded_cursor loop
         Put(Integer(decoded(elem)), Width => 6, Base => 16);
         Put(" ");
      end loop;
      Put_Line("");
      Put_Line("Correct? :     " & Boolean'(data = decoded)'Image);

      Put("Decoded frame: ");
      with_decode_frame(decoded, frame, cursor, decoded_cursor, has_data);
      if has_data then
         for elem in decoded'First .. decoded_cursor loop
            Put(Integer(decoded(elem)), Width => 6, Base => 16);
            Put(" ");
         end loop;
      end if;
      Put_Line("");
      Put_Line("Correct? :     " & Boolean'(data = decoded)'Image);

   end show_test;

   z1 : t_buffer         := (1,      2, 3, 4);
   z2 : t_buffer         := (1, 16#C0#, 3, 4);
   z3 : t_buffer(2 .. 1) := (others => 16#FF#);
   z4 : t_buffer         := (0 => 99);
begin

   show_test(z1);
   Put_Line("");

   show_test(z2);
   Put_Line("");

   show_test(z3);
   Put_Line("");

   show_test(z4);
end slip_examples;
