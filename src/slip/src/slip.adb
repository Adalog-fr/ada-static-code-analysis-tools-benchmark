package body slip
with
  SPARK_Mode => On
is

   SLIP_END     : constant Unsigned_8 := 16#C0#;
   SLIP_ESC     : constant Unsigned_8 := 16#DB#;
   SLIP_ESC_END : constant Unsigned_8 := 16#DC#;
   SLIP_ESC_ESC : constant Unsigned_8 := 16#DD#;

   function end_byte_get return Unsigned_8 is (SLIP_END);

   function is_in_frame(ctx : in t_slip) return Boolean is (ctx.in_frame);

   procedure encode_byte
     (
      byte   : in     Unsigned_8;
      frame  : in out t_buffer;
      cursor :    out Natural
     )
   is
   begin
      cursor := frame'First;

      if byte = SLIP_END then
         frame(cursor) := SLIP_ESC;
         cursor        := cursor + 1;
         frame(cursor) := SLIP_ESC_END;
      elsif byte = SLIP_ESC then
         frame(cursor) := SLIP_ESC;
         cursor        := cursor + 1;
         frame(cursor) := SLIP_ESC_ESC;
      else
         frame(cursor) := byte;
      end if;
   end encode_byte;

   procedure decode_byte
     (
      ctx        : in out t_slip;
      input      : in     Unsigned_8;
      data       :    out t_decode_result;
      frame_done :    out Boolean
     )
   is
      byte : Unsigned_8;
   begin
      frame_done := False;
      data       := (has_byte => False);

      if input = SLIP_ESC then
         ctx.last_byte := SLIP_ESC;
      elsif input = SLIP_END then
         ctx.last_byte := SLIP_END;

         if ctx.in_frame then
            ctx.in_frame := False;
            frame_done   := True;
         else
            ctx.in_frame := True;
         end if;
      else
         byte := input;

         if ctx.last_byte = SLIP_ESC then
            if    input = SLIP_ESC_END then byte := SLIP_END;
            elsif input = SLIP_ESC_ESC then byte := SLIP_ESC;
            end if;
         end if;

         data          := (has_byte => True, byte => byte);
         ctx.last_byte := input;
      end if;
   end decode_byte;

   procedure start_frame
     (
      frame  : in out t_buffer;
      cursor :    out Natural
     )
   is
   begin
      cursor        := frame'First;
      frame(cursor) := SLIP_END;
   end start_frame;

   procedure end_frame
     (
      frame  : in out t_buffer;
      cursor :    out Natural
     )
   is
   begin
      cursor        := frame'First;
      frame(cursor) := SLIP_END;
   end end_frame;

   procedure encode_frame
     (
      data   : in     t_buffer;
      frame  : in out t_buffer;
      cursor :    out Natural
     )
   is
   begin
      cursor        := frame'First;
      frame(cursor) := SLIP_END;

      for i in data'Range loop
         cursor := cursor + 1;
         encode_byte(data(i), frame(cursor .. frame'Last), cursor);

         pragma Loop_Invariant ((cursor - frame'First) > (i - data'First));
         pragma Loop_Invariant ((cursor - frame'First) <= ((i - data'First + 1) * 2) + 1);
      end loop;

      cursor        := cursor + 1;
      frame(cursor) := SLIP_END;
   end encode_frame;

   procedure decode_frame
     (
      frame    : in     t_buffer;
      data     : in out t_buffer;
      end_at   :    out Natural;
      cursor   :    out Natural;
      has_data :    out Boolean
     )
   is
      ctx          : t_slip;
      result       : t_decode_result;
      frame_done   : Boolean;
      loop_started : Boolean := false;
   begin
      init(ctx);

      has_data := False;
      end_at   := frame'First;
      cursor   := data'First;

      for i in frame'Range loop
         decode_byte(ctx, frame(i), result, frame_done);

         if result.has_byte then
            has_data := true;

            if loop_started then
               cursor := cursor + 1;
            else
               loop_started := true;
            end if;

            data(cursor) := result.byte;
         end if;

         if frame_done then
            end_at := i;
            exit;
         end if;

         pragma Loop_Invariant (cursor >= data'First);
         pragma Loop_Invariant ((i - frame'First) >= (cursor - data'First));
      end loop;
   end decode_frame;

   procedure init
     (
      ctx : out t_slip
     )
   is
   begin
      ctx.in_frame  := False;
      ctx.last_byte := 0;
   end init;

end slip;
