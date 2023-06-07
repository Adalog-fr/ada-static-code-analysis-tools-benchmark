with Interfaces; use Interfaces;

package slip
with
  SPARK_Mode => On
is

   type t_slip is private;

   type t_buffer is array (Natural range <>) of Unsigned_8;

   type t_decode_result
     (
      has_byte : Boolean := False
     )
   is
      record
         case has_byte is
            when True =>
               byte: Unsigned_8;
            when False =>
               null;
         end case;
      end record;

   function end_byte_get return Unsigned_8 with Ghost;

   function is_in_frame(ctx : in t_slip) return Boolean with Ghost;

   procedure encode_byte
     (
      byte   : in     Unsigned_8;
      frame  : in out t_buffer;
      cursor :    out Natural
     )
       with
         Pre => frame'Length >= 2,
         Post => (cursor = frame'First) or (cursor = frame'First + 1);

   procedure decode_byte
     (
      ctx        : in out t_slip;
      input      : in     Unsigned_8;
      data       :    out t_decode_result;
      frame_done :    out Boolean
     )
       with
         Pre => not data'Constrained,
         Post => (if ((input = end_byte_get) and (is_in_frame(ctx)'Old = true)) then frame_done = true);

   procedure start_frame
     (
      frame  : in out t_buffer;
      cursor :    out Natural
     )
       with
         Pre => (frame'Length >= 1),
         Post => (cursor = frame'First);

   procedure end_frame
     (
      frame  : in out t_buffer;
      cursor :    out Natural
     )
       with
         Pre => (frame'Length >= 1),
         Post => (cursor = frame'First);

   procedure encode_frame
     (
      data   : in     t_buffer;
      frame  : in out t_buffer;
      cursor :    out Natural
     )
       with
         Pre  => (frame'Length > ((data'Length * 2) + 2)),
         Post => (cursor - frame'First) >= data'Length;

   procedure decode_frame
     (
      frame    : in     t_buffer;
      data     : in out t_buffer;
      end_at   :    out Natural;
      cursor   :    out Natural;
      has_data :    out Boolean
     )
       with
         Pre  => (data'Length > 0) and
                 (frame'Length >= 2) and
                 (data'Length >= (frame'Length + 2)),
         Post => (cursor >= data'First) and
                 (cursor <= data'Last);

   procedure init
     (
      ctx : out t_slip
     );

private

   type t_slip is
      record
         in_frame  : Boolean;
         last_byte : Unsigned_8;
      end record;

end slip;
