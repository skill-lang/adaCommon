--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     stream to skill tokens                              --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --

with Ada.Unchecked_Conversion;
with Interfaces;

with Skill.Types;
with Interfaces.C.Strings;
with Ada.Characters.Latin_1;

package body Skill.Streams.Reader is

   use Skill;

   function Open (Path : Types.String_Access) return Input_Stream is
      Cpath : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (Path.all);

      Map : Mmap := MMap_Open (Cpath);
   begin
      Interfaces.C.Strings.Free (Cpath);
      return new Input_Stream_T'
          (Path     => Path,
           File     => Map.File,
           Length   => Map.Length,
           Map      => Map.Map,
           Position => 0);
   end Open;

   function Path
     (This : access Input_Stream_T) return Skill.Types.String_Access
   is
   begin
      return This.Path;
   end Path;

   function Position (This : access Input_Stream_T) return Skill.Types.v64 is
   begin
      return Types.v64 (This.Position);
   end Position;

   procedure Jump (This : access Input_Stream_T; Pos : Skill.Types.v64) is
   begin
      This.Position := Interfaces.C.size_t (Pos);
   end Jump;

   function Eof (This : access Input_Stream_T) return Boolean is
      use C;
   begin
      return This.Position >= This.Length;
   end Eof;

   function I8 (This : access Input_Stream_T) return Types.i8 is
      use C;
      use Uchar;

      function Convert is new Ada.Unchecked_Conversion
        (Interfaces.C.unsigned_char,
         Skill.Types.i8);
      P : Uchar.Pointer := This.Map + C.ptrdiff_t (This.Position);
      R : Types.i8      := Convert (P.all);
   begin
      -- Increment (P);

      This.Position := This.Position + 1;
      return R;
   end I8;

   function I16 (This : access Input_Stream_T) return Types.i16 is
      use C;
      use Uchar;

      subtype Bytes is Unsigned_Char_Array (1 .. 2);
      function Convert is new Ada.Unchecked_Conversion (Bytes, Types.i16);
      P  : Uchar.Pointer := This.Map + C.ptrdiff_t (This.Position);
      P1 : Uchar.Pointer := P + 1;
      R  : Types.i16     := Convert (Bytes'(P1.all, P.all));
   begin
      This.Position := This.Position + 2;
      return R;
   end I16;

   function I32 (This : access Input_Stream_T) return Types.i32 is
      use C;
      use Uchar;

      subtype Bytes is Unsigned_Char_Array (1 .. 4);
      function Convert is new Ada.Unchecked_Conversion (Bytes, Types.i32);
      P  : Uchar.Pointer := This.Map + C.ptrdiff_t (This.Position);
      P1 : Uchar.Pointer := P + 1;
      P2 : Uchar.Pointer := P + 2;
      P3 : Uchar.Pointer := P + 3;
      R  : Types.i32     := Convert (Bytes'(P3.all, P2.all, P1.all, P.all));
   begin
      This.Position := This.Position + 4;
      return R;
   end I32;

   function I64 (This : access Input_Stream_T) return Types.i64 is
      use C;
      use Uchar;

      subtype Bytes is Unsigned_Char_Array (1 .. 8);
      function Convert is new Ada.Unchecked_Conversion (Bytes, Types.i64);
      P  : Uchar.Pointer := This.Map + C.ptrdiff_t (This.Position);
      P1 : Uchar.Pointer := P + 1;
      P2 : Uchar.Pointer := P + 2;
      P3 : Uchar.Pointer := P + 3;
      P4 : Uchar.Pointer := P + 4;
      P5 : Uchar.Pointer := P + 5;
      P6 : Uchar.Pointer := P + 6;
      P7 : Uchar.Pointer := P + 7;
      R  : Types.i64     :=
        Convert
          (Bytes'
             (P7.all, P6.all, P5.all, P4.all, P3.all, P2.all, P1.all, P.all));
   begin
      This.Position := This.Position + 8;
      return R;
   end I64;

   -- TODO replace by fast variant
   function V64 (This : access Input_Stream_T) return Types.v64 is
      pragma Warnings (Off);

      subtype Count_Type is Natural'Base range 0 .. 8;
      use type Interfaces.C.size_t;
      use type Interfaces.Unsigned_64;
      use Skill.Types;
      function Convert is new Ada.Unchecked_Conversion
        (Source => Types.i8,
         Target => Types.Uv64);
      function Convert is new Ada.Unchecked_Conversion
        (Source => Types.Uv64,
         Target => Types.v64);

      Count        : Count_Type := 0;
      Return_Value : Uv64       := 0;
      Bucket       : Uv64       := Convert (This.I8);
   begin
      while (Count < 8 and then 0 /= (Bucket and 16#80#)) loop
         Return_Value :=
           Return_Value or
           Interfaces.Shift_Left (Bucket and 16#7f#, 7 * Count);
         Count  := Count + 1;
         Bucket := Convert (This.I8);
      end loop;

      case Count is
         when 8 =>
            Return_Value := Return_Value or Interfaces.Shift_Left (Bucket, 56);
         when others =>
            Return_Value :=
              Return_Value or
              Interfaces.Shift_Left (Bucket and 16#7f#, 7 * Count);
      end case;

      return Convert (Return_Value);
   end V64;

--     function Read_String
--       (Mapped : Unsigned_Char_Array;
--        Start  : Interfaces.C.size_t;
--        Length : i32) return String
--     is
--        use type Interfaces.C.size_t;
--
--        Return_Value : String (1 .. Integer (Length));
--     begin
--        for I in Return_Value'Range loop
--           Return_Value (I) :=
--             Character'Val (Mapped (-1 + Start + Interfaces.C.size_t (I)));
--        end loop;
--        return Return_Value;
--     end Read_String;

   function Parse_Exception
     (This          :    access Input_Stream_T;
      Block_Counter :    Positive;
      Cause         : in Ada.Exceptions.Exception_Occurrence;
      Message       :    String) return String
   is
   begin
      return "Parse exception at" &
        Ada.Characters.Latin_1.LF &
        This.Path.all &
        Ada.Characters.Latin_1.LF &
        " position: " &
        Long_Integer'Image (Long_Integer (This.Position)) &
        Ada.Characters.Latin_1.LF &
        " block: " &
        Positive'Image (Block_Counter) &
        Ada.Characters.Latin_1.LF &
        " reason: " &
        Message &
        Ada.Characters.Latin_1.LF &
        " caused by: " &
        Ada.Exceptions.Exception_Information (Cause);

   end Parse_Exception;
   function Parse_Exception
     (This          : access Input_Stream_T;
      Block_Counter : Positive;
      Message       : String) return String
   is
   begin
      return "Parse exception at" &
        Ada.Characters.Latin_1.LF &
        This.Path.all &
        Ada.Characters.Latin_1.LF &
        " position: " &
        Long_Integer'Image (Long_Integer (This.Position)) &
        Ada.Characters.Latin_1.LF &
        " block: " &
        Positive'Image (Block_Counter) &
        Ada.Characters.Latin_1.LF &
        " reason: " &
        Message &
        Ada.Characters.Latin_1.LF;

   end Parse_Exception;

end Skill.Streams.Reader;
