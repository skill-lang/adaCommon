--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     stream to skill tokens                              --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --

with Ada.Unchecked_Conversion;
with Interfaces;

with Skill.Types;
with Interfaces.C.Strings;

package body Skill.Streams.Reader is

   function Open (Path : Skill.Types.String_Access) return Input_Stream is
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

   function Eof (This : Input_Stream_T) return Boolean is
      use C;
   begin
      return This.Position >= This.Length;
   end Eof;

   function I8
     (This : access Input_Stream_T) return Skill.Types.i8
   is
      use C;
      use Uchar;

      function Convert is new Ada.Unchecked_Conversion
        (Interfaces.C.unsigned_char,
         Skill.Types.i8);
      P : Uchar.Pointer := This.Map + C.Ptrdiff_T(This.Position);
      R : Skill.Types.I8 := Convert (P.all);
      begin
      -- Increment (P);

      This.Position := This.Position + 1;
      return R;
   end I8;

--     function Read_i16
--       (Mapped : Unsigned_Char_Array;
--        Start  : Interfaces.C.size_t) return i16
--     is
--        use type Interfaces.C.size_t;
--        subtype Two_Bytes is Unsigned_Char_Array (1 .. 2);
--        function Convert is new Ada.Unchecked_Conversion (Two_Bytes, i16);
--     begin
--        return Convert (Two_Bytes'(Mapped (Start + 1), Mapped (Start)));
--     end Read_i16;
--
--     function Read_i32
--       (Mapped : Unsigned_Char_Array;
--        Start  : Interfaces.C.size_t) return i32
--     is
--        use type Interfaces.C.size_t;
--        subtype Four_Bytes is Unsigned_Char_Array (1 .. 4);
--        function Convert is new Ada.Unchecked_Conversion (Four_Bytes, i32);
--     begin
--        return Convert
--            (Four_Bytes'
--               (Mapped (Start + 3),
--                Mapped (Start + 2),
--                Mapped (Start + 1),
--                Mapped (Start)));
--     end Read_i32;
--
--     function Read_i64
--       (Mapped : Unsigned_Char_Array;
--        Start  : Interfaces.C.size_t) return i64
--     is
--        use type Interfaces.C.size_t;
--        subtype Eight_Bytes is Unsigned_Char_Array (1 .. 8);
--        function Convert is new Ada.Unchecked_Conversion (Eight_Bytes, i64);
--     begin
--        return Convert
--            (Eight_Bytes'
--               (Mapped (Start + 7),
--                Mapped (Start + 6),
--                Mapped (Start + 5),
--                Mapped (Start + 4),
--                Mapped (Start + 3),
--                Mapped (Start + 2),
--                Mapped (Start + 1),
--                Mapped (Start)));
--     end Read_i64;
--
--     function Read_v64
--       (Mapped : Unsigned_Char_Array;
--        Start  : Interfaces.C.size_t) return v64_Extended
--     is
--        subtype Count_Type is Natural'Base range 0 .. 8;
--        subtype Unsigned_64 is Interfaces.Unsigned_64;
--        use type Interfaces.C.size_t;
--        use type Interfaces.Unsigned_64;
--        function Convert is new Ada.Unchecked_Conversion (Unsigned_64, v64);
--
--        Count        : Count_Type  := 0;
--        Return_Value : Unsigned_64 := 0;
--        Bucket       : Unsigned_64 := Unsigned_64 (Mapped (Start));
--     begin
--        while (Count < 8 and then 0 /= (Bucket and 16#80#)) loop
--           Return_Value :=
--             Return_Value or
--             Interfaces.Shift_Left (Bucket and 16#7f#, 7 * Count);
--           Count  := Count + 1;
--           Bucket := Unsigned_64 (Mapped (Start + Interfaces.C.size_t (Count)));
--        end loop;
--
--        case Count is
--           when 8 =>
--              Return_Value :=
--                Return_Value or Interfaces.Shift_Left (Bucket, 7 * Count);
--           when others =>
--              Return_Value :=
--                Return_Value or
--                Interfaces.Shift_Left (Bucket and 16#7f#, 7 * Count);
--        end case;
--
--        return v64_Extended'
--            (Convert (Return_Value), 1 + Interfaces.C.size_t (Count));
--     end Read_v64;
--
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

end Skill.Streams.Reader;
