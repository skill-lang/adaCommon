--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     stream to skill tokens                              --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --

with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Interfaces;

with Skill.Types;
with Interfaces.C.Strings;
with System;
with Skill.Errors;
with Interfaces.C_Streams;
with System.Storage_Elements;
with System.Address_To_Access_Conversions;

package body Skill.Streams.Writer is

   use Skill;
   use Interfaces;
   use type System.Address;

   function Open
     (Path : not null Types.String_Access;
      Mode : String) return Output_Stream
   is

      F : Interfaces.C_Streams.FILEs :=
        C_Streams.fopen (Path.all'Address, Mode'Address);

      R : Output_Stream;

      use System.Storage_Elements;
      package Casts is new System.Address_To_Access_Conversions
        (C.unsigned_char);
      function Convert is new Ada.Unchecked_Conversion
        (Casts.Object_Pointer,
         Map_Pointer);
   begin
      if C_Streams.NULL_Stream = F then
         raise Skill.Errors.Skill_Error
           with "failed to write file: " & Path.all;
      end if;

      R :=
        new Output_Stream_T'
          (Path   => Path,
           File   => F,
           Map    => Invalid_Pointer,
           Base   => Invalid_Pointer,
           EOF    => Invalid_Pointer,
           Buffer => <>);

      R.Map  := Convert (Casts.To_Pointer (R.Buffer'Address));
      R.Base := Convert (Casts.To_Pointer (R.Buffer'Address));
      R.EOF  := Convert (Casts.To_Pointer (R.Buffer'Address + R.Buffer'Size));

      return R;
   end Open;

   function Map
     (This : access Output_Stream_T;
      Size : Types.v64) return Sub_Stream
   is
      use type Uchar.Pointer;
      use type Interfaces.Integer_64;

      Map : Uchar.Pointer :=
        MMap_Write_Map (This.File, C.size_t (This.Position), C.size_t (Size));
   begin
      if null = Map then
         raise Skill.Errors.Skill_Error
           with "failed to create map of size " &
           Long_Long_Integer'Image (Long_Long_Integer (Size)) &
           "in file: " &
           This.Path.all;
      end if;

      return new Sub_Stream_T'
          (Map  => Map_Pointer (Map),
           Base => Map_Pointer (Map),
           EOF  => Map_Pointer (Map) + C.ptrdiff_t (Size));
   end Map;
--
--     procedure Free (This : access Output_Stream_T) is
--
--        type S is access all Output_Stream_T;
--        procedure Delete is new Ada.Unchecked_Deallocation (Output_Stream_T, S);
--        D : S := S (This);
--     begin
--        MMap_Close (This.File);
--        Delete (D);
--     end Free;
--
--     procedure Free (This : access Sub_Stream_T) is
--        type S is access all Sub_Stream_T;
--        procedure Delete is new Ada.Unchecked_Deallocation (Sub_Stream_T, S);
--        D : S := S (This);
--     begin
--        Delete (D);
--     end Free;
--
--     function Path
--       (This : access Output_Stream_T) return Skill.Types.String_Access
--     is
--     begin
--        return This.Path;
--     end Path;
--
   function Position
     (This : access Abstract_Stream'Class) return Skill.Types.v64
   is
      use type Map_Pointer;
   begin
      return Types.v64 (This.Map - This.Base);
   end Position;
--
--     procedure Jump
--       (This : access Abstract_Stream'Class;
--        Pos  : Skill.Types.v64)
--     is
--     begin
--        This.Position := Interfaces.C.size_t (Pos);
--     end Jump;
--
--     function Eof (This : access Abstract_Stream'Class) return Boolean is
--        use C;
--     begin
--        return This.Position >= This.Length;
--     end Eof;
--
--     function I8 (This : access Abstract_Stream'Class) return Skill.Types.i8 is
--        use C;
--        use Uchar;
--
--        function Convert is new Ada.Unchecked_Conversion
--          (Interfaces.C.unsigned_char,
--           Skill.Types.i8);
--        P : Uchar.Pointer := This.Map + C.ptrdiff_t (This.Position);
--        R : Types.i8      := Convert (P.all);
--     begin
--        -- Increment (P);
--
--        This.Position := This.Position + 1;
--        return R;
--     end I8;
--
--     function I16 (This : access Abstract_Stream'Class) return Skill.Types.i16 is
--        use C;
--        use Uchar;
--
--        subtype Bytes is Unsigned_Char_Array (1 .. 2);
--        function Convert is new Ada.Unchecked_Conversion (Bytes, Types.i16);
--        P  : Uchar.Pointer := This.Map + C.ptrdiff_t (This.Position);
--        P1 : Uchar.Pointer := P + 1;
--        R  : Types.i16     := Convert (Bytes'(P1.all, P.all));
--     begin
--        This.Position := This.Position + 2;
--        return R;
--     end I16;
--
--     function I32 (This : access Abstract_Stream'Class) return Skill.Types.i32 is
--        use C;
--        use Uchar;
--
--        subtype Bytes is Unsigned_Char_Array (1 .. 4);
--        function Convert is new Ada.Unchecked_Conversion (Bytes, Types.i32);
--        P  : Uchar.Pointer := This.Map + C.ptrdiff_t (This.Position);
--        P1 : Uchar.Pointer := P + 1;
--        P2 : Uchar.Pointer := P + 2;
--        P3 : Uchar.Pointer := P + 3;
--        R  : Types.i32     := Convert (Bytes'(P3.all, P2.all, P1.all, P.all));
--     begin
--        This.Position := This.Position + 4;
--        return R;
--     end I32;
--
--     function I64 (This : access Abstract_Stream'Class) return Skill.Types.i64 is
--        use C;
--        use Uchar;
--
--        subtype Bytes is Unsigned_Char_Array (1 .. 8);
--        function Convert is new Ada.Unchecked_Conversion (Bytes, Types.i64);
--        P  : Uchar.Pointer := This.Map + C.ptrdiff_t (This.Position);
--        P1 : Uchar.Pointer := P + 1;
--        P2 : Uchar.Pointer := P + 2;
--        P3 : Uchar.Pointer := P + 3;
--        P4 : Uchar.Pointer := P + 4;
--        P5 : Uchar.Pointer := P + 5;
--        P6 : Uchar.Pointer := P + 6;
--        P7 : Uchar.Pointer := P + 7;
--        R  : Types.i64     :=
--          Convert
--            (Bytes'
--               (P7.all, P6.all, P5.all, P4.all, P3.all, P2.all, P1.all, P.all));
--     begin
--        This.Position := This.Position + 8;
--        return R;
--     end I64;
--
--     function F32 (This : access Abstract_Stream'Class) return Skill.Types.F32 is
--        use C;
--        use Uchar;
--
--        R  : Types.F32;
--        P  : Uchar.Pointer := This.Map + C.ptrdiff_t (This.Position);
--        for R'Address use P.all'Address;
--        pragma Import (Ada, R);
--     begin
--        This.Position := This.Position + 4;
--        return R;
--     end F32;
--
--     function F64 (This : access Abstract_Stream'Class) return Skill.Types.F64 is
--        use C;
--        use Uchar;
--
--        R : Types.F64;
--        P  : Uchar.Pointer := This.Map + C.ptrdiff_t (This.Position);
--        for R'Address use P.all'Address;
--        pragma Import (Ada, R);
--     begin
--        This.Position := This.Position + 8;
--        return R;
--     end F64;
--
--     -- TODO unroll this loop and try to enable inlining somehow
--     function V64 (This : access Abstract_Stream'Class) return Skill.Types.v64 is
--        pragma Warnings (Off);
--
--        subtype Count_Type is Natural range 0 .. 8;
--        use type Interfaces.Unsigned_64;
--        function Convert is new Ada.Unchecked_Conversion
--          (Source => Types.i8,
--           Target => Types.Uv64);
--        function Convert is new Ada.Unchecked_Conversion
--          (Source => Types.Uv64,
--           Target => Types.v64);
--
--        Count        : Count_Type := 0;
--        Return_Value : Types.Uv64 := 0;
--        Bucket       : Types.Uv64 := Convert (This.I8);
--     begin
--        while (Count < 8 and then 0 /= (Bucket and 16#80#)) loop
--           Return_Value :=
--             Return_Value or
--             Interfaces.Shift_Left (Bucket and 16#7f#, 7 * Count);
--           Count  := Count + 1;
--           Bucket := Convert (This.I8);
--        end loop;
--
--        case Count is
--           when 8 =>
--              Return_Value := Return_Value or Interfaces.Shift_Left (Bucket, 56);
--           when others =>
--              Return_Value :=
--                Return_Value or
--                Interfaces.Shift_Left (Bucket and 16#7f#, 7 * Count);
--        end case;
--
--        return Convert (Return_Value);
--     end V64;
--
--     function Parse_Exception
--       (This          :    access Output_Stream_T;
--        Block_Counter :    Positive;
--        Cause         : in Ada.Exceptions.Exception_Occurrence;
--        Message       :    String) return String
--     is
--     begin
--        return "Parse exception at" &
--          Ada.Characters.Latin_1.LF &
--          This.Path.all &
--          Ada.Characters.Latin_1.LF &
--          " position: " &
--          Long_Integer'Image (Long_Integer (This.Position)) &
--          Ada.Characters.Latin_1.LF &
--          " block: " &
--          Positive'Image (Block_Counter) &
--          Ada.Characters.Latin_1.LF &
--          " reason: " &
--          Message &
--          Ada.Characters.Latin_1.LF &
--          " caused by: " &
--          Ada.Exceptions.Exception_Information (Cause);
--
--     end Parse_Exception;
--     function Parse_Exception
--       (This          : access Output_Stream_T;
--        Block_Counter : Positive;
--        Message       : String) return String
--     is
--     begin
--        return "Parse exception at" &
--          Ada.Characters.Latin_1.LF &
--          This.Path.all &
--          Ada.Characters.Latin_1.LF &
--          " position: " &
--          Long_Integer'Image (Long_Integer (This.Position)) &
--          Ada.Characters.Latin_1.LF &
--          " block: " &
--          Positive'Image (Block_Counter) &
--          Ada.Characters.Latin_1.LF &
--          " reason: " &
--          Message &
--          Ada.Characters.Latin_1.LF;
--
--     end Parse_Exception;
--
end Skill.Streams.Writer;
