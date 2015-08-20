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

   function Cast is new Ada.Unchecked_Conversion (Types.v64, Types.Uv64);

   function Open
     (Path : not null Types.String_Access;
      Mode : String) return Output_Stream
   is

      F : Interfaces.C_Streams.FILEs :=
        C_Streams.fopen (C.To_C (Path.all)'Address, Mode'Address);

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
          (Path          => Path,
           File          => F,
           Map           => Invalid_Pointer,
           Base          => Invalid_Pointer,
           EOF           => Invalid_Pointer,
           Bytes_Written => 0,
           Buffer        => <>,
           Last_Byte     => Invalid_Pointer);

      R.Map  := Convert (Casts.To_Pointer (R.Buffer'Address));
      R.Base := Convert (Casts.To_Pointer (R.Buffer'Address));
      R.EOF  := Convert (Casts.To_Pointer (R.Buffer'Address + 1024));

      return R;
   end Open;

   function Map
     (This : access Output_Stream_T;
      Size : Types.v64) return Sub_Stream
   is
      use type Uchar.Pointer;
      use type Interfaces.Integer_64;

      Map : Uchar.Pointer;

   begin
      if 0 = Size then
         return new Sub_Stream_T'
             (Map  => Invalid_Pointer,
              Base => Invalid_Pointer,
              EOF  => Invalid_Pointer);
      end if;

      -- Save Our Buffer To Disk
      This.Flush_Buffer;

      Map := MMap_Write_Map (This.File, Size) + C.ptrdiff_t (This.Position);
      if null = Map then
         raise Skill.Errors.Skill_Error
           with "failed to create map of size" &
           Long_Long_Integer'Image (Long_Long_Integer (Size)) &
           " in file: " &
           This.Path.all;
      end if;

      -- Advance File Pointer
      -- @Note: File Position Was Updated By C Code
      This.Bytes_Written := This.Bytes_Written + Size;
      This.Last_Byte     := Map_Pointer (Map) + C.ptrdiff_t (Size);

      return new Sub_Stream_T'
          (Map  => Map_Pointer (Map),
           Base => Map_Pointer (Map),
           EOF  => This.Last_Byte);
   end Map;

   procedure Flush_Buffer (This : access Output_Stream_T) is
      use type Map_Pointer;
      use type C_Streams.size_t;

      package Casts is new System.Address_To_Access_Conversions
        (C.unsigned_char);

      function Convert is new Ada.Unchecked_Conversion
        (Casts.Object_Pointer,
         Map_Pointer);
      function Convert is new Ada.Unchecked_Conversion
        (Map_Pointer,
         Casts.Object_Pointer);

      Length : C_Streams.size_t := C_Streams.size_t (This.Map - This.Base);
   begin

      if 0 /= Length then
         if Length /=
           Interfaces.C_Streams.fwrite
             (Casts.To_Address (Convert (This.Base)),
              1,
              Length,
              This.File)
         then
            raise Skill.Errors.Skill_Error
              with "something went sideways while flushing a buffer";
         end if;

         This.Bytes_Written := This.Bytes_Written + Types.v64 (Length);
         This.Map           := This.Base;
         This.Last_Byte     := Invalid_Pointer;
      end if;
   end Flush_Buffer;

   procedure Close (This : access Output_Stream_T) is

      type S is access all Output_Stream_T;
      procedure Delete is new Ada.Unchecked_Deallocation (Output_Stream_T, S);
      D : S := S (This);

      Exit_Code : Integer;
   begin
      -- do Pending Writes
      This.Flush_Buffer;
      MMap_Flush (This.File, This.Last_Byte);
      Exit_Code := Interfaces.C_Streams.fclose (This.File);
      Delete (D);
   end Close;

   procedure Close (This : access Sub_Stream_T) is
      type S is access all Sub_Stream_T;
      procedure Delete is new Ada.Unchecked_Deallocation (Sub_Stream_T, S);
      D : S := S (This);
   begin
      MMap_Unmap (This.Base, This.EOF);
      Delete (D);
   end Close;

--     function Path
--       (This : access Output_Stream_T) return Skill.Types.String_Access
--     is
--     begin
--        return This.Path;
--     end Path;
--
   function Position (This : access Output_Stream_T) return Skill.Types.v64 is
      use type Map_Pointer;
   begin
      return This.Bytes_Written + Types.v64 (This.Map - This.Base);
   end Position;

   function Position (This : access Sub_Stream_T) return Skill.Types.v64 is
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

   procedure Advance (This : access Abstract_Stream'Class) is
      use C;
      use Uchar;
      use System.Storage_Elements;

      package Casts is new System.Address_To_Access_Conversions
        (C.unsigned_char);

      function Convert is new Ada.Unchecked_Conversion
        (Interfaces.C.unsigned_char,
         Skill.Types.i8);
      function Convert is new Ada.Unchecked_Conversion
        (Casts.Object_Pointer,
         Map_Pointer);
      function Convert is new Ada.Unchecked_Conversion
        (Map_Pointer,
         Casts.Object_Pointer);
   begin
      This.Map :=
        Convert (Casts.To_Pointer (Casts.To_Address (Convert (This.Map)) + 1));
   end Advance;
   pragma Inline (Advance);

   procedure Advance (P : in out Map_Pointer) is
      use C;
      use Uchar;
      use System.Storage_Elements;

      package Casts is new System.Address_To_Access_Conversions
        (C.unsigned_char);

      function Convert is new Ada.Unchecked_Conversion
        (Interfaces.C.unsigned_char,
         Skill.Types.i8);
      function Convert is new Ada.Unchecked_Conversion
        (Casts.Object_Pointer,
         Map_Pointer);
      function Convert is new Ada.Unchecked_Conversion
        (Map_Pointer,
         Casts.Object_Pointer);
   begin
      P := Convert (Casts.To_Pointer (Casts.To_Address (Convert (P)) + 1));
   end Advance;

   procedure Advance
     (P    : in out Map_Pointer;
      Diff :        System.Storage_Elements.Storage_Offset)
   is
      use C;
      use Uchar;
      use System.Storage_Elements;

      package Casts is new System.Address_To_Access_Conversions
        (C.unsigned_char);

      function Convert is new Ada.Unchecked_Conversion
        (Interfaces.C.unsigned_char,
         Skill.Types.i8);
      function Convert is new Ada.Unchecked_Conversion
        (Casts.Object_Pointer,
         Map_Pointer);
      function Convert is new Ada.Unchecked_Conversion
        (Map_Pointer,
         Casts.Object_Pointer);
   begin
      P := Convert (Casts.To_Pointer (Casts.To_Address (Convert (P)) + Diff));
   end Advance;

   procedure Ensure_Size (This : access Output_Stream_T; V : C.ptrdiff_t) is
      use type Map_Pointer;
      use type C.ptrdiff_t;
   begin
      if This.EOF - This.Map < 1 + V then
         This.Flush_Buffer;
      end if;
   end Ensure_Size;

   procedure I8 (This : access Output_Stream_T; V : Skill.Types.i8) is
      function Cast is new Ada.Unchecked_Conversion (Types.i8, Unsigned_8);
   begin
      This.Ensure_Size (1);
      This.Put_Byte (Cast (V));
   end I8;

   procedure I8 (This : access Sub_Stream_T; V : Skill.Types.i8) is
      function Cast is new Ada.Unchecked_Conversion (Types.i8, Unsigned_8);
   begin
      This.Put_Byte (Cast (V));
   end I8;

   procedure Bool (This : access Sub_Stream_T; V : Boolean) is
   begin
      if V then
         This.Put_Byte (16#ff#);
      else
         This.Put_Byte (0);
      end if;
   end Bool;

   procedure I16 (This : access Sub_Stream_T; V : Skill.Types.i16) is
      pragma Warnings (Off);
      use C;
      use Uchar;

      function Cast is new Ada.Unchecked_Conversion (Unsigned_16, Unsigned_8);
      function Cast is new Ada.Unchecked_Conversion (Types.i16, Unsigned_16);
   begin
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 8)));
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 0)));
   end I16;

   procedure I32 (This : access Output_Stream_T; V : Skill.Types.i32) is
      pragma Warnings (Off);
      use C;
      use Uchar;

      function Cast is new Ada.Unchecked_Conversion (Unsigned_32, Unsigned_8);
      function Cast is new Ada.Unchecked_Conversion (Types.i32, Unsigned_32);
   begin
      This.Ensure_Size (4);
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 24)));
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 16)));
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 8)));
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 0)));
   end I32;
   procedure I32 (This : access Sub_Stream_T; V : Skill.Types.i32) is
      use C;
      use Uchar;

      function Cast is new Ada.Unchecked_Conversion (Unsigned_32, Unsigned_8);
      function Cast is new Ada.Unchecked_Conversion (Types.i32, Unsigned_32);
   begin
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 24)));
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 16)));
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 8)));
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 0)));
   end I32;

   procedure I64 (This : access Sub_Stream_T; V : Skill.Types.i64) is
      use C;
      use Uchar;

      function Cast is new Ada.Unchecked_Conversion (Unsigned_64, Unsigned_8);
      function Cast is new Ada.Unchecked_Conversion (Types.i64, Unsigned_64);
   begin
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 56)));
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 48)));
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 40)));
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 32)));
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 24)));
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 16)));
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 8)));
      This.Put_Byte (Cast (Interfaces.Shift_Right (Cast (V), 0)));
   end I64;

   procedure F32 (This : access Sub_Stream_T; V : Skill.Types.F32) is
      use C;
      use Uchar;

      R : Types.F32;
      P : Uchar.Pointer := This.Map + C.ptrdiff_t (This.Position);
      for R'Address use P.all'Address;
      pragma Import (Ada, R);
   begin
      R := V;
      Advance (This.Map, 4);
   end F32;

   procedure F64 (This : access Sub_Stream_T; V : Skill.Types.F64) is
      use C;
      use Uchar;

      R : Types.F64;
      P : Uchar.Pointer := This.Map + C.ptrdiff_t (This.Position);
      for R'Address use P.all'Address;
      pragma Import (Ada, R);
   begin
      R := V;
      Advance (This.Map, 8);
   end F64;

   -- TODO unroll this loop and try to enable inlining somehow
   procedure V64 (This : access Output_Stream_T; Value : Skill.Types.v64) is
      function To_Byte is new Ada.Unchecked_Conversion
        (Skill.Types.Uv64,
         Interfaces.Unsigned_8);
      V : Types.Uv64 := Cast (Value);

      use C;
      use Uchar;
      use System.Storage_Elements;

      package Casts is new System.Address_To_Access_Conversions
        (C.unsigned_char);

      function Convert is new Ada.Unchecked_Conversion
        (Interfaces.C.unsigned_char,
         Skill.Types.i8);
      function Convert is new Ada.Unchecked_Conversion
        (Casts.Object_Pointer,
         Map_Pointer);
      function Convert is new Ada.Unchecked_Conversion
        (Map_Pointer,
         Casts.Object_Pointer);
   begin
      This.Ensure_Size (9);

      if 0 = (V and 16#FFFFFFFFFFFFFF80#) then
         This.Put_Byte (To_Byte (V));
      else
         if 0 = (V and 16#FFFFFFFFFFFFC000#) then
            This.Put_Byte (To_Byte (16#80# or V));
            This.Put_Byte (To_Byte (Interfaces.Shift_Right (V, 7)));
         else
            if 0 = (V and 16#FFFFFFFFFFE00000#) then
               This.Put_Byte (To_Byte (16#80# or V));
               This.Put_Byte
               (To_Byte (16#80# or Interfaces.Shift_Right (V, 7)));
               This.Put_Byte (To_Byte (Interfaces.Shift_Right (V, 14)));
            else
               if 0 = (V and 16#FFFFFFFFF0000000#) then
                  This.Put_Byte (To_Byte (16#80# or V));
                  This.Put_Byte
                  (To_Byte (16#80# or Interfaces.Shift_Right (V, 7)));
                  This.Put_Byte
                  (To_Byte (16#80# or Interfaces.Shift_Right (V, 14)));
                  This.Put_Byte (To_Byte (Interfaces.Shift_Right (V, 21)));
               else
                  if 0 = (V and 16#FFFFFFF800000000#) then
                     This.Put_Byte (To_Byte (16#80# or V));
                     This.Put_Byte
                     (To_Byte (16#80# or Interfaces.Shift_Right (V, 7)));
                     This.Put_Byte
                     (To_Byte (16#80# or Interfaces.Shift_Right (V, 14)));
                     This.Put_Byte
                     (To_Byte (16#80# or Interfaces.Shift_Right (V, 21)));
                     This.Put_Byte (To_Byte (Interfaces.Shift_Right (V, 28)));
                  else
                     if 0 = (V and 16#FFFFFC0000000000#) then
                        This.Put_Byte (To_Byte (16#80# or V));
                        This.Put_Byte
                        (To_Byte (16#80# or Interfaces.Shift_Right (V, 7)));
                        This.Put_Byte
                        (To_Byte (16#80# or Interfaces.Shift_Right (V, 14)));
                        This.Put_Byte
                        (To_Byte (16#80# or Interfaces.Shift_Right (V, 21)));
                        This.Put_Byte
                        (To_Byte (16#80# or Interfaces.Shift_Right (V, 28)));
                        This.Put_Byte
                        (To_Byte (Interfaces.Shift_Right (V, 35)));
                     else
                        if 0 = (V and 16#FFFE000000000000#) then
                           This.Put_Byte (To_Byte (16#80# or V));
                           This.Put_Byte
                           (To_Byte (16#80# or Interfaces.Shift_Right (V, 7)));
                           This.Put_Byte
                           (To_Byte
                              (16#80# or Interfaces.Shift_Right (V, 14)));
                           This.Put_Byte
                           (To_Byte
                              (16#80# or Interfaces.Shift_Right (V, 21)));
                           This.Put_Byte
                           (To_Byte
                              (16#80# or Interfaces.Shift_Right (V, 28)));
                           This.Put_Byte
                           (To_Byte
                              (16#80# or Interfaces.Shift_Right (V, 35)));
                           This.Put_Byte
                           (To_Byte (Interfaces.Shift_Right (V, 42)));
                        else
                           if 0 = (V and 16#FF00000000000000#) then
                              This.Put_Byte (To_Byte (16#80# or V));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 7)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 14)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 21)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 28)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 35)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 42)));
                              This.Put_Byte
                              (To_Byte (Interfaces.Shift_Right (V, 49)));
                           else
                              This.Put_Byte (To_Byte (16#80# or V));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 7)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 14)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 21)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 28)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 35)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 42)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 49)));
                              This.Put_Byte
                              (To_Byte (Interfaces.Shift_Right (V, 56)));
                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end V64;
   procedure V64 (This : access Sub_Stream_T; Value : Skill.Types.v64) is
      function To_Byte is new Ada.Unchecked_Conversion
        (Skill.Types.Uv64,
         Interfaces.Unsigned_8);
      V : Types.Uv64 := Cast (Value);

      use C;
      use Uchar;
      use System.Storage_Elements;

      package Casts is new System.Address_To_Access_Conversions
        (C.unsigned_char);

      function Convert is new Ada.Unchecked_Conversion
        (Interfaces.C.unsigned_char,
         Skill.Types.i8);
      function Convert is new Ada.Unchecked_Conversion
        (Casts.Object_Pointer,
         Map_Pointer);
      function Convert is new Ada.Unchecked_Conversion
        (Map_Pointer,
         Casts.Object_Pointer);
   begin
      if 0 = (V and 16#FFFFFFFFFFFFFF80#) then
         This.Put_Byte (To_Byte (V));
      else
         if 0 = (V and 16#FFFFFFFFFFFFC000#) then
            This.Put_Byte (To_Byte (16#80# or V));
            This.Put_Byte (To_Byte (Interfaces.Shift_Right (V, 7)));
         else
            if 0 = (V and 16#FFFFFFFFFFE00000#) then
               This.Put_Byte (To_Byte (16#80# or V));
               This.Put_Byte
               (To_Byte (16#80# or Interfaces.Shift_Right (V, 7)));
               This.Put_Byte (To_Byte (Interfaces.Shift_Right (V, 14)));
            else
               if 0 = (V and 16#FFFFFFFFF0000000#) then
                  This.Put_Byte (To_Byte (16#80# or V));
                  This.Put_Byte
                  (To_Byte (16#80# or Interfaces.Shift_Right (V, 7)));
                  This.Put_Byte
                  (To_Byte (16#80# or Interfaces.Shift_Right (V, 14)));
                  This.Put_Byte (To_Byte (Interfaces.Shift_Right (V, 21)));
               else
                  if 0 = (V and 16#FFFFFFF800000000#) then
                     This.Put_Byte (To_Byte (16#80# or V));
                     This.Put_Byte
                     (To_Byte (16#80# or Interfaces.Shift_Right (V, 7)));
                     This.Put_Byte
                     (To_Byte (16#80# or Interfaces.Shift_Right (V, 14)));
                     This.Put_Byte
                     (To_Byte (16#80# or Interfaces.Shift_Right (V, 21)));
                     This.Put_Byte (To_Byte (Interfaces.Shift_Right (V, 28)));
                  else
                     if 0 = (V and 16#FFFFFC0000000000#) then
                        This.Put_Byte (To_Byte (16#80# or V));
                        This.Put_Byte
                        (To_Byte (16#80# or Interfaces.Shift_Right (V, 7)));
                        This.Put_Byte
                        (To_Byte (16#80# or Interfaces.Shift_Right (V, 14)));
                        This.Put_Byte
                        (To_Byte (16#80# or Interfaces.Shift_Right (V, 21)));
                        This.Put_Byte
                        (To_Byte (16#80# or Interfaces.Shift_Right (V, 28)));
                        This.Put_Byte
                        (To_Byte (Interfaces.Shift_Right (V, 35)));
                     else
                        if 0 = (V and 16#FFFE000000000000#) then
                           This.Put_Byte (To_Byte (16#80# or V));
                           This.Put_Byte
                           (To_Byte (16#80# or Interfaces.Shift_Right (V, 7)));
                           This.Put_Byte
                           (To_Byte
                              (16#80# or Interfaces.Shift_Right (V, 14)));
                           This.Put_Byte
                           (To_Byte
                              (16#80# or Interfaces.Shift_Right (V, 21)));
                           This.Put_Byte
                           (To_Byte
                              (16#80# or Interfaces.Shift_Right (V, 28)));
                           This.Put_Byte
                           (To_Byte
                              (16#80# or Interfaces.Shift_Right (V, 35)));
                           This.Put_Byte
                           (To_Byte (Interfaces.Shift_Right (V, 42)));
                        else
                           if 0 = (V and 16#FF00000000000000#) then
                              This.Put_Byte (To_Byte (16#80# or V));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 7)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 14)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 21)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 28)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 35)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 42)));
                              This.Put_Byte
                              (To_Byte (Interfaces.Shift_Right (V, 49)));
                           else
                              This.Put_Byte (To_Byte (16#80# or V));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 7)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 14)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 21)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 28)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 35)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 42)));
                              This.Put_Byte
                              (To_Byte
                                 (16#80# or Interfaces.Shift_Right (V, 49)));
                              This.Put_Byte
                              (To_Byte (Interfaces.Shift_Right (V, 56)));
                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end V64;
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
   procedure Put_Plain_String
     (This : access Output_Stream_T;
      V    : Skill.Types.String_Access)
   is
      function Cast is new Ada.Unchecked_Conversion (Character, Unsigned_8);
   begin
      for C of V.all loop
         This.Put_Byte (Cast (C));
      end loop;
   end Put_Plain_String;

   procedure Put_Byte (This : access Abstract_Stream'Class; V : Unsigned_8) is
   begin
      This.Map.all := C.unsigned_char (V);
      This.Advance;
   end Put_Byte;
end Skill.Streams.Writer;
