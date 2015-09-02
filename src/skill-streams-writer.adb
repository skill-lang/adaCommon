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
with System.Address_Image;
with Ada.Text_IO;

package body Skill.Streams.Writer is

   use Skill;
   use Interfaces;
   use type System.Address;

   --- common type conversions ---
-- @note they are required at this level, because otherwise inlining will crash

   function Cast is new Ada.Unchecked_Conversion (Types.v64, Types.Uv64);

   package Casts is new System.Address_To_Access_Conversions (C.unsigned_char);

   function Convert is new Ada.Unchecked_Conversion
     (Casts.Object_Pointer,
      Map_Pointer);
   function Convert is new Ada.Unchecked_Conversion
     (Map_Pointer,
      Casts.Object_Pointer);

   function Open
     (Path : not null Types.String_Access;
      Mode : String) return Output_Stream
   is

      F : Interfaces.C_Streams.FILEs :=
        C_Streams.fopen (C.To_C (Path.all)'Address, Mode'Address);

      R : Output_Stream;

      use System.Storage_Elements;
   begin
      if C_Streams.NULL_Stream = F then
         raise Skill.Errors.Skill_Error
           with "failed to write file: " & Path.all;
      end if;

      R :=
        new Output_Stream_T'
          (Path           => Path,
           File           => F,
           Map            => Invalid_Pointer,
           Base           => Invalid_Pointer,
           EOF            => Invalid_Pointer,
           Bytes_Written  => 0,
           Buffer         => <>,
           Block_Map_Mode => False,
           Client_Map     => Invalid_Pointer,
           Client_Base    => Invalid_Pointer,
           Client_EOF     => Invalid_Pointer);

      R.Map  := Convert (Casts.To_Pointer (R.Buffer'Address));
      R.Base := Convert (Casts.To_Pointer (R.Buffer'Address));
      R.EOF  := Convert (Casts.To_Pointer (R.Buffer'Address + 1024));

      return R;
   end Open;

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
      end if;
   end Flush_Buffer;

   -- creates a map for a block and enables usage of map function
   procedure Begin_Block_Map
     (This : access Output_Stream_T;
      Size : Types.v64)
   is
      use type Uchar.Pointer;
      use type Interfaces.Integer_64;

      Map : Uchar.Pointer;
   begin
      pragma Assert (not This.Block_Map_Mode);
      This.Block_Map_Mode := True;

      -- Save Our Buffer To Disk
      Flush_Buffer (This);

      if 0 = Size then
         This.Client_Map  := Invalid_Pointer;
         This.Client_Base := Invalid_Pointer;
         This.Client_EOF  := Invalid_Pointer;
      else

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
         This.Client_Map    := Map_Pointer (Map);
         This.Client_Base   := Map_Pointer (Map);
         This.Client_EOF    := Map_Pointer (Map) + C.ptrdiff_t (Size);
      end if;
   end Begin_Block_Map;

   -- unmaps backing memory map
   procedure End_Block_Map (This : access Output_Stream_T) is
      use type Uchar.Pointer;
   begin
      pragma Assert (This.Block_Map_Mode);
      This.Block_Map_Mode := False;

      if Invalid_Pointer /= This.Client_Base then
         MMap_Unmap (This.Client_Base, This.Client_EOF);
      end if;
   end End_Block_Map;

   function Map
     (This : access Output_Stream_T;
      Size : Types.v64) return Sub_Stream
   is
      use type Uchar.Pointer;
      use type Interfaces.Integer_64;

      Result : Sub_Stream;
   begin
      pragma Assert (This.Block_Map_Mode);

      Result :=
        new Sub_Stream_T'
          (Map  => This.Client_Map,
           Base => This.Client_Map,
           EOF  => This.Client_Map + C.ptrdiff_t (Size));

      This.Client_Map := Result.EOF;

      return Result;
   end Map;

   procedure Close (This : access Output_Stream_T) is

      type S is access all Output_Stream_T;
      procedure Delete is new Ada.Unchecked_Deallocation (Output_Stream_T, S);
      D : S := S (This);

      Exit_Code : Integer;
   begin
      -- do Pending Writes
      Flush_Buffer (This);
      Exit_Code := Interfaces.C_Streams.fclose (This.File);
      Delete (D);
   end Close;

   procedure Close (This : access Sub_Stream_T) is
      type S is access all Sub_Stream_T;
      procedure Delete is new Ada.Unchecked_Deallocation (Sub_Stream_T, S);
      D : S := S (This);
   begin
      Delete (D);
   end Close;

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

   function Remaining_Bytes
     (This : access Abstract_Stream'Class) return Skill.Types.v64
   is
      use type Map_Pointer;
   begin
      return Types.v64 (This.EOF - This.Map);
   end Remaining_Bytes;

   function Eof (This : access Sub_Stream_T) return Boolean is
      use C;
      function Cast is new Ada.Unchecked_Conversion (Uchar.Pointer, Types.i64);
      use type Interfaces.Integer_64;
   begin
      return Cast (This.Map) >= Cast (This.EOF);
   end Eof;

   procedure Advance (P : in out Map_Pointer) is
      use C;
      use Uchar;
      use System.Storage_Elements;

   begin
      P := Convert (Casts.To_Pointer (Casts.To_Address (Convert (P)) + 1));
   end Advance;
   pragma Inline_Always (Advance);

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

   procedure Ensure_Size
     (This : not null access Output_Stream_T;
      V    : C.ptrdiff_t)
   is
      use type Map_Pointer;
      use type C.ptrdiff_t;
      use type C_Streams.size_t;
   begin
      if This.EOF - This.Map < 1 + V then
         Flush_Buffer (This);
      end if;
   end Ensure_Size;

   function Cast is new Ada.Unchecked_Conversion (Types.i8, C.unsigned_char);
   procedure I8 (This : access Output_Stream_T; V : Skill.Types.i8) is

      P : Map_Pointer := Invalid_Pointer;
   begin
      This.Ensure_Size (1);

      P     := This.Map;
      P.all := Cast (V);
      Advance (P);
      This.Map := P;
   end I8;

   procedure I8 (This : access Sub_Stream_T; V : Skill.Types.i8) is
      P : Map_Pointer := This.Map;
   begin
      P     := This.Map;
      P.all := Cast (V);
      Advance (P);
      This.Map := P;
   end I8;

   procedure Bool (This : access Sub_Stream_T; V : Boolean) is
      P : Map_Pointer := This.Map;
   begin
      if V then
         P.all := 16#ff#;
      else
         P.all := 0;
      end if;

      Advance (P);
      This.Map := P;
   end Bool;

   procedure I16 (This : access Output_Stream_T; V : Skill.Types.i16) is
      pragma Warnings (Off);
      use C;
      use Uchar;

      function Cast is new Ada.Unchecked_Conversion
        (Unsigned_16,
         C.unsigned_char);
      function Cast is new Ada.Unchecked_Conversion (Types.i16, Unsigned_16);

      P : Map_Pointer := Invalid_Pointer;
   begin
      This.Ensure_Size (2);

      P     := This.Map;
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 8));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 0));
      Advance (P);
      This.Map := P;
   end I16;

   procedure I16 (This : access Sub_Stream_T; V : Skill.Types.i16) is
      pragma Warnings (Off);
      use C;
      use Uchar;

      function Cast is new Ada.Unchecked_Conversion
        (Unsigned_16,
         C.unsigned_char);
      function Cast is new Ada.Unchecked_Conversion (Types.i16, Unsigned_16);

      P : Map_Pointer := This.Map;
   begin
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 8));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 0));
      Advance (P);
      This.Map := P;
   end I16;

   procedure I32 (This : access Output_Stream_T; V : Skill.Types.i32) is
      pragma Warnings (Off);
      use C;
      use Uchar;

      function Cast is new Ada.Unchecked_Conversion
        (Unsigned_32,
         C.unsigned_char);
      function Cast is new Ada.Unchecked_Conversion (Types.i32, Unsigned_32);

      P : Map_Pointer := Invalid_Pointer;
   begin
      This.Ensure_Size (4);

      P     := This.Map;
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 24));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 16));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 8));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 0));
      Advance (P);
      This.Map := P;
   end I32;
   procedure I32 (This : access Sub_Stream_T; V : Skill.Types.i32) is
      use C;
      use Uchar;

      function Cast is new Ada.Unchecked_Conversion
        (Unsigned_32,
         C.unsigned_char);
      function Cast is new Ada.Unchecked_Conversion (Types.i32, Unsigned_32);

      P : Map_Pointer := This.Map;
   begin
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 24));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 16));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 8));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 0));
      Advance (P);
      This.Map := P;
   end I32;

   procedure I64 (This : access Output_Stream_T; V : Skill.Types.i64) is
      use C;
      use Uchar;

      function Cast is new Ada.Unchecked_Conversion
        (Unsigned_64,
         C.unsigned_char);
      function Cast is new Ada.Unchecked_Conversion (Types.i64, Unsigned_64);

      P : Map_Pointer := Invalid_Pointer;
   begin
      This.Ensure_Size (8);

      P     := This.Map;
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 56));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 48));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 40));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 32));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 24));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 16));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 8));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 0));
      Advance (P);
      This.Map := P;
   end I64;

   procedure I64 (This : access Sub_Stream_T; V : Skill.Types.i64) is
      use C;
      use Uchar;

      function Cast is new Ada.Unchecked_Conversion
        (Unsigned_64,
         C.unsigned_char);
      function Cast is new Ada.Unchecked_Conversion (Types.i64, Unsigned_64);

      P : Map_Pointer := This.Map;
   begin
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 56));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 48));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 40));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 32));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 24));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 16));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 8));
      Advance (P);
      P.all := Cast (Interfaces.Shift_Right (Cast (V), 0));
      Advance (P);
      This.Map := P;
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

   function To_Byte is new Ada.Unchecked_Conversion
     (Skill.Types.Uv64,
      C.unsigned_char);
   procedure V64 (This : access Output_Stream_T; Value : Skill.Types.v64) is
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

      P : Map_Pointer := Invalid_Pointer;
   begin
      This.Ensure_Size (9);
      P := This.Map;

      if 0 = (V and 16#FFFFFFFFFFFFFF80#) then
         P.all := To_Byte (V);
         Advance (P);
      else
         P.all := To_Byte (16#80# or V);
         Advance (P);
         if 0 = (V and 16#FFFFFFFFFFFFC000#) then
            P.all := To_Byte (Interfaces.Shift_Right (V, 7));
            Advance (P);
         else
            P.all := To_Byte (16#80# or Interfaces.Shift_Right (V, 7));
            Advance (P);
            if 0 = (V and 16#FFFFFFFFFFE00000#) then
               P.all := To_Byte (Interfaces.Shift_Right (V, 14));
               Advance (P);
            else
               P.all := To_Byte (16#80# or Interfaces.Shift_Right (V, 14));
               Advance (P);
               if 0 = (V and 16#FFFFFFFFF0000000#) then
                  P.all := To_Byte (Interfaces.Shift_Right (V, 21));
                  Advance (P);
               else
                  P.all := To_Byte (16#80# or Interfaces.Shift_Right (V, 21));
                  Advance (P);
                  if 0 = (V and 16#FFFFFFF800000000#) then
                     P.all := To_Byte (Interfaces.Shift_Right (V, 28));
                     Advance (P);
                  else
                     P.all :=
                       To_Byte (16#80# or Interfaces.Shift_Right (V, 28));
                     Advance (P);
                     if 0 = (V and 16#FFFFFC0000000000#) then
                        P.all := To_Byte (Interfaces.Shift_Right (V, 35));
                        Advance (P);
                     else
                        P.all :=
                          To_Byte (16#80# or Interfaces.Shift_Right (V, 35));
                        Advance (P);
                        if 0 = (V and 16#FFFE000000000000#) then
                           P.all := To_Byte (Interfaces.Shift_Right (V, 42));
                           Advance (P);
                        else
                           P.all :=
                             To_Byte
                               (16#80# or Interfaces.Shift_Right (V, 42));
                           Advance (P);
                           if 0 = (V and 16#FF00000000000000#) then
                              P.all :=
                                To_Byte (Interfaces.Shift_Right (V, 49));
                              Advance (P);
                           else
                              P.all :=
                                To_Byte
                                  (16#80# or Interfaces.Shift_Right (V, 49));
                              Advance (P);
                              P.all :=
                                To_Byte (Interfaces.Shift_Right (V, 56));
                              Advance (P);
                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
      This.Map := P;
   end V64;

   procedure V64 (This : access Sub_Stream_T; Value : Skill.Types.v64) is
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

      P : Map_Pointer := This.Map;
   begin
      if 0 = (V and 16#FFFFFFFFFFFFFF80#) then
         P.all := To_Byte (V);
         Advance (P);
      else
         P.all := To_Byte (16#80# or V);
         Advance (P);
         if 0 = (V and 16#FFFFFFFFFFFFC000#) then
            P.all := To_Byte (Interfaces.Shift_Right (V, 7));
            Advance (P);
         else
            P.all := To_Byte (16#80# or Interfaces.Shift_Right (V, 7));
            Advance (P);
            if 0 = (V and 16#FFFFFFFFFFE00000#) then
               P.all := To_Byte (Interfaces.Shift_Right (V, 14));
               Advance (P);
            else
               P.all := To_Byte (16#80# or Interfaces.Shift_Right (V, 14));
               Advance (P);
               if 0 = (V and 16#FFFFFFFFF0000000#) then
                  P.all := To_Byte (Interfaces.Shift_Right (V, 21));
                  Advance (P);
               else
                  P.all := To_Byte (16#80# or Interfaces.Shift_Right (V, 21));
                  Advance (P);
                  if 0 = (V and 16#FFFFFFF800000000#) then
                     P.all := To_Byte (Interfaces.Shift_Right (V, 28));
                     Advance (P);
                  else
                     P.all :=
                       To_Byte (16#80# or Interfaces.Shift_Right (V, 28));
                     Advance (P);
                     if 0 = (V and 16#FFFFFC0000000000#) then
                        P.all := To_Byte (Interfaces.Shift_Right (V, 35));
                        Advance (P);
                     else
                        P.all :=
                          To_Byte (16#80# or Interfaces.Shift_Right (V, 35));
                        Advance (P);
                        if 0 = (V and 16#FFFE000000000000#) then
                           P.all := To_Byte (Interfaces.Shift_Right (V, 42));
                           Advance (P);
                        else
                           P.all :=
                             To_Byte
                               (16#80# or Interfaces.Shift_Right (V, 42));
                           Advance (P);
                           if 0 = (V and 16#FF00000000000000#) then
                              P.all :=
                                To_Byte (Interfaces.Shift_Right (V, 49));
                              Advance (P);
                           else
                              P.all :=
                                To_Byte
                                  (16#80# or Interfaces.Shift_Right (V, 49));
                              Advance (P);
                              P.all :=
                                To_Byte (Interfaces.Shift_Right (V, 56));
                              Advance (P);
                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;

      This.Map := P;
   end V64;

   function Cast is new Ada.Unchecked_Conversion (Character, C.unsigned_char);
   procedure Put_Plain_String
     (This : access Output_Stream_T;
      V    : Skill.Types.String_Access)
   is
      P : Map_Pointer := Invalid_Pointer;
   begin
      This.Ensure_Size (V.all'Length);
      P := This.Map;
      for C of V.all loop
         P.all := Cast (C);
         Advance (P);
      end loop;
      This.Map := P;
   end Put_Plain_String;

end Skill.Streams.Writer;
