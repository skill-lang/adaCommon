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
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

package body Skill.Streams.Reader is

   use Skill;
   use type Uchar.Pointer;

   function Invalid_Pointer return Map_Pointer is
      pragma Warnings (Off);
      function Cast is new Ada.Unchecked_Conversion (Integer, Map_Pointer);
   begin
      return Cast (-1);
   end Invalid_Pointer;

   -- type used to represent empty input streams
   -- @note(TF) this is used to keep stream & map pointers not null
   function Empty_Stream return Input_Stream is
   begin
      return new Input_Stream_T'
          (Map  => Invalid_Pointer,
           Base => Invalid_Pointer,
           EOF  => Invalid_Pointer,
           Path => null,
           File => Interfaces.C_Streams.NULL_Stream);
   end Empty_Stream;

   function Open (Path : Types.String_Access) return Input_Stream is
      use type Skill.Types.String_Access;
      use type Interfaces.C.size_t;
   begin
      if null = Path then
         return Empty_Stream;
      end if;

      declare
         Cpath : Interfaces.C.Strings.chars_ptr :=
           Interfaces.C.Strings.New_String (Path.all);

         Map : Mmap := MMap_Open (Cpath);

         Mf : aliased access Integer;
         for Mf'Address use Map.File'Address;
         pragma Import (Ada, Mf);

         Ml : aliased access Integer;
         for Ml'Address use Map.Length'Address;
         pragma Import (Ada, Ml);

         Mm : aliased access Integer;
         for Mm'Address use Map.Map'Address;
         pragma Import (Ada, Mm);

      begin
         Interfaces.C.Strings.Free (Cpath);

         if Mf = null and Ml = null and Mm = null then
            raise Skill.Errors.Skill_Error
              with "failed to open stream, see stdout for details";
         end if;

         if 0 = Map.Length then
            return Empty_Stream;
         end if;

         return new Input_Stream_T'
             (Path => Path,
              File => Map.File,
              Map  => Map.Map,
              Base => Map.Map,
              EOF  => Map.Map + C.ptrdiff_t (Map.Length));
      end;
   end Open;

   function Map
     (This  : access Input_Stream_T;
      Base  : Types.v64;
      First : Types.v64;
      Last  : Types.v64) return Sub_Stream
   is
      use type Uchar.Pointer;
      use type Interfaces.Integer_64;

      function Cast is new Ada.Unchecked_Conversion (Map_Pointer, Types.v64);
   begin
      if Cast (This.EOF) < Cast (This.Base + C.ptrdiff_t (Base + Last)) then
         raise Constraint_Error with "Tried to read behind end of file.";
      end if;

      return new Sub_Stream_T'
          (Map  => This.Base + C.ptrdiff_t (Base + First),
           Base => This.Base + C.ptrdiff_t (Base + First),
           EOF  => This.Base + C.ptrdiff_t (Base + Last));
   end Map;

   The_Empty_Sub_Stream : Sub_Stream :=
     new Sub_Stream_T'
       (Map  => Invalid_Pointer,
        Base => Invalid_Pointer,
        EOF  => Invalid_Pointer);
   function Empty_Sub_Stream return Sub_Stream is (The_Empty_Sub_Stream);

   procedure Close (This : access Input_Stream_T) is

      type S is access all Input_Stream_T;
      procedure Delete is new Ada.Unchecked_Deallocation (Input_Stream_T, S);
      D : S := S (This);

   begin
      -- do not close fake files
      if Invalid_Pointer /= This.Map then
         MMap_Close (This.File);
      end if;

      Delete (D);
   end Close;

   procedure Free (This : access Sub_Stream_T) is
      type S is access all Sub_Stream_T;
      procedure Delete is new Ada.Unchecked_Deallocation (Sub_Stream_T, S);
      D : S := S (This);
   begin
      if S (This) /= S (The_Empty_Sub_Stream) then
         Delete (D);
      end if;
   end Free;

   function Path
     (This : access Input_Stream_T) return Skill.Types.String_Access
   is
   begin
      return This.Path;
   end Path;

   function Position
     (This : access Abstract_Stream'Class) return Skill.Types.v64
   is
   begin
      return Types.v64 (This.Map - This.Base);
   end Position;

   procedure Check_Offset
     (This : access Abstract_Stream'Class;
      Pos  : Skill.Types.v64)
   is
      use type Types.v64;
   begin
      if Types.v64 (This.EOF - This.Base) <= Pos then
         raise Constraint_Error
           with "Offset check failed, argument position is behind end of file.";
      end if;
   end Check_Offset;

   procedure Jump
     (This : access Abstract_Stream'Class;
      Pos  : Skill.Types.v64)
   is
   begin
      This.Map := This.Base + C.ptrdiff_t (Pos);
   end Jump;

   function Eof (This : access Abstract_Stream'Class) return Boolean is
      use C;
      function Cast is new Ada.Unchecked_Conversion (Uchar.Pointer, Types.i64);
      use type Interfaces.Integer_64;
   begin
      return Cast (This.Map) >= Cast (This.EOF);
   end Eof;

   package Casts is new System.Address_To_Access_Conversions (C.unsigned_char);

   function Convert is new Ada.Unchecked_Conversion
     (Interfaces.C.unsigned_char,
      Skill.Types.i8);
   function Convert is new Ada.Unchecked_Conversion
     (Casts.Object_Pointer,
      Map_Pointer);
   function Convert is new Ada.Unchecked_Conversion
     (Map_Pointer,
      Casts.Object_Pointer);

   procedure Advance (P : in out Map_Pointer) is
      use C;
      use Uchar;
      use System.Storage_Elements;
   begin
      P := Convert (Casts.To_Pointer (Casts.To_Address (Convert (P)) + 1));
   end Advance;
   pragma Inline_Always (Advance);

   function I8 (This : access Abstract_Stream'Class) return Skill.Types.i8 is
      use C;
      use Uchar;

      P : Map_Pointer := This.Map;
      R : Types.i8    := Convert (This.Map.all);

   begin
      Advance (P);
      This.Map := P;
      return R;
   end I8;

   function I16 (This : access Abstract_Stream'Class) return Skill.Types.i16 is
      use C;
      use Uchar;

      subtype Bytes is Unsigned_Char_Array (1 .. 2);
      function Convert is new Ada.Unchecked_Conversion (Bytes, Types.i16);
      P  : Uchar.Pointer := This.Map;
      P1 : Uchar.Pointer := P + 1;
      R  : Types.i16     := Convert (Bytes'(P1.all, This.Map.all));
   begin
      This.Map := This.Map + 2;
      return R;
   end I16;

   function I32 (This : access Abstract_Stream'Class) return Skill.Types.i32 is
      use C;
      use Uchar;

      subtype Bytes is Unsigned_Char_Array (1 .. 4);
      function Convert is new Ada.Unchecked_Conversion (Bytes, Types.i32);
      P  : Uchar.Pointer := This.Map;
      P1 : Uchar.Pointer := P + 1;
      P2 : Uchar.Pointer := P + 2;
      P3 : Uchar.Pointer := P + 3;
      R  : Types.i32 := Convert (Bytes'(P3.all, P2.all, P1.all, This.Map.all));
   begin
      This.Map := This.Map + 4;
      return R;
   end I32;

   function I64 (This : access Abstract_Stream'Class) return Skill.Types.i64 is
      use C;
      use Uchar;

      subtype Bytes is Unsigned_Char_Array (1 .. 8);
      function Convert is new Ada.Unchecked_Conversion (Bytes, Types.i64);
      P  : Uchar.Pointer := This.Map;
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
             (P7.all,
              P6.all,
              P5.all,
              P4.all,
              P3.all,
              P2.all,
              P1.all,
              This.Map.all));
   begin
      This.Map := This.Map + 8;
      return R;
   end I64;

   function F32 (This : access Abstract_Stream'Class) return Skill.Types.F32 is
      function Cast is new Ada.Unchecked_Conversion (Types.i32, Types.F32);
   begin
      return Cast (This.I32);
   end F32;

   function F64 (This : access Abstract_Stream'Class) return Skill.Types.F64 is
      function Cast is new Ada.Unchecked_Conversion (Types.i64, Types.F64);
   begin
      return Cast (This.I64);
   end F64;

   function V64 (This : access Abstract_Stream'Class) return Skill.Types.v64 is

      use C;
      use Uchar;
      use type Interfaces.Unsigned_64;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Types.Uv64,
         Target => Types.v64);

      P            : Map_Pointer     := This.Map;
      Bucket       : C.unsigned_char := P.all;
      Return_Value : Types.Uv64      := Types.Uv64 (Bucket);
   begin
      Advance (P);
      if 0 /= (Bucket and 16#80#) then
         Bucket := P.all;
         Advance (P);
         Return_Value :=
           (Return_Value and 16#7f#) or
           Interfaces.Shift_Left (Types.Uv64 (Bucket) and 16#7f#, 7);

         if 0 /= (Bucket and 16#80#) then
            Bucket := P.all;
            Advance (P);
            Return_Value :=
              Return_Value or
              Interfaces.Shift_Left (Types.Uv64 (Bucket) and 16#7f#, 14);

            if 0 /= (Bucket and 16#80#) then
               Bucket := P.all;
               Advance (P);
               Return_Value :=
                 Return_Value or
                 Interfaces.Shift_Left (Types.Uv64 (Bucket) and 16#7f#, 21);

               if 0 /= (Bucket and 16#80#) then
                  Bucket := P.all;
                  Advance (P);
                  Return_Value :=
                    Return_Value or
                    Interfaces.Shift_Left (Types.Uv64 (Bucket) and 16#7f#, 28);

                  if 0 /= (Bucket and 16#80#) then
                     Bucket := P.all;
                     Advance (P);
                     Return_Value :=
                       Return_Value or
                       Interfaces.Shift_Left
                         (Types.Uv64 (Bucket) and 16#7f#,
                          35);

                     if 0 /= (Bucket and 16#80#) then
                        Bucket := P.all;
                        Advance (P);
                        Return_Value :=
                          Return_Value or
                          Interfaces.Shift_Left
                            (Types.Uv64 (Bucket) and 16#7f#,
                             42);
                        if 0 /= (Bucket and 16#80#) then
                           Bucket := P.all;
                           Advance (P);
                           Return_Value :=
                             Return_Value or
                             Interfaces.Shift_Left
                               (Types.Uv64 (Bucket) and 16#7f#,
                                49);

                           if 0 /= (Bucket and 16#80#) then
                              Bucket := P.all;
                              Advance (P);
                              Return_Value :=
                                Return_Value or
                                Interfaces.Shift_Left
                                  (Types.Uv64 (Bucket),
                                   56);

                           end if;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;

      This.Map := P;
      return Convert (Return_Value);
   end V64;

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
