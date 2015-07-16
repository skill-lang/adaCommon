--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     stream to skill tokens                              --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --

with Interfaces.C;

with Skill.Types.Api; use Skill.Types.Api;

package Skill.Streams.Reader is

   function Read_i8 (
      Mapped : Unsigned_Char_Array;
      Start  : Interfaces.C.size_t
   ) return i8;

   function Read_i16 (
      Mapped : Unsigned_Char_Array;
      Start  : Interfaces.C.size_t
   ) return i16;

   function Read_i32 (
      Mapped : Unsigned_Char_Array;
      Start  : Interfaces.C.size_t
   ) return i32;

   function Read_i64 (
      Mapped : Unsigned_Char_Array;
      Start  : Interfaces.C.size_t
   ) return i64;

   function Read_v64 (
      Mapped : Unsigned_Char_Array;
      Start  : Interfaces.C.size_t
   ) return v64_Extended;

   function Read_String (
      Mapped : Unsigned_Char_Array;
      Start  : Interfaces.C.size_t;
      Length : i32
   ) return String;

   pragma Inline (
      Read_i8,
      Read_i16,
      Read_i32,
      Read_i64,
      Read_v64,
      Read_String
   );

end Skill.Streams.Reader;
