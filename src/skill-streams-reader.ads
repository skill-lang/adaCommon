--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     stream to skill tokens                              --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --

with Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;
with Interfaces.C_Streams;

with Skill.Types;

package Skill.Streams.Reader is
   pragma Preelaborate;

   type Input_Stream_T is tagged private;

   function Open (Path : Skill.Types.String_Access) return Input_Stream;

   function Eof (This : Input_Stream_T) return Boolean;

   function I8 (This : access Input_Stream_T) return Skill.Types.i8;

--     function Read_i16
--       (Mapped : Unsigned_Char_Array;
--        Start  : Interfaces.C.size_t) return i16;
--
--     function Read_i32
--       (Mapped : Unsigned_Char_Array;
--        Start  : Interfaces.C.size_t) return i32;
--
--     function Read_i64
--       (Mapped : Unsigned_Char_Array;
--        Start  : Interfaces.C.size_t) return i64;
--
--     function Read_v64
--       (Mapped : Unsigned_Char_Array;
--        Start  : Interfaces.C.size_t) return v64_Extended;
--
--     function Read_String
--       (Mapped : Unsigned_Char_Array;
--        Start  : Interfaces.C.size_t;
--        Length : i32) return String;
--
--     pragma Inline
--       (Read_i8,
--        Read_i16,
--        Read_i32,
--        Read_i64,
--        Read_v64,
--        Read_String);

private
   package C renames Interfaces.C;

   type Uchar_Array is array (C.size_t range <>) of aliased C.unsigned_char;
   package Uchar is new C.Pointers
     (Index              => C.size_t,
      Element            => C.unsigned_char,
      Element_Array      => Uchar_Array,
      Default_Terminator => 0);

   type Mmap is record
      File   : Interfaces.C_Streams.FILEs;
      Length : Interfaces.C.size_t;
      Map    : Uchar.Pointer;
   end record;

   -- mmap_c_array mmap_open (char const * filename)
   function MMap_Open (Path : Interfaces.C.Strings.chars_ptr) return Mmap;
   pragma Import (C, MMap_Open, "mmap_open");

   type Input_Stream_T is tagged record
      Path     : Skill.Types.String_Access;
      File     : Interfaces.C_Streams.FILEs;
      Length   : Interfaces.C.size_t;
      Position : Interfaces.C.size_t;
      Map      : Uchar.Pointer;
   end record;

end Skill.Streams.Reader;
