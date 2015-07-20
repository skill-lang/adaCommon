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
with Ada.Exceptions;

package Skill.Streams.Reader is
   pragma Preelaborate;

   type Input_Stream_T is tagged private;

   function Open (Path : Skill.Types.String_Access) return Input_Stream;

--     function Map
--     Eventuell Will Man Die Map Funktion Dadurch Realisieren, Dass Man Einen
--       Submap record Anlegt, Der Angpasste Map / Size / Lengths Enthält.

   function Path (This : access Input_Stream_T) return Skill.Types.String_Access;

   function Eof (This : access Input_Stream_T) return Boolean;

   function Position (This : access Input_Stream_T) return Skill.Types.v64;

   procedure Jump (This : access Input_Stream_T; Pos : Skill.Types.V64);

   function I8 (This : access Input_Stream_T) return Skill.Types.i8;

   function I16 (This : access Input_Stream_T) return Skill.Types.i16;

   function I32 (This : access Input_Stream_T) return Skill.Types.i32;

   function I64 (This : access Input_Stream_T) return Skill.Types.i64;

   function V64 (This : access Input_Stream_T) return Skill.Types.v64;

   pragma Inline (I8, I16, I32, I64, V64);

   function Parse_Exception
     (This          :    access Input_Stream_T;
      Block_Counter :    Positive;
      Cause         : in Ada.Exceptions.Exception_Occurrence;
      Message       :    String) return String;

   function Parse_Exception
     (This          :    access Input_Stream_T;
      Block_Counter :    Positive;
      Message       :    String) return String;

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
