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

package Skill.Streams.Writer is

   type Abstract_Stream is tagged private;

   type Output_Stream_T is new Abstract_Stream with private;
   type Output_Stream is access Output_Stream_T;
   type Sub_Stream_T is new Abstract_Stream with private;
   type Sub_Stream is access Sub_Stream_T;

   function Open (Path : Skill.Types.String_Access) return Output_Stream;

   -- creates a sub map
--     function Map
--       (This  : access Output_Stream_T;
--        Base  : Types.v64;
--        First : Types.v64;
--        Last  : Types.v64) return Sub_Stream;
--
--     -- destroy a map and close the file
--     procedure Free (This : access Output_Stream_T);
--     -- destroy a sub map
--     procedure Free (This : access Sub_Stream_T);
--
--     function Path
--       (This : access Output_Stream_T) return Skill.Types.String_Access;
--
--     function Position
--       (This : access Abstract_Stream'Class) return Skill.Types.v64;

--     function I8 (This : access Abstract_Stream'Class) return Skill.Types.i8;
--     pragma Inline (I8);
--
--     use type Interfaces.Integer_8;
--     function Bool
--       (This : access Abstract_Stream'Class) return Boolean is
--       (This.I8 /= 0);
--     pragma Inline (Bool);
--
--     function I16 (This : access Abstract_Stream'Class) return Skill.Types.i16;
--     pragma Inline (I16);
--
--     function I32 (This : access Abstract_Stream'Class) return Skill.Types.i32;
--     pragma Inline (I32);
--
--     function I64 (This : access Abstract_Stream'Class) return Skill.Types.i64;
--     pragma Inline (I64);
--
--     function F32 (This : access Abstract_Stream'Class) return Skill.Types.F32;
--     pragma Inline (F32);
--
--     function F64 (This : access Abstract_Stream'Class) return Skill.Types.F64;
--     pragma Inline (F64);
--
--     function V64 (This : access Abstract_Stream'Class) return Skill.Types.v64;
--     -- wont happen, simply too large
--     pragma Inline (V64);

private
   package C renames Interfaces.C;

   -- mmap_c_array mmap_open (char const * filename)
   function MMap_Open (Path : Interfaces.C.Strings.chars_ptr) return Mmap;
   pragma Import (C, MMap_Open, "mmap_open");
   -- void mmap_close(FILE *stream)
   procedure MMap_Close (File : Interfaces.C_Streams.FILEs);
   pragma Import (C, MMap_Close, "mmap_close");

   type Abstract_Stream is tagged record
      Length   : Interfaces.C.size_t;
      Position : Interfaces.C.size_t;
      Map      : Uchar.Pointer;
   end record;

   type Output_Stream_T is new Abstract_Stream with record
      Path : Skill.Types.String_Access; -- shared string!
      File : Interfaces.C_Streams.FILEs;
   end record;

   -- a part that is a sub section of an input stream
   type Sub_Stream_T is new Abstract_Stream with null record;

end Skill.Streams.Writer;
