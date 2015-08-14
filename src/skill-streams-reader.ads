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

   type Abstract_Stream is tagged private;

   type Input_Stream_T is new Abstract_Stream with private;
   type Input_Stream is not null access Skill.Streams.Reader.Input_Stream_T;
   type Sub_Stream_T is new Abstract_Stream with private;
   type Sub_Stream is access Sub_Stream_T;

   -- note an empty stream will be created if path is null
   function Open (Path : Skill.Types.String_Access) return Input_Stream;

   -- creates a sub map
   function Map
     (This  : access Input_Stream_T;
      Base  : Types.v64;
      First : Types.v64;
      Last  : Types.v64) return Sub_Stream;

   -- destroy a map and close the file
   procedure Free (This : access Input_Stream_T);
   -- destroy a sub map
   procedure Free (This : access Sub_Stream_T);

   function Path
     (This : access Input_Stream_T) return Skill.Types.String_Access;

   function Eof (This : access Abstract_Stream'Class) return Boolean;

   function Position
     (This : access Abstract_Stream'Class) return Skill.Types.v64;

   procedure Jump (This : access Abstract_Stream'Class; Pos : Skill.Types.v64);

   function I8 (This : access Abstract_Stream'Class) return Skill.Types.i8;
   pragma Inline (I8);

   use type Interfaces.Integer_8;
   function Bool
     (This : access Abstract_Stream'Class) return Boolean is
     (This.I8 /= 0);
   pragma Inline (Bool);

   function I16 (This : access Abstract_Stream'Class) return Skill.Types.i16;
   pragma Inline (I16);

   function I32 (This : access Abstract_Stream'Class) return Skill.Types.i32;
   pragma Inline (I32);

   function I64 (This : access Abstract_Stream'Class) return Skill.Types.i64;
   pragma Inline (I64);

   function F32 (This : access Abstract_Stream'Class) return Skill.Types.F32;
   pragma Inline (F32);

   function F64 (This : access Abstract_Stream'Class) return Skill.Types.F64;
   pragma Inline (F64);

   function V64 (This : access Abstract_Stream'Class) return Skill.Types.v64;
   -- wont happen, simply too large
   pragma Inline (V64);

   function Parse_Exception
     (This          :    access Input_Stream_T;
      Block_Counter :    Positive;
      Cause         : in Ada.Exceptions.Exception_Occurrence;
      Message       :    String) return String;

   function Parse_Exception
     (This          : access Input_Stream_T;
      Block_Counter : Positive;
      Message       : String) return String;

private
   package C renames Interfaces.C;

   -- mmap_c_array mmap_open (char const * filename)
   function MMap_Open (Path : Interfaces.C.Strings.chars_ptr) return Mmap;
   pragma Import (C, MMap_Open, "mmap_open");
   -- void mmap_close(FILE *stream)
   procedure MMap_Close (File : Interfaces.C_Streams.FILEs);
   pragma Import (C, MMap_Close, "mmap_close");

   type Abstract_Stream is tagged record
      -- current position
      Map : Map_Pointer;
      -- first position
      Base : Map_Pointer;
      -- last position
      EOF : Map_Pointer;
   end record;

   type Input_Stream_T is new Abstract_Stream with record
      Path : Skill.Types.String_Access; -- shared string!
      File : Interfaces.C_Streams.FILEs;
   end record;

   -- a part that is a sub section of an input stream
   type Sub_Stream_T is new Abstract_Stream with null record;

end Skill.Streams.Reader;
