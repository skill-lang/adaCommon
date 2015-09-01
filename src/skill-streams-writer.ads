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

   type Abstract_Stream is abstract tagged private;

   type Output_Stream_T is new Abstract_Stream with private;
   type Output_Stream is access Output_Stream_T;
   type Sub_Stream_T is new Abstract_Stream with private;
   type Sub_Stream is access Sub_Stream_T;

   function Open
     (Path : not null Skill.Types.String_Access;
      Mode : String) return Output_Stream;

   -- creates a map for a block and enables usage of map function
   procedure Begin_Block_Map (This : access Output_Stream_T; Size : Types.v64);

   -- unmaps backing memory map
   procedure End_Block_Map (This : access Output_Stream_T);

   -- creates a sub stream
   -- note: only sub streams use mmaps
   function Map
     (This : access Output_Stream_T;
      Size : Types.v64) return Sub_Stream;

   -- destroy a map and close the file
   procedure Close (This : access Output_Stream_T);
   -- destroy a sub map
   procedure Close (This : access Sub_Stream_T);

   -- reached end of mapped region?
   function Eof (This : access Sub_Stream_T) return Boolean;
--
--     function Path
--       (This : access Output_Stream_T) return Skill.Types.String_Access;
--
   function Position
     (This : access Abstract_Stream) return Skill.Types.v64 is abstract;
   function Position (This : access Output_Stream_T) return Skill.Types.v64;
   function Position (This : access Sub_Stream_T) return Skill.Types.v64;

   -- bytes remaining in the current buffer
   function Remaining_Bytes
     (This : access Abstract_Stream'Class) return Skill.Types.v64;

   procedure I8
     (This : access Abstract_Stream;
      V    : Skill.Types.i8) is abstract;
   procedure I8 (This : access Output_Stream_T; V : Skill.Types.i8);
   procedure I8 (This : access Sub_Stream_T; V : Skill.Types.i8);

   procedure Bool (This : access Sub_Stream_T; V : Boolean);
   pragma Inline (Bool);

   procedure I16 (This : access Sub_Stream_T; V : Skill.Types.i16);
   pragma Inline (I16);

   procedure I32
     (This : access Abstract_Stream;
      V    : Skill.Types.i32) is abstract;
   pragma Inline (I32);
   procedure I32 (This : access Output_Stream_T; V : Skill.Types.i32);
   procedure I32 (This : access Sub_Stream_T; V : Skill.Types.i32);

   procedure I64 (This : access Sub_Stream_T; V : Skill.Types.i64);
   pragma Inline (I64);

   procedure F32 (This : access Sub_Stream_T; V : Skill.Types.F32);
   pragma Inline (F32);

   procedure F64 (This : access Sub_Stream_T; V : Skill.Types.F64);
   pragma Inline (F64);

   procedure V64
     (This : access Abstract_Stream;
      V    : Skill.Types.v64) is abstract;
   procedure V64 (This : access Output_Stream_T; Value : Skill.Types.v64);
   procedure V64 (This : access Sub_Stream_T; Value : Skill.Types.v64);

   -- write the image of a string into a file
   procedure Put_Plain_String
     (This : access Output_Stream_T;
      V    : Skill.Types.String_Access);

private

   procedure Ensure_Size (This : access Output_Stream_T; V : C.ptrdiff_t);
   pragma No_Inline (Ensure_Size);

   package C renames Interfaces.C;

   type Abstract_Stream is abstract tagged record
      -- current position
      Map : Map_Pointer;
      -- first position
      Base : Map_Pointer;
      -- last position
      EOF : Map_Pointer;
   end record;

   function MMap_Write_Map
     (F      : Interfaces.C_Streams.FILEs;
      Length : Types.v64) return Uchar.Pointer;
   pragma Import (C, MMap_Write_Map, "mmap_write_map_block");

   procedure MMap_Unmap (Base : Map_Pointer; Eof : Map_Pointer);
   pragma Import (C, MMap_Unmap, "mmap_write_unmap");

   type Output_Stream_T is new Abstract_Stream with record
      Path          : Skill.Types.String_Access; -- shared string!
      File          : Interfaces.C_Streams.FILEs;
      Bytes_Written : Types.v64;
      Buffer        : Uchar_Array (1 .. 1024);

      -- true, iff a block is currently mapped
      Block_Map_Mode : Boolean;
      -- current position in client map
      Client_Map : Map_Pointer;
      -- first position in client map
      Client_Base : Map_Pointer;
      -- last position in client map
      Client_EOF : Map_Pointer;
   end record;

   -- a part that is a sub section of an input stream
   type Sub_Stream_T is new Abstract_Stream with null record;

end Skill.Streams.Writer;
