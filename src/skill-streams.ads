--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     stream manipulation package                         --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --

with Interfaces.C;
with Interfaces.C.Pointers;

with Skill.Types;
limited with Skill.Streams.Reader;
limited with Skill.Streams.Writer;
with Interfaces.C_Streams;

package Skill.Streams is

   type Unsigned_Char_Array is
     array
       (Interfaces.C.size_t range <>) of aliased Interfaces.C.unsigned_char;
   pragma Convention (C, Unsigned_Char_Array);
   for Unsigned_Char_Array'Component_Size use Interfaces.C.unsigned_char'Size;

   function Input
     (Path : Skill.Types.String_Access)
      return Skill.Streams.Reader.Input_Stream;

   function Write
     (Path : Skill.Types.String_Access)
      return Skill.Streams.Writer.Output_Stream;

   function Append
     (Path : Skill.Types.String_Access)
      return Skill.Streams.Writer.Output_Stream;

private
   package C renames Interfaces.C;

   type Uchar_Array is array (C.size_t range <>) of aliased C.unsigned_char;
   package Uchar is new C.Pointers
     (Index              => C.size_t,
      Element            => C.unsigned_char,
      Element_Array      => Uchar_Array,
      Default_Terminator => 0);
   subtype Map_Pointer is not null Uchar.Pointer;

   -- returns an invalid map pointer, that can be used in empty maps
   function Invalid_Pointer return Map_Pointer;
   pragma Inline (Invalid_Pointer);
   pragma Pure_Function (Invalid_Pointer);

   type Mmap is record
      File   : Interfaces.C_Streams.FILEs;
      Length : Interfaces.C.size_t;
      Map    : Uchar.Pointer;
   end record;

end Skill.Streams;
