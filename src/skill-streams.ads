--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     stream manipulation package                         --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --

with Interfaces.C;
with Interfaces.C.Pointers;

with Skill.Types;
limited with Skill.Streams.Reader;


package Skill.Streams is
   pragma Preelaborate;

   type v64_Extended is record
      Value  : Skill.Types.v64;
      Length : Interfaces.C.size_t'Base range 1 .. 9;
   end record;

   type Unsigned_Char_Array is
     array
       (Interfaces.C.size_t range <>) of aliased Interfaces.C.unsigned_char;
   pragma Convention (C, Unsigned_Char_Array);
   for Unsigned_Char_Array'Component_Size use Interfaces.C.unsigned_char'Size;

   type Input_Stream is not null access Skill.Streams.Reader.Input_Stream_T;

   function Input (Path : Skill.Types.String_Access) return Input_Stream;

end Skill.Streams;
