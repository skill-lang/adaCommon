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

   type Unsigned_Char_Array is
     array
       (Interfaces.C.size_t range <>) of aliased Interfaces.C.unsigned_char;
   pragma Convention (C, Unsigned_Char_Array);
   for Unsigned_Char_Array'Component_Size use Interfaces.C.unsigned_char'Size;

   function Input
     (Path : Skill.Types.String_Access)
      return Skill.Streams.Reader.Input_Stream;

end Skill.Streams;
