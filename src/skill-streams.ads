--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     stream manipulation package                         --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --

with Interfaces.C;
with Interfaces.C.Pointers;

with Skill.Types.Api;

package Skill.Streams is

   type v64_Extended is
      record
         Value  : Skill.Types.Api.V64;
         Length : Interfaces.C.size_t'Base range 1 .. 9;
      end record;

   type Unsigned_Char_Array is array (Interfaces.C.size_t range <>) of aliased Interfaces.C.unsigned_char;
   pragma Convention (C, Unsigned_Char_Array);
   for Unsigned_Char_Array'Component_Size use Interfaces.C.unsigned_char'Size;

end Skill.Streams;
