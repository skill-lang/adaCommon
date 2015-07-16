--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     stream manipulation package                         --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --

with Interfaces.C;
with Interfaces.C.Pointers;

with Skill.Types;
with Skill.Streams.Reader;

package body Skill.Streams is

   function Input (Path : Skill.Types.String_Access) return Input_Stream is
   begin
      return Skill.Streams.Reader.Open(Path);
   end Input;

end Skill.Streams;
