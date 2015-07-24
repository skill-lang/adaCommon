--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     general file interaction                            --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Errors;
with Skill.Internal.File_Parsers;
with Skill.Streams;
with Skill.Types;

package body Skill.Files is

   package FileParser renames Skill.Internal.File_Parsers;

   function Strings
     (This : access File_T'Class) return Skill.String_Pools.Pool
   is
   begin
      return This.Strings;
   end Strings;

end Skill.Files;
