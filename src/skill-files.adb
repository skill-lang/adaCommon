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

   function Finish_Allocation
     (Path          : Skill.Types.String_Access;
      Mode          : Write_Mode;
      Strings       : Skill.String_Pools.Pool;
      Types         : Type_Vector;
      Types_By_Name : Type_Map) return File
   is
   begin
      return null; -- TODO
   end Finish_Allocation;

end Skill.Files;
