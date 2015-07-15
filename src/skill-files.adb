--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     general file interaction                            --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Text_IO;

package body Skill.Files is

   function Open
     (Path    : String;
      Read_M  : Read_Mode  := Read;
      Write_M : Write_Mode := Write) return File
   is
   begin
      return new File_T;
   end Open;

end Skill.Files;
