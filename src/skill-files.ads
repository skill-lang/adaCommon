--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     general file interaction                            --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Text_IO;

package Skill.Files is

   type File_T is tagged private;
   type File is not null access File_T'Class;

   type Read_Mode is (Create, Read);
   type Write_Mode is (Write, Append);

   function Open
     (Path    : String;
      Read_M  : Read_Mode  := Read;
      Write_M : Write_Mode := Write) return File;

private

   type File_T is tagged record
      null;
   end record;

end Skill.Files;
