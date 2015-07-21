--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     !! remove after integration into generator !!       --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Files;
with Skill.Internal.File_Parsers;

-- parametrization of file, read/write and pool code
package Age.Api is

   use Skill;

   type File_T is new Skill.Files.File_T with private;
   type File is access File_T;

   -- create a new file using the argument path for I/O
   function Open
     (Path    : String;
      Read_M  : Files.Read_Mode  := Skill.Files.Read;
      Write_M : Files.Write_Mode := Skill.Files.Write) return File;

private

   type File_T is new Skill.Files.File_T with null record;

end Age.Api;
