--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     !! remove after integration into generator !!       --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Files;
with Skill.Internal.File_Parsers;
with Skill.Types.Pools;
with Skill.Types.Pools.Age_Pools;

-- parametrization of file, read/write and pool code
package Age.Api is

   type File_T is new Skill.Files.File_T with private;
   type File is access File_T;

   -- create a new file using the argument path for I/O
   function Open
     (Path    : String;
      Read_M  : Skill.Files.Read_Mode  := Skill.Files.Read;
      Write_M : Skill.Files.Write_Mode := Skill.Files.Write) return File;

   -- user type pools
   Age_Pool_Skill_Name : not null Skill.Types.String_Access :=
                           new String'("age");
   -- work aroun GNAT bug
   package Age_Pool_P renames Skill.Types.Pools.Age_Pools.Age_P;
   subtype Age_Pool is Age_Pool_P.Pool;

private

   type File_T is new Skill.Files.File_T with null record;

end Age.Api;
