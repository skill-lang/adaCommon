--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Your SKilL Scala Binding                            --
-- \__ \ ' <| | | |__     <<debug>>                                           --
-- |___/_|\_\_|_|____|    by: <<some developer>>                              --
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

   -- write changes to disk
   procedure Flush (This : access File_T);

   -- write changes to disk, free all memory
   procedure Close (This : access File_T);

   -- user type pools
   -- work around GNAT bug

   package Age_Pool_P renames Skill.Types.Pools.Age_Pools.Age_P;
   subtype Age_Pool is Age_Pool_P.Pool;
   function Ages (This : access File_T) return Age_Pool;

private

   type File_T is new Skill.Files.File_T with record
      Ages : Age_Pool;
   end record;

end Age.Api;
