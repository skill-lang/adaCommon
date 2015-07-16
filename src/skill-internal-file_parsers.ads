--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file parser implementation                          --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Streams;
with Skill.Files;

-- documentation can be found in java common
package Skill.Internal.File_Parsers is

   function Read
     (Input : Skill.Streams.Input_Stream;
      Mode  : Skill.Files.Write_Mode) return Skill.Files.File;
end Skill.Internal.File_Parsers;
