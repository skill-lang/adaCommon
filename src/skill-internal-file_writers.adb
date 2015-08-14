--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file writer implementation                          --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

-- documentation can be found in java common
-- this is a combination of serialization functions, write and append
package body Skill.Internal.File_Writers is

   -- write a file to disk
   procedure Write
     (State  : Skill.Files.File;
      Output : Skill.Streams.Writer.Output_Stream) is
   begin
      null;
   end;

   -- append a file to an existing one
   procedure Append
     (State  : Skill.Files.File;
      Output : Skill.Streams.Writer.Output_Stream) is
   begin null; end;

end Skill.Internal.File_Writers;
