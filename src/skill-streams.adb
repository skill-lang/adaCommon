--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     stream manipulation package                         --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --

with Interfaces.C;
with Interfaces.C.Pointers;

with Skill.Types;
with Skill.Streams.Reader;
with Skill.Streams.Writer;
with Ada.Unchecked_Conversion;

package body Skill.Streams is

   function Input
     (Path : Skill.Types.String_Access)
      return Skill.Streams.Reader.Input_Stream
   is
   begin
      return Skill.Streams.Reader.Open (Path);
   end Input;

   function Write
     (Path : Skill.Types.String_Access)
      return Skill.Streams.Writer.Output_Stream is
     (Streams.Writer.Open(Path, "w+"));

   function Append
     (Path : Skill.Types.String_Access)
      return Skill.Streams.Writer.Output_Stream is
     (Streams.Writer.Open (Path, "a+"));

   function Invalid_Pointer return Map_Pointer is
      pragma Warnings (Off);
      function Cast is new Ada.Unchecked_Conversion (Integer, Map_Pointer);
   begin
      return Cast (-1);
   end;

end Skill.Streams;
