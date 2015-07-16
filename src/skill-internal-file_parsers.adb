--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file parser implementation                          --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;
with Skill.Files;
with Skill.Types;
with Skill.Streams.Reader;
with Ada.Text_Io;

-- documentation can be found in java common
package body Skill.Internal.File_Parsers is

   use Skill;

   procedure Print (I : Types.I8) is
   begin
      Ada.Text_Io.Put_Line (Integer'Image (Integer(I)));
      end Print;

   -- TODO parametrization
   function Read
     (Input : Streams.Input_Stream;
      Mode  : Files.Write_Mode) return Skill.Files.File
   is

      -- read an entire string block
      procedure String_Block is
         V : Types.I8 := Input.I8;
      begin
         Ada.Text_IO.Put_Line(Integer'Image(INteger(V)));
      end String_Block;

      -- read an entire type block
      procedure Type_Block is
      begin
         null;
      end Type_Block;

      -- build a state from intermediate information
      function Make_State return Files.File is
         Result : Files.File := new Skill.Files.File_T;
      begin
         return Result;
      end Make_State;

   begin

      while not Input.Eof loop

         String_Block;
         Type_Block;
      end loop;

      return Make_State;
   end Read;
end Skill.Internal.File_Parsers;
