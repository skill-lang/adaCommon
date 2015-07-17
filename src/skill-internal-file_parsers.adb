--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file parser implementation                          --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;
with Skill.Files;
with Skill.Types;
with Skill.Streams.Reader;
with Ada.Text_IO;
with Interfaces;
with Skill.Errors;
with Ada.Unchecked_Conversion;
with Skill.String_Pools;

-- documentation can be found in java common
package body Skill.Internal.File_Parsers is

   use Skill;

   procedure Print (I : Types.i8) is
   begin
      Ada.Text_IO.Put_Line (Integer'Image (Integer (I)));
   end Print;

   -- TODO parametrization
   function Read
     (Input : Streams.Input_Stream;
      Mode  : Files.Write_Mode) return Skill.Files.File
   is
      -- begin error reporting
      Block_Counter : Positive := 1;
      -- end error reporting

      Strings : String_Pools.Pool := String_Pools.Create (Input);

      -- read an entire string block
      procedure String_Block is
         use type Interfaces.Integer_64;
         Count : Types.v64 := Input.V64;
      begin
         if 0 = Count then
            return;
         end if;

         -- read offsets
         declare
            Last : Types.i32 := 0;
            Off : Types.I32;
            use type Interfaces.Integer_32;
            type Offset_Array is
              array (Types.v64 range 0 .. Count - 1) of Types.i32;
            Offsets : Offset_Array;
         begin
            for I in Offset_Array'Range loop
               Offsets (I) := Input.I32;
            end loop;

            for I in Offset_Array'Range loop
               Off  := Offsets (I);
               Strings.AddPosition (Input.Position + Types.V64(Last), Off - Last);
               Last := Off;
            end loop;

            Input.Jump(Input.Position + Types.V64(Last));
         end;

      exception
         when E : others =>
            raise Skill.Errors.Skill_Error
              with Input.Parse_Exception
              (Block_Counter, E, "corrupted string block");
      end String_Block;

      -- read an entire type block
      procedure Type_Block is
      begin
         null;
      end Type_Block;

      -- build a state from intermediate information
      function Make_State return Files.File is
      begin
         return Skill.Files.Finish_Allocation (Path    => Input.Path,
                                               Mode    => Mode,
                                               Strings => Strings);
      end Make_State;

   begin

      while not Input.Eof loop
         String_Block;
         Type_Block;
         Block_Counter := Block_Counter + 1;
      end loop;

      return Make_State;
   end Read;
end Skill.Internal.File_Parsers;
