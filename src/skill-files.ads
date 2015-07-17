--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     general file interaction                            --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Types;
with Skill.Types.Pools;
with Skill.Types.Vectors;
with Skill.String_Pools;

package Skill.Files is

   type File_T is tagged limited private;
   type File is not null access File_T'Class;

   type Read_Mode is (Create, Read);
   type Write_Mode is (Write, Append);

   -- create a new file using the argument path for I/O
   function Open
     (Path    : String;
      Read_M  : Read_Mode  := Read;
      Write_M : Write_Mode := Write) return File;

   function Strings (This : access File_T) return Skill.String_Pools.Pool;

   -- internal use only
   -- should be abstract eventually!!
   function Finish_Allocation
     (Path    : Skill.Types.String_Access;
      Mode    : Write_Mode;
      Strings : Skill.String_Pools.Pool) return File;

private
   package A2 is new Skill.Types.Vectors
     (Index_Type   => Natural,
      Element_Type => Skill.Types.String_Access);
   type Type_Vector is access A2.Vector;

   type File_T is tagged limited record
      -- path used for flush/close operations
      Path : Skill.Types.String_Access;

      -- current write mode
      Mode : Write_Mode;

      -- strings stored in this file
      Strings : Skill.String_Pools.Pool;

      -- types stored in this file
      Types : Type_Vector;
   end record;

end Skill.Files;
