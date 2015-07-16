--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     general file interaction                            --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Types;
with Skill.Types.Pools;
with Skill.Types.Vectors;

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

private
   package A2 is new Skill.Types.Vectors (Index_Type   => Natural,
                                          Element_Type => Skill.Types.String_Access);
   subtype Type_Vector is A2.Vector;

   type File_T is tagged limited record
      -- path used for flush/close operations
      Path : Skill.Types.String_Access;

      -- current write mode
      Mode : Write_Mode;

      -- types stored in this file
      Types : Type_Vector;
   end record;

end Skill.Files;
