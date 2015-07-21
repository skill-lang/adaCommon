--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     general file interaction                            --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Hashed_Maps;

with Skill.Types;
with Skill.Types.Pools;
with Skill.Types.Vectors;
with Skill.String_Pools;
with Skill.Hashes;
with Skill.Equals;

package Skill.Files is


   type Read_Mode is (Create, Read);
   type Write_Mode is (Write, Append);


   package P_Type_Vector is new Skill.Types.Vectors
     (Index_Type   => Natural,
      Element_Type => Skill.Types.Pools.Pool);
   type Type_Vector is not null access P_Type_Vector.Vector;

   use type Skill.Types.Pools.Pool;
   package P_Type_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Skill.Types.String_Access,
      Element_Type    => Skill.Types.Pools.Pool,
      Hash            => Skill.Hashes.Hash,
      Equivalent_Keys => Skill.Equals.Equals);
   type Type_Map is not null access P_Type_Map.Map;


   type File_T is abstract tagged limited record
   -- path used for flush/close operations
      Path : Skill.Types.String_Access;

      -- current write mode
      Mode : Write_Mode;

      -- strings stored in this file
      Strings : Skill.String_Pools.Pool;

      -- types stored in this file
      Types : Type_Vector;

      -- types by skill name
      Types_By_Name : Type_Map;
   end record;
   type File is not null access File_T'Class;


   function Strings (This : access File_T'Class) return Skill.String_Pools.Pool;

   -- internal use only
   -- should be abstract eventually!!
   function Finish_Allocation
     (Path          : Skill.Types.String_Access;
      Mode          : Write_Mode;
      Strings       : Skill.String_Pools.Pool;
      Types         : Type_Vector;
      Types_By_Name : Type_Map) return File;




end Skill.Files;
