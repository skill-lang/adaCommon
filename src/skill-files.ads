--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     general file interaction                            --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

with Ada.Containers.Hashed_Maps;

with Skill.Types;
with Skill.Types.Pools;
with Skill.Containers.Vectors;
with Skill.String_Pools;
with Skill.Hashes;
with Skill.Equals;
with Skill.Field_Types.Builtin;
with Skill.Field_Types.Builtin.String_Type_P;

package Skill.Files is

   type Read_Mode is (Create, Read);
   type Write_Mode is (Write, Append, Destroyed);

   type File_T is abstract tagged limited record
      -- path used for flush/close operations
      Path : Skill.Types.String_Access;

      -- current write mode
      Mode : Write_Mode;

      -- strings stored in this file
      Strings : Skill.String_Pools.Pool;

      -- string type used for string RTTI
      String_Type : Skill.Field_Types.Builtin.String_Type_P.Field_Type;
      -- annotation type used for annotations RTTI
      Annotation_Type : Skill.Field_Types.Builtin.Annotation_Type_P.Field_Type;

      -- types stored in this file
      Types : Skill.Types.Pools.Type_Vector;

      -- types by skill name
      Types_By_Name : Skill.Types.Pools.Type_Map;
   end record;
   type File is not null access File_T'Class;

   function Strings
     (This : access File_T'Class) return Skill.String_Pools.Pool;

   -- change the output path
   -- @note can currently only be used in write mode
   procedure Change_Path (This : access File_T'Class; New_Path : String);

   -- checks restrictions
   -- raises skill_error, if a check fails
   procedure Check (This : access File_T'Class);

   -- write changes to disk
   procedure Flush (This : access File_T'Class);

   procedure Free (This : access File_T) is abstract;

   -- internal use only
   procedure Finalize_Pools (This : access File_T'Class);

end Skill.Files;
