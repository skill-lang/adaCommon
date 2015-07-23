--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     API types for skill types                           --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --

with Interfaces;


package Skill.Types is
   pragma Preelaborate;

   subtype i8 is Interfaces.Integer_8 range Interfaces.Integer_8'Range;
   subtype i16 is Interfaces.Integer_16 range Interfaces.Integer_16'Range;
   subtype i32 is Interfaces.Integer_32 range Interfaces.Integer_32'Range;
   subtype i64 is Interfaces.Integer_64 range Interfaces.Integer_64'Range;
   subtype v64 is Interfaces.Integer_64 range Interfaces.Integer_64'Range;
   -- used in places, where v64 values are used as lengths or counts
   subtype Uv64 is Interfaces.Unsigned_64 range Interfaces.Unsigned_64'Range;

   -- TF: we can not restrict range, because that would destroy NaNs, right?
   subtype F32 Is Interfaces.IEEE_Float_32;
   subtype F64 is Interfaces.IEEE_Float_64;

   type String_Access is access String;
   type String_Access_Array is
     array (Integer range <>) of not null String_Access;


   -- declare skill ids type for later configuration
   subtype Skill_ID_T is Integer;

   -- we use integer IDs, because they are smaller and we would
   -- internal use only!
   type Skill_Object is tagged record
      Skill_ID : Skill_ID_T;
   end record;
   type Annotation is access Skill_Object;
   type Annotation_Array_T is array (Natural range <>) of Annotation;
   type Annotation_Array is access Annotation_Array_T;

end Skill.Types;
