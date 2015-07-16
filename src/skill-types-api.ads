--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     API types for skill types                           --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --

with Interfaces;


package Skill.Types.Api is

   subtype i8 is Interfaces.Integer_8 range Interfaces.Integer_8'Range;
   subtype i16 is Interfaces.Integer_16 range Interfaces.Integer_16'Range;
   subtype i32 is Interfaces.Integer_32 range Interfaces.Integer_32'Range;
   subtype i64 is Interfaces.Integer_64 range Interfaces.Integer_64'Range;
   subtype v64 is Interfaces.Integer_64 range Interfaces.Integer_64'Range;

   -- TF: we can not restrict range, because that would destroy NaNs, right?
   subtype F32 Is Interfaces.IEEE_Float_32;
   subtype F64 is Interfaces.IEEE_Float_64;

   -- note string is types.string_access

   type Annotation is access Skill_Object;

end Skill.Types.Api;
