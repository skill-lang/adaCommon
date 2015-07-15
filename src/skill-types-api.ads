--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     API types for skill types                           --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Interfaces;


package Skill.Types.Api is

   type I8 is new Interfaces.Integer_8;
   type I16 is new Interfaces.Integer_16;
   type I32 is new Interfaces.Integer_32;
   type I64 is new Interfaces.Integer_64;
   subtype V64 is I64;

   type F32 is new Interfaces.IEEE_Float_32;
   type F64 is new Interfaces.IEEE_Float_64;

   -- note string is types.string_access

   type Annotation is access Skill_Object;

end Skill.Types.Api;
