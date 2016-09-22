--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     error reporting                                     --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

with Ada.Exceptions;

package Skill.Errors is

   -- this exception is used for skill error reporting
   Skill_Error : exception;

end Skill.Errors;
