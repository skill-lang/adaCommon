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

   -- prints currents stack trace
   -- can be used for debugging purpose and is used to enhance error reporting
   -- in generated code.
   procedure Print_Stacktrace;

   -- prints the exceptions stack trace
   -- can be used for debugging purpose and is used to enhance error reporting
   -- in generated code.
   procedure Print_Stacktrace (E : in Ada.Exceptions.Exception_Occurrence);

end Skill.Errors;
