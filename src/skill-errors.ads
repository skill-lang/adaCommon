--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     error reporting                                     --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Exceptions;

package Skill.Errors is

   -- prints currents stack trace
   -- can be used for debugging purpose and is used to enhance error reporting
   -- in generated code.
   procedure Print_Stacktrace;

   -- prints the exceptions stack trace
   -- can be used for debugging purpose and is used to enhance error reporting
   -- in generated code.
   procedure Print_Stacktrace (E : in Ada.Exceptions.Exception_Occurrence);

end Skill.Errors;
