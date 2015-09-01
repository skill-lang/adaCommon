--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     tasks package                                       --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski                               --
--                                                                            --

--  usage:
--  X : Run (Some_Procedure'Access);
--  X.Start;
package Skill.Tasks is
   pragma Preelaborate;

   type Closure_T is tagged null record;
   type Closure is not null access Closure_T'Class;

   task type Run (Runnable : not null access procedure(C : Closure)) is
      entry Start (C : Closure);
   end Run;

end Skill.Tasks;
