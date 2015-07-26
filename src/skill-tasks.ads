--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     tasks package                                       --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski                               --
--                                                                            --

--  usage:
--  X : Run (Some_Procedure'Access);
--  X.Start;
package Skill.Tasks is
   pragma Pure;

   task type Run (Runnable : access procedure) is
      entry Start;
   end Run;

end Skill.Tasks;
