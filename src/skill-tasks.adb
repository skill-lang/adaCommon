--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     tasks package                                       --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski                               --
--                                                                            --

package body Skill.Tasks is

   task body Run is
   begin
      accept Start (C : Closure) do
         Runnable.all (C => C);
      end Start;
   end Run;

end Skill.Tasks;
