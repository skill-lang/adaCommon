--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     synchronization in skill                            --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski                               --
--                                                                            --

package Skill.Synchronization is

   pragma Pure;

   protected type Mutex is
      entry Lock;
      procedure Unlock;
   private
      Locked : Boolean := False;
   end Mutex;

end Skill.Synchronization;
