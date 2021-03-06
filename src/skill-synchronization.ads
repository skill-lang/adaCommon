--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     synchronization in skill                            --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski, Timm Felden                  --
--                                                                            --
pragma Ada_2012;

package Skill.Synchronization is

   pragma Pure;

   protected type Mutex is
      entry Lock;
      procedure Unlock;
   private
      Locked : Boolean := False;
   end Mutex;

   protected type Barrier is
      entry Await;
      procedure Start;
      procedure Complete;
   private
      Counter : Natural := 0;
   end Barrier;

end Skill.Synchronization;
