--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     synchronization in skill                            --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski, Timm Felden                  --
--                                                                            --

package body Skill.Synchronization is

   protected body Mutex is

      entry Lock when not Locked is
      begin
         Locked := True;
      end Lock;

      procedure Unlock is
      begin
         Locked := False;
      end Unlock;

   end Mutex;

   protected body Barrier is

      entry Await when 0 = Counter is
      begin
         null;
      end Await;

      procedure Start is
      begin
         Counter := Counter + 1;
      end Start;

      procedure Complete is
      begin
         Counter := Counter - 1;
      end Complete;

   end Barrier;

end Skill.Synchronization;
