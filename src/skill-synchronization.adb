--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     synchronization in skill                            --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski                               --
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

end Skill.Synchronization;
