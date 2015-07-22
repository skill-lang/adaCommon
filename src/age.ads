--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     !! remove after integration into generator !!       --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Types;

-- types generated out of the specification
package Age is
   pragma Preelaborate;

   type Age_T is new Skill.Types.Skill_Object with record
      Age : Skill.Types.v64;
   end record;
   type Age is access Age_T;

   procedure My_Lib_Dummy;
end Age;
