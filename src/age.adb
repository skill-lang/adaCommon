--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     !! remove after integration into generator !!       --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Unchecked_Conversion;


-- types generated out of the specification
package body Age is

   function To_Age (This : access Skill.Types.Skill_Object) return Age
   is
      type T is access all Skill.Types.Skill_Object;
      function Convert is new Ada.Unchecked_Conversion (T, Age);
   begin
      return Convert (T (This));
   end To_Age;

end Age;
