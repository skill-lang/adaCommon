--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Your SKilL Scala Binding                            --
-- \__ \ ' <| | | |__     <<debug>>                                           --
-- |___/_|\_\_|_|____|    by: <<some developer>>                              --
--                                                                            --

with Skill.Types;

-- types generated out of the specification
package Age is
   pragma Preelaborate;

   type Age_T is new Skill.Types.Skill_Object with private;
   --  The age of a person.
   --  @author  Timm Felden
   type Age is access Age_T;

   -- Age type conversions
   function To_Age (This : access Skill.Types.Skill_Object) return Age;

   -- Age fields

   --  People have a small positive age, but maybe they will start to live
   --  longer in the future, who knows
   function Get_Age (This : access Age_T'Class) return Skill.Types.V64;
   --  People have a small positive age, but maybe they will start to live
   --  longer in the future, who knows
   procedure Set_Age (This : access Age_T'Class; V : Skill.Types.V64);

private

   type Age_T is new Skill.Types.Skill_Object with record
      Age : Skill.Types.V64;
   end record;

end Age;
