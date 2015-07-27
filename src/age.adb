--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Your SKilL Scala Binding                            --
-- \__ \ ' <| | | |__     <<debug>>                                           --
-- |___/_|\_\_|_|____|    by: <<some developer>>                              --
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

   -- Age fields

   function Get_Age (This : access Age_T'Class) return Skill.Types.V64
   is
   begin
      return This.Age;
   end Get_Age;

   procedure Set_Age (This : access Age_T'Class; V : Skill.Types.V64)
   is
   begin
      This.Age := V;
   end Set_Age;

end Age;
