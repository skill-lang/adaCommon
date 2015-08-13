--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     API types for skill types                           --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --

with Ada.Unchecked_Conversion;

package body Skill.Types is

   -- default type conversion for root type
   function To_Annotation
     (This : access Skill_Object'Class) return Skill.Types.Annotation
   is
      type T is access all Skill_Object;
      function Cast is new Ada.Unchecked_Conversion (T, Annotation);
   begin
      return Cast (T (This));
   end To_Annotation;

   function Dynamic (This : access Skill_Object) return Annotation_Dyn is
      type T is access all Skill_Object;
      function Cast is new Ada.Unchecked_Conversion (T, Annotation_Dyn);
   begin
      return Cast (T (This));
   end Dynamic;

end Skill.Types;
