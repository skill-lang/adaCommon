--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     API types for skill types                           --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --

with Ada.Unchecked_Conversion;

package body Skill.Types is

   function Hash (This : Box) return Ada.Containers.Hash_Type is
      pragma Warnings (Off);
      function Convert is new Ada.Unchecked_Conversion
        (Box,
         Ada.Containers.Hash_Type);
   begin
      return Ada.Containers.Hash_Type'Mod (Convert (This));
   end Hash;

   -- default type conversion for root type
   function To_Annotation
     (This : access Skill_Object'Class) return Skill.Types.Annotation
   is
      type T is access all Skill_Object;
      function Cast is new Ada.Unchecked_Conversion (T, Annotation);
   begin
      return Cast (T (This));
   end To_Annotation;

   function Skill_Name (This : access Skill_Object) return String_Access is
   begin
      raise Constraint_Error with "requires dispatching call!";
      return null;
   end Skill_Name;

   function Dynamic (This : access Skill_Object) return Annotation_Dyn is
      type T is access all Skill_Object;
      function Cast is new Ada.Unchecked_Conversion (T, Annotation_Dyn);
   begin
      return Cast (T (This));
   end Dynamic;

   -- reflective getter
   function Reflective_Get
     (This : access Skill_Object;
      F    : Skill.Field_Declarations.Field_Declaration) return Box
   is
      function Cast is new Ada.Unchecked_Conversion (Annotation, Box);
   begin
      raise Constraint_Error
        with "reflective get not implemented for skill objects";
      return Cast (null);
   end Reflective_Get;

   -- reflective setter
   procedure Reflective_Set
     (This : access Skill_Object;
      F    : Field_Declarations.Field_Declaration;
      V    : Box)
   is
   begin
      raise Constraint_Error
        with "reflective set not implemented for skill objects";
   end Reflective_Set;

end Skill.Types;
