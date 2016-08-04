--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     runtime field restriction handling                  --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

with Ada.Containers.Vectors;
with Ada.Unchecked_Conversion;

with Skill.Types;

package Skill.Field_Restrictions is
   pragma Warnings (Off);
   use type Skill.Types.Annotation;

   type Base_T is abstract tagged null record;
   -- ID as of SkillTR
   function Id (This : access Base_T) return Integer is abstract;
   type Base is access all Base_T'Class;

   package Vector_P is new Ada.Containers.Vectors (Natural, Base);
   subtype Vector is Vector_P.Vector;
   function Empty return Vector is
      (Vector_P.Empty_Vector);

   type Checkable_T is abstract new Base_T with null record;
   function Id (This : access Checkable_T) return Integer is abstract;
   -- checks argument v, return true iff argument fulfills the condition
   function Check (This : access Checkable_T; V : Skill.Types.Box)
                   return Boolean is abstract;
   type Checkable is access all Checkable_T'Class;

   -- just because Ada sucks
   function To is new Ada.Unchecked_Conversion(Base, Checkable);
   function To is new Ada.Unchecked_Conversion(Skill.Types.Box, Skill.Types.Annotation);


   type Nonnull_T is new Checkable_T with null record;

   overriding
   function Id (This : access Nonnull_T) return Integer is
     (0);
   overriding
   function Check (This : access Nonnull_T; V : Skill.Types.Box)
                   return Boolean is
     (To(V)/=null);

   -- returns the nonnull instance
   function Nonnull return Base;


   generic
      type T is private;
   package Default is
      type Restriction is new Base_T with record
         Value : T;
      end record;
      type Ref is access Restriction;

      overriding
      function Id (This : access Restriction) return Integer is
        (1);
   end Default;

   generic
      type T is private;

      With function "<=" (L,R : T) return Boolean is <>;
      With function To (V : Skill.Types.Box) return T is <>;
   package Rage is
      type Restriction is new Checkable_T with record
         Min : T;
         Max : T;
      end record;
      type Ref is access Restriction;


      overriding
      function Id (This : access Restriction) return Integer is
        (3);

      overriding
      function Check (This : access Restriction; V : Skill.Types.Box)
                      return Boolean is
         (This.Min <= To(V) and then To(V) <= This.Max);
   end Rage;

   type Coding_T is new Base_T with record
      Name : Skill.Types.String_Access;
   end record;
   overriding
   function Id (This : access Coding_T) return Integer is
     (5);

   type Constant_Length_Pointer_T is new Base_T with null record;
   overriding
   function Id (This : access Constant_Length_Pointer_T) return Integer is
     (7);
   function Constant_Length_Pointer return Base;


end Skill.Field_Restrictions;
