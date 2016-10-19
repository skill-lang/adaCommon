--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     API types for skill types                           --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --
pragma Ada_2012;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;
with Ada.Tags;

with Interfaces;
with System;
limited with Skill.Field_Declarations;
with Skill.Containers;

package Skill.Types is

   -- this is a boxed object; it is required, because one can not mix generic
   -- and object oriented polymorphism in ada.
   -- we use size of pointer and store all regular object in it by just abusing
   -- the space :-]
   type Box is access String;

   function Hash (This : Box) return Ada.Containers.Hash_Type;

   subtype i8 is Interfaces.Integer_8 range Interfaces.Integer_8'Range;
   subtype i16 is Interfaces.Integer_16 range Interfaces.Integer_16'Range;
   subtype i32 is Interfaces.Integer_32 range Interfaces.Integer_32'Range;
   subtype i64 is Interfaces.Integer_64 range Interfaces.Integer_64'Range;
   subtype v64 is Interfaces.Integer_64 range Interfaces.Integer_64'Range;
   -- used in places, where v64 values are used as lengths or counts
   subtype Uv64 is Interfaces.Unsigned_64 range Interfaces.Unsigned_64'Range;

   -- TF: we can not restrict range, because that would destroy NaNs, right?
   subtype F32 is Interfaces.IEEE_Float_32;
   subtype F64 is Interfaces.IEEE_Float_64;

   type String_Access is access String;
   type String_Access_Array is
     array (Integer range <>) of not null String_Access;
   type String_Access_Array_Access is access all String_Access_Array;

   subtype Boxed_Array is Skill.Containers.Boxed_Array;
   subtype Boxed_List is Skill.Containers.Boxed_Array;
   subtype Boxed_Map is Skill.Containers.Boxed_Map;

   -- declare skill ids type for later configuration
   subtype Skill_ID_T is Integer;

   -- we use integer IDs, because they are smaller and we would
   -- internal use only!
   type Skill_Object is tagged record
      Skill_ID : Skill_ID_T;
   end record;
   type Annotation is access all Skill_Object;
   type Annotation_Dyn is access all Skill_Object'Class;
   type Annotation_Array_T is array (Positive range <>) of Annotation;
   type Annotation_Array is access Annotation_Array_T;

   -- default type conversion for root type
   function To_Annotation
     (This : access Skill_Object'Class) return Skill.Types.Annotation;
   pragma Inline (To_Annotation);
   pragma Pure_Function (To_Annotation);

   function Skill_Name (This : access Skill_Object) return String_Access;

   function Dynamic (This : access Skill_Object) return Annotation_Dyn;
   pragma Inline (Dynamic);
   pragma Pure_Function (Dynamic);

   -- return true, iff the argument object will be deleted on the next flush
   -- operation
   -- @note: references to the object carried by other managed skill objects
   --        will be deleted automatically
   function Is_Deleted(This : access Skill_Object'Class) return Boolean is
      (0 = This.Skill_ID);

   function Tag
     (This : access Skill_Object'Class) return Ada.Tags.Tag is
     (This'Tag);
   pragma Inline (Tag);
   pragma Pure_Function (Tag);

   -- reflective getter
   function Reflective_Get
     (This : access Skill_Object;
      F    : Skill.Field_Declarations.Field_Declaration) return Box;

   -- reflective setter
   procedure Reflective_Set
     (This : access Skill_Object;
      F    : Field_Declarations.Field_Declaration;
      V    : Box);

end Skill.Types;
