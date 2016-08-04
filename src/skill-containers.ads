--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     skill containers collection                         --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

limited with Skill.Types;

-- this packe solves most of ada container's shortcomings
package Skill.Containers is

   -- dispatching arrays
   type Boxed_Array_T is abstract tagged null record;
   type Boxed_Array is access all Boxed_Array_T'Class;

   procedure Append
     (This : access Boxed_Array_T;
      V    : Skill.Types.Box) is abstract;
   -- same as append; used for uniformity
   procedure Add
     (This : access Boxed_Array_T;
      V    : Skill.Types.Box) is abstract;
   function Get
     (This : access Boxed_Array_T;
      I    : Natural) return Skill.Types.Box is abstract;
   procedure Update
     (This : access Boxed_Array_T;
      I    : Natural;
      V    : Skill.Types.Box) is abstract;

   function Length (This : access Boxed_Array_T) return Natural is abstract;
   procedure Ensure_Size
     (This : access Boxed_Array_T;
      I    : Natural) is abstract;

   -- boxed set access
   type Set_Iterator_T is abstract tagged null record;
   type Set_Iterator is access all Set_Iterator_T'Class;

   function Has_Next (This : access Set_Iterator_T) return Boolean is abstract;
   function Next
     (This : access Set_Iterator_T) return Skill.Types.Box is abstract;
   procedure Free (This : access Set_Iterator_T) is abstract;

   -- dispatching sets
   type Boxed_Set_T is abstract tagged null record;
   type Boxed_Set is access all Boxed_Set_T'Class;

   procedure Add (This : access Boxed_Set_T; V : Skill.Types.Box) is abstract;
   function Contains
     (This : access Boxed_Set_T;
      V    : Skill.Types.Box) return Boolean is abstract;

   function Length (This : access Boxed_Set_T) return Natural is abstract;
   function Iterator
     (This : access Boxed_Set_T) return Set_Iterator is abstract;

   -- boxed map access
   type Map_Iterator_T is abstract tagged null record;
   type Map_Iterator is access all Map_Iterator_T'Class;

   function Has_Next (This : access Map_Iterator_T) return Boolean is abstract;
   function Key
     (This : access Map_Iterator_T) return Skill.Types.Box is abstract;
   function Value
     (This : access Map_Iterator_T) return Skill.Types.Box is abstract;
   procedure Advance (This : access Map_Iterator_T) is abstract;
   procedure Free (This : access Map_Iterator_T) is abstract;

   -- dispatching maps
   type Boxed_Map_T is abstract tagged null record;
   type Boxed_Map is access all Boxed_Map_T'Class;

   function Contains
     (This : access Boxed_Map_T;
      V    : Skill.Types.Box) return Boolean is abstract;
   function Get
     (This : access Boxed_Map_T;
      K    : Skill.Types.Box) return Skill.Types.Box is abstract;
   procedure Update
     (This : access Boxed_Map_T;
      K    : Skill.Types.Box;
      V    : Skill.Types.Box) is abstract;

   procedure Remove
     (This : access Boxed_Map_T;
      K    : Skill.Types.Box) is abstract;

   function Length (This : access Boxed_Map_T) return Natural is abstract;
   function Iterator
     (This : access Boxed_Map_T) return Map_Iterator is abstract;

end Skill.Containers;
