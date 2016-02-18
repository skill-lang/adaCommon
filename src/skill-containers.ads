--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     skill containers collection                         --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

limited with Skill.Types;

package Skill.Containers is

   type Boxed_Array_T is abstract tagged null record;
   type Boxed_Array is access all Boxed_Array_T'Class;

   procedure Append (This : access Boxed_Array_T; V : Skill.Types.Box) is abstract;
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

end Skill.Containers;
