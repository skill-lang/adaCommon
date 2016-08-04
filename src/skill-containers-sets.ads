--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     skills vector container implementation              --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski, Timm Felden                  --
--                                                                            --
pragma Ada_2012;

with Ada.Containers.Hashed_Sets;
with Ada.Finalization;
with Ada.Unchecked_Conversion;

with Skill.Types;
with Ada.Containers;

-- sets used by skill; those are basically ada hashed sets with template aware boxing
generic
   type T is private;

   with function Hash (Element : T) return Ada.Containers.Hash_Type is <>;

   with function Equals (Left, Right : T) return Boolean is <>;

   with function "=" (Left, Right : T) return Boolean is <>;
package Skill.Containers.Sets is
   pragma Warnings (Off);

   use Skill.Types;

   function Cast is new Ada.Unchecked_Conversion (Box, T);
   function Cast is new Ada.Unchecked_Conversion (T, Box);

   package HS is new Ada.Containers.Hashed_Sets (T, Hash, Equals, "=");


   type Iterator_T is new Set_Iterator_T with record
      Cursor : Hs.Cursor;
   end record;

   function Has_Next (This : access Iterator_T) return Boolean is
     (Hs.Has_Element(This.Cursor));

   function Next (This : access Iterator_T) return Skill.Types.Box;

   procedure Free (This : access Iterator_T);


   type Set_T is new Boxed_Set_T with record
      This : HS.Set;
   end record;
   type Ref is access Set_T;

   procedure Add (This : access Set_T; V : Skill.Types.Box);
   function Contains
     (This : access Set_T;
      V    : Skill.Types.Box) return Boolean is
     (This.This.Contains (Cast (V)));

   function Length
     (This : access Set_T) return Natural is
     (Natural (This.This.Length));

   overriding
   function Iterator (This : access Set_T) return Set_Iterator is
      (new Iterator_T'(Cursor => This.This.First));

   -- create a new container
   function Make return Ref;

   -- turn a box into a container of right type
   function Unboxed is new Ada.Unchecked_Conversion (Box, Ref);

end Skill.Containers.Sets;
