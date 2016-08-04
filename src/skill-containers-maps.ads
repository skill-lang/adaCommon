--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     skills vector container implementation              --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski, Timm Felden                  --
--                                                                            --
pragma Ada_2012;

with Ada.Containers.Hashed_Maps;
with Ada.Finalization;
with Ada.Unchecked_Conversion;

with Skill.Types;
with Ada.Containers;

-- sets used by skill; those are basically ada hashed sets with template aware boxing
generic
   type K is private;
   type V is private;

   with function Hash (Element : K) return Ada.Containers.Hash_Type is <>;

   with function Equals (Left, Right : K) return Boolean is <>;

   with function "=" (Left, Right : V) return Boolean is <>;
package Skill.Containers.Maps is
   pragma Warnings (Off);

   use Skill.Types;

   function CastK is new Ada.Unchecked_Conversion (Box, K);
   function CastK is new Ada.Unchecked_Conversion (K, Box);
   -- if K = V, we would be fucked, if we'd call both cast
   function CastV is new Ada.Unchecked_Conversion (Box, V);
   function CastV is new Ada.Unchecked_Conversion (V, Box);

   package HS is new Ada.Containers.Hashed_Maps (K, V, Hash, Equals, "=");


   type Iterator_T is new Map_Iterator_T with record
      Cursor : Hs.Cursor;
   end record;

   function Has_Next (This : access Iterator_T) return Boolean is
     (Hs.Has_Element(This.Cursor));

   function Key (This : access Iterator_T) return Skill.Types.Box is
     (CastK(Hs.Key(This.Cursor)));

   function Value (This : access Iterator_T) return Skill.Types.Box is
     (CastV(Hs.Element(This.Cursor)));

   procedure Advance (This : access Iterator_T);

   procedure Free (This : access Iterator_T);


   type Map_T is new Boxed_Map_T with record
      This : HS.Map;
   end record;
   type Ref is access Map_T;

   function Contains
     (This : access Map_T;
      V    : Skill.Types.Box) return Boolean is
     (This.This.Contains (CastK (V)));

   function Get
     (This : access Map_T;
      K    : Skill.Types.Box) return Skill.Types.Box is
     (CastV(This.This.Element(CastK(K))));

   procedure Update
     (This : access Map_T;
      K    : Skill.Types.Box;
      V    : Skill.Types.Box);

   procedure Remove
     (This : access Map_T;
      K    : Skill.Types.Box);

   function Length
     (This : access Map_T) return Natural is
     (Natural (This.This.Length));

   overriding
   function Iterator (This : access Map_T) return Map_Iterator is
      (new Iterator_T'(Cursor => This.This.First));

   -- create a new container
   function Make return Ref;

   -- turn a box into a container of right type
   function Unboxed is new Ada.Unchecked_Conversion (Box, Ref);

end Skill.Containers.Maps;
