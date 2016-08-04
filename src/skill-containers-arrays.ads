--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     skills vector container implementation              --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski, Timm Felden                  --
--                                                                            --
pragma Ada_2012;

with Ada.Finalization;
with Ada.Unchecked_Conversion;

with Skill.Types;
with Skill.Containers.Vectors;

-- arrays used by skill; those are basically ada vectors with template aware boxing
generic
   type T is private;
package Skill.Containers.Arrays is
   pragma Warnings(Off);

   use Skill.Types;

   function Cast is new Ada.Unchecked_Conversion (Box, T);
   function Cast is new Ada.Unchecked_Conversion (T, Box);

   package Vec is new Skill.Containers.Vectors (Natural, T);

   type Array_T is new Boxed_Array_T with record
      This : Vec.Vector;
   end record;

   type Ref is access Array_T;

   procedure Append (This : access Array_T; V : Box);
   procedure Add (This : access Array_T; V : Box);
   function Get
     (This : access Array_T;
      I    : Natural) return Box is
     (Cast (This.This.Element (I)));

   procedure Update (This : access Array_T; I : Natural; V : Box);

   procedure Ensure_Size (This : access Array_T; I : Natural);

   function Length
     (This : access Array_T) return Natural is
     (Vec.Length (This.This));

   -- create a new container
   function Make return Ref;

   -- turn a box into a container of right type
   function Unboxed is new Ada.Unchecked_Conversion (Box, Ref);

end Skill.Containers.Arrays;
