--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     type handling in skill                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;

with Skill.Field_Types;
with Skill.Internal.Parts;

-- TODO push down:
--  type A2 is not null access T;
--  package New_Objects_T is new Ada.Containers.Vectors (Natural, A2);
--
--  -- objects that have not yet been written to disk
--  New_Objects : New_Objects_T.Vector;

-- pool realizations are moved to the pools.adb, because this way we can work
-- around several restrictions of the (generic) ada type system.
package body Skill.Types.Pools is

   -- pool properties

   function To_String (This : Pool_T) return String is (This.Name.all);

   function Skill_Name
     (This : access Pool_T) return String_Access is
     (This.Name);

   function ID (This : access Pool_T) return Natural is (This.Type_Id);

   function Base (This : access Pool_T) return Base_Pool is (This.Base);

   function Super (This : access Pool_T) return Pool is (This.Super);

   function Blocks
     (This : access Pool_T) return Skill.Internal.Parts.Blocks is
     (This.Blocks);

   -- base pool properties

   -- internal use only
   function Data
     (This : access Base_Pool_T) return Skill.Types.Annotation_Array is
     (This.Data);

   procedure Resize_Data (This : access Base_Pool_T) is
   -- data = Arrays.copyOf(data, data.length + (int) blocks.getLast().count);
      Count : Types.v64        := This.Blocks.Last_Element.Count;
      D     : Annotation_Array :=
        new Annotation_Array_T
        (This.Data'First .. (This.Data'Last + Natural (Count)));
   begin
      for I in This.Data'First .. This.Data'Last loop
         D (I) := This.Data (I);
      end loop;
      This.Data := D;
   end Resize_Data;

   -- sub pool properties

end Skill.Types.Pools;
