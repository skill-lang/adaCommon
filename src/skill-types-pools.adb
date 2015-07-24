--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     type handling in skill                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;

with Skill.Field_Types;
with Skill.Internal.Parts;
with Ada.Unchecked_Conversion;

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

   function Size (This : access Pool_T) return Natural is
      Size : Natural;

      type P is access all Pool_T;
      type D is access Pool_T'Class;
      function Convert is new Ada.Unchecked_Conversion (P, D);

      procedure F (I : Sub_Pool) is
      begin
         Size := Size + I.Size;
      end F;

   begin
      if This.Fixed then
         return This.Cached_Size;
      end if;

      Size := Convert (P (This)).Static_Size;
      This.Sub_Pools.Foreach (F'Access);

      return Size;
   end Size;

   function Blocks
     (This : access Pool_T) return Skill.Internal.Parts.Blocks is
     (This.Blocks);

   function Data_Fields
     (This : access Pool_T)
      return Skill.Field_Declarations.Field_Vector is
     (This.Data_Fields_F);

   -- internal use only
   function Add_Field
     (This : access Pool_T;
      ID   : Natural;
      T    : Field_Types.Field_Type;
      Name : String_Access) return Skill.Field_Declarations.Field_Declaration

   is
      function Convert is new Ada.Unchecked_Conversion
        (Field_Declarations.Lazy_Field,
         Field_Declarations.Field_Declaration);
      type P is access all Pool_T;
      function Convert is new Ada.Unchecked_Conversion
        (P,
         Field_Declarations.Owner_T);

      F : Field_Declarations.Field_Declaration :=
        Convert
          (Skill.Field_Declarations.Make_Lazy_Field
             (Convert (P (This)),
              ID,
              T,
              Name));
   begin
      -- TODO restrictions
      --          for (FieldRestriction<?> r : restrictions)
      --              f.addRestriction(r);
      This.Data_Fields.Append (F);

      return F;
   end Add_Field;

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
