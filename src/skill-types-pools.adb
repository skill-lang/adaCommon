--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     type handling in skill                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;

with Skill.Field_Types;
with Skill.Internal.Parts;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;

-- TODO push down:
--  type A2 is not null access T;
--  package New_Objects_T is new Ada.Containers.Vectors (Natural, A2);
--
--  -- objects that have not yet been written to disk
--  New_Objects : New_Objects_T.Vector;

-- pool realizations are moved to the pools.adb, because this way we can work
-- around several restrictions of the (generic) ada type system.
package body Skill.Types.Pools is

   function Dynamic (This : access Pool_T) return Pool_Dyn is
      type P is access all Pool_T;
      function Convert is new Ada.Unchecked_Conversion (P, Pool_Dyn);
   begin
      return Convert (P (This));
   end Dynamic;

   function To_Pool (This : access Pool_T'Class) return Pool is
      type T is access all Pool_T;
      function Convert is new Ada.Unchecked_Conversion (T, Pool);
   begin
      return Convert (T (This));
   end To_Pool;

   -- pool properties

   function To_String (This : Pool_T) return String is (This.Name.all);

   function Skill_Name
     (This : access Pool_T) return String_Access is
     (This.Name);

   function ID (This : access Pool_T) return Natural is (This.Type_Id);

   function Base (This : access Pool_T) return Base_Pool is (This.Base);

   function Super (This : access Pool_T) return Pool is (This.Super);

   function Size (This : access Pool_T'Class) return Natural is
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

      Size := This.Dynamic.Static_Size;
      This.Sub_Pools.Foreach (F'Access);

      return Size;
   end Size;

   procedure Fixed (This : access Pool_T'Class; Fix : Boolean) is
   begin
      if This.Fixed = Fix then
         return;
      end if;

      if Fix then
         This.Cached_Size := This.Size;
      end if;
      This.Fixed := Fix;
   end Fixed;


   procedure Do_In_Type_Order (This : access Pool_T'Class;
                               F : access procedure(I : Annotation)) is

      procedure Closure (This : Sub_Pool) is
      begin
         This.Do_In_Type_Order(F);
      end Closure;
   begin
      This.Do_For_Static_Instances(F);
      This.Sub_Pools.Foreach(Closure'Access);
   end;

   function Blocks
     (This : access Pool_T) return Skill.Internal.Parts.Blocks is
     (This.Blocks);

   function Data_Fields
     (This : access Pool_T) return Skill.Field_Declarations.Field_Vector is
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

   function Add_Field
     (This : access Base_Pool_T;
      ID   : Natural;
      T    : Field_Types.Field_Type;
      Name : String_Access) return Skill.Field_Declarations.Field_Declaration
   is

      type P is access all Pool_T;
      function Convert is new Ada.Unchecked_Conversion (P, Pool);
   begin
      return Convert (P (This)).Add_Field (ID, T, Name);
   end Add_Field;

   function Add_Field
     (This : access Sub_Pool_T;
      ID   : Natural;
      T    : Field_Types.Field_Type;
      Name : String_Access) return Skill.Field_Declarations.Field_Declaration
   is

      type P is access all Pool_T;
      function Convert is new Ada.Unchecked_Conversion (P, Pool);
   begin
      return Convert (P (This)).Add_Field (ID, T, Name);
   end Add_Field;

   function Pool_Offset (This : access Pool_T'Class) return Integer is
   begin
      return This.Type_Id;
   end Pool_Offset;

   function Sub_Pools
     (This : access Pool_T'Class) return Sub_Pool_Vector is
     (This.Sub_Pools);

   function Known_Fields (This  : access Pool_T'Class) return String_Access_Array_Access is
      (This.Known_Fields);

   -- base pool properties

   -- internal use only
   function Data
     (This : access Base_Pool_T) return Skill.Types.Annotation_Array is
     (This.Data);

   procedure Compress (This : access Base_Pool_T'Class; Lbpo_Map : Skill.Internal.Lbpo_Map_T) is
      D : Annotation_Array := new Annotation_Array_T(1 .. This.Size);
      P : Skill_ID_T := 0;

      procedure Update(I : Annotation) is
      begin
         D(P) := I;
         P := P + 1;
         I.Skill_Id := P;
      end update;
   begin
      This.Do_In_Type_Order(Update'Access);

      This.Data := D;
      This.Update_After_Compress(Lbpo_Map);
   end Compress;



   -- invoked by resize pool (from base pool implementation)
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

   procedure Set_Owner
     (This  : access Base_Pool_T'Class;
      Owner : access Skill.Files.File_T'Class)
   is

      type P is access all Skill.Files.File_T'Class;
      function Convert is new Ada.Unchecked_Conversion (P, Owner_T);
   begin
      This.Owner := Convert (P (Owner));
   end Set_Owner;

   -- sub pool properties

end Skill.Types.Pools;
