--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     type handling in skill                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;
with Ada.Unchecked_Conversion;

with Skill.Field_Types;
with Skill.Internal.Parts;
with Ada.Unchecked_Deallocation;
with Skill.Field_Declarations;
with Skill.Streams.Reader;

with Skill.Iterators.Static_Data;

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

   function Base (This : access Pool_T'Class) return Base_Pool is (This.Base);

   function Super (This : access Pool_T) return Pool is (This.Super);

   function Next (This : access Pool_T'Class) return Pool is (This.Next);
   procedure Establish_Next (This : access Base_Pool_T'Class) is
      procedure Set_Next_Pool (This : access Pool_T'Class; Nx : Pool) is
      begin
         if This.Sub_Pools.Is_Empty then
            This.Next := Nx;
         else
            This.Next := This.Sub_Pools.First_Element.To_Pool;
            for I in 0 .. This.Sub_Pools.Length - 2 loop
               Set_Next_Pool
                 (This.Sub_Pools.Element (I).To_Pool,
                  This.Sub_Pools.Element (I + 1).To_Pool);
            end loop;
            Set_Next_Pool (This.Sub_Pools.Last_Element, Nx);
         end if;
      end Set_Next_Pool;
   begin
      Set_Next_Pool (This, null);
   end Establish_Next;

   function Type_Hierarchy_Height
     (This : access Pool_T'Class) return Natural is
     (This.Super_Type_Count);

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

   function Static_Size (This : access Pool_T'Class) return Natural is
   begin
      return This.Static_Data_Instances +
        Natural (This.New_Objects.Length);
   end Static_Size;

   function New_Objects_Size (This : access Pool_T'Class) return Natural is
      (This.New_Objects.Length);


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

   procedure Do_In_Type_Order
     (This : access Pool_T'Class;
      F    : not null access procedure (I : Annotation))
   is

      procedure Closure (This : Sub_Pool) is
      begin
         This.Do_In_Type_Order (F);
      end Closure;
   begin
      This.Do_For_Static_Instances (F);
      This.Sub_Pools.Foreach (Closure'Access);
   end Do_In_Type_Order;


   procedure Do_For_Static_Instances
     (This : access Pool_T'Class;
      F    : not null access procedure (I : Annotation)) is
      Iter : aliased Skill.Iterators.Static_Data.Iterator :=
               Skill.Iterators.Static_Data.Make (This.To_Pool);
   begin
      while Iter.Has_Next loop
         F(Iter.Next);
      end loop;
   end Do_For_Static_Instances;

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
      return This.Type_Id - 32;
   end Pool_Offset;

   function Sub_Pools
     (This : access Pool_T'Class) return Sub_Pool_Vector is
     (This.Sub_Pools);

   function Known_Fields
     (This : access Pool_T'Class) return String_Access_Array_Access is
     (This.Known_Fields);

   -- base pool properties

   -- internal use only
   function Data
     (This : access Base_Pool_T) return Skill.Types.Annotation_Array is
     (This.Data);

   procedure Compress
     (This     : access Base_Pool_T'Class;
      Lbpo_Map : Skill.Internal.Lbpo_Map_T)
   is
      D : Annotation_Array := new Annotation_Array_T (1 .. This.Size);
      P : Skill_ID_T       := 1;

      procedure Update (I : Annotation) is
      begin
         D (P)      := I;
         I.Skill_ID := P;
         P          := P + 1;
      end Update;
   begin
      This.Do_In_Type_Order (Update'Access);

      This.Data := D;
      This.Update_After_Compress (Lbpo_Map);
   end Compress;

   -- invoked by resize pool (from base pool implementation)
   procedure Resize_Data (This : access Base_Pool_T'Class) is
      Count : Types.v64        := This.Blocks.Last_Element.Count;
      D     : Annotation_Array :=
        new Annotation_Array_T
        (This.Data'First .. (This.Data'Last + Natural (Count)));

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Annotation_Array_T,
         Name   => Annotation_Array);
   begin
      D (This.Data'First .. This.Data'Last) := This.Data.all;
      if This.Data /= Empty_Data then
         Free (This.Data);
      end if;
      This.Data := D;
   end Resize_Data;

   -- Called after a prepare append operation to write empty the new objects
   -- buffer and to set blocks correctly
   procedure Update_After_Prepare_Append
     (This      : access Pool_T'Class;
      Chunk_Map : Skill.Field_Declarations.Chunk_Map)
   is

      New_Instances : constant Boolean :=
        null /= This.Dynamic.First_Dynamic_New_Instance;
      New_Pool  : constant Boolean := This.Blocks.Is_Empty;
      New_Field : Boolean          := False;

      procedure Find_New_Field (F : Field_Declarations.Field_Declaration) is
      begin
         if not New_Field and then F.Data_Chunks.Is_Empty then
            New_Field := True;
         end if;
      end Find_New_Field;

   begin
      This.Data_Fields_F.Foreach (Find_New_Field'Access);

      if New_Pool or else New_Instances or else New_Field then
         declare
            -- build block chunk
            Lcount : Natural := This.New_Objects_Size;
            procedure Dynamic_Lcount (P : Sub_Pool) is
            begin
               Lcount := Lcount + P.To_Pool.Dynamic.New_Objects_Size;
               P.Sub_Pools.Foreach (Dynamic_Lcount'Access);
            end Dynamic_Lcount;

            Lbpo : Natural;
         begin
            This.Sub_Pools.Foreach (Dynamic_Lcount'Access);

            if 0 = Lcount then
               Lbpo := 0;
            else
               Lbpo := This.Dynamic.First_Dynamic_New_Instance.Skill_ID - 1;
            end if;

            This.Blocks.Append
            (Skill.Internal.Parts.Block'
               (BPO => Types.v64 (Lbpo), Count => Types.v64 (Lcount)));

            -- @note: if this does not hold for p; then it will not hold for
            -- p.subPools either!
            if New_Instances or else not New_Pool then
               -- build field chunks
               for I in 1 .. This.Data_Fields_F.Length loop
                  declare
                     F : Field_Declarations.Field_Declaration :=
                       This.Data_Fields_F.Element (I);
                     CE : Skill.Field_Declarations.Chunk_Entry;
                  begin
                     if F.Data_Chunks.Is_Empty then
                        CE :=
                          new Skill.Field_Declarations.Chunk_Entry_T'
                            (C =>
                               new Skill.Internal.Parts.Bulk_Chunk'
                                 (First       => Types.v64 (-1),
                                  Last        => Types.v64 (-1),
                                  Count       => Types.v64 (This.Size),
                                  Block_Count => This.Blocks.Length),
                             Input => Skill.Streams.Reader.Empty_Sub_Stream);
                        F.Data_Chunks.Append (CE);
                        Chunk_Map.Include (F, CE.C);
                     elsif New_Instances then
                        CE :=
                          new Skill.Field_Declarations.Chunk_Entry_T'
                            (C =>
                               new Skill.Internal.Parts.Simple_Chunk'
                                 (First => Types.v64 (-1),
                                  Last  => Types.v64 (-1),
                                  Count => Types.v64 (Lcount),
                                  BPO   => Types.v64 (Lbpo)),
                             Input => Skill.Streams.Reader.Empty_Sub_Stream);
                        F.Data_Chunks.Append (CE);
                        Chunk_Map.Include (F, CE.C);
                     end if;
                  end;
               end loop;
            end if;
         end;
      end if;

      -- notify sub pools
      declare
         procedure Update (P : Sub_Pool) is
         begin
            Update_After_Prepare_Append (P, Chunk_Map);
         end Update;
      begin
         This.Sub_Pools.Foreach (Update'Access);
      end;

      -- remove new objects, because they are regular objects by now

      -- TODO if we ever want to get rid of Destroyed mode
      --          staticData.addAll(newObjects);
      --          newObjects.clear();
      --          newObjects.trimToSize();
   end Update_After_Prepare_Append;

   procedure Prepare_Append
     (This      : access Base_Pool_T'Class;
      Chunk_Map : Skill.Field_Declarations.Chunk_Map)
   is

      New_Instances : constant Boolean :=
        null /= This.Dynamic.First_Dynamic_New_Instance;
   begin

      -- check if we have to append at all
      if not New_Instances
        and then not This.Blocks.Is_Empty
        and then not This.Data_Fields.Is_Empty
      then
         declare
            Done : Boolean := True;

            procedure Check (F : Field_Declarations.Field_Declaration) is
            begin
               if F.Data_Chunks.Is_Empty then
                  Done := False;
               end if;
            end Check;
         begin
            This.Data_Fields_F.Foreach (Check'Access);

            if Done then
               return;
            end if;
         end;
      end if;

      if New_Instances then
         -- we have to resize
         declare

            Count : Natural          := This.Size;
            D     : Annotation_Array :=
              new Annotation_Array_T (This.Data'First .. Count);

            procedure Free is new Ada.Unchecked_Deallocation
              (Object => Annotation_Array_T,
               Name   => Annotation_Array);

            I : Natural := This.Data'Last + 1;
            procedure Mark (Inst : Annotation) is
            begin
               D (I)         := Inst;
               Inst.Skill_ID := I;
               I             := I + 1;
            end Mark;

         begin
            D (This.Data'First .. This.Data'Last) := This.Data.all;
            This.Foreach_Dynamic_New_Instance (Mark'Access);

            if This.Data /= Empty_Data then
               Free (This.Data);
            end if;
            This.Data := D;
         end;
      end if;

      Update_After_Prepare_Append (This, Chunk_Map);
   end Prepare_Append;

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
