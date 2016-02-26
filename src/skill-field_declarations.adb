--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     field handling in skill                             --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Errors;
with Skill.Field_Types;
with Skill.Internal.Parts;
with Skill.Iterators.Type_Order;
with Interfaces;
with Skill.Types.Pools;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Skill.Field_Declarations is

   procedure Delete_Chunk (This : Chunk_Entry) is
      procedure Delete is new Ada.Unchecked_Deallocation
        (Chunk_Entry_T,
         Chunk_Entry);
      D : Chunk_Entry := This;
   begin
      This.C.Free;
      This.Input.Free;
      Delete (D);
   end Delete_Chunk;

   function Name
     (This : access Field_Declaration_T'Class) return Types.String_Access is
     (This.Name);

   function Owner
     (This : access Field_Declaration_T'Class) return Skill.Types.Pools.Pool
   is
      function Convert is new Ada.Unchecked_Conversion
        (Owner_T,
         Skill.Types.Pools.Pool);
   begin
      return Convert (This.Owner);
   end Owner;

   function Check (This : access Field_Declaration_T) return Boolean is
      Iter : aliased Skill.Iterators.Type_Order.Iterator;
      RC   : Skill.Field_Restrictions.Checkable;
   begin
      if This.Restrictions.Is_Empty then
         return True;
      end if;
      for R of This.Restrictions loop
         if R.all in Skill.Field_Restrictions.Checkable_T'Class then
            RC := Skill.Field_Restrictions.Checkable (R);
            Iter.Init (This.Owner.To_Pool);
            while Iter.Has_Next loop
               if not RC.Check
                 (Iter.Next.Reflective_Get
                  (Skill.Field_Declarations.Field_Declaration (This)))
               then
                  return False;
               end if;
            end loop;
         end if;
      end loop;
      return True;
   end Check;

   function Check (This : access Lazy_Field_T) return Boolean is
      Iter : aliased Skill.Iterators.Type_Order.Iterator;
      RC   : Skill.Field_Restrictions.Checkable;
   begin
      if This.Restrictions.Is_Empty then
         return True;
      end if;
      This.Ensure_Is_Loaded;
      for R of This.Restrictions loop
         if R.all in Skill.Field_Restrictions.Checkable_T'Class then
            RC := Skill.Field_Restrictions.Checkable (R);
            Iter.Init (This.Owner.To_Pool);
            while Iter.Has_Next loop
               if not RC.Check (This.Data.Element (Iter.Next)) then
                  return False;
               end if;
            end loop;
         end if;
      end loop;
      return True;
   end Check;

   function Hash
     (This : Field_Declaration) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type
        (37 * This.Index + 31337 * This.Owner.Pool_Offset));

   function Field_ID
     (This : access Field_Declaration_T'Class) return Natural is
     (This.Index);

   procedure Add_Chunk
     (This : access Field_Declaration_T'Class;
      C    : Skill.Internal.Parts.Chunk)
   is
   begin
      This.Data_Chunks.Append (new Chunk_Entry_T'(C, null));
   end Add_Chunk;

   function Add_Offset_To_Last_Chunk
     (This        : access Field_Declaration_T'Class;
      Input       : Skill.Streams.Reader.Input_Stream;
      File_Offset : Types.v64) return Types.v64
   is
      CE : Chunk_Entry                := This.Data_Chunks.Last_Element;
      C  : Skill.Internal.Parts.Chunk := CE.C;

      use type Interfaces.Integer_64;
   begin
      C.First  := C.First + File_Offset;
      C.Last   := C.Last + File_Offset;
      CE.Input := Input.Map (0, C.First, C.Last);

      return C.Last;
   end Add_Offset_To_Last_Chunk;

   function Make_Lazy_Field
     (Owner        : Owner_T;
      ID           : Natural;
      T            : Field_Types.Field_Type;
      Name         : Skill.Types.String_Access;
      Restrictions : Field_Restrictions.Vector) return Lazy_Field
   is
   begin
      return new Lazy_Field_T'
          (Data_Chunks   => Chunk_List_P.Empty_Vector,
           T             => T,
           Name          => Name,
           Index         => ID,
           Owner         => Owner,
           Future_Offset => 0,
           Restrictions  => Restrictions,
           Data          => Data_P.Empty_Map,
           Parts         => Part_P.Empty_Vector);
   end Make_Lazy_Field;

   procedure Ensure_Is_Loaded (This : access Lazy_Field_T) is
   begin
      if not This.Is_Loaded then
         This.Load;
      end if;
   end Ensure_Is_Loaded;

   procedure Read (This : access Lazy_Field_T; CE : Chunk_Entry) is
   begin
      This.Parts.Append (CE);
   end Read;

   procedure Offset (This : access Lazy_Field_T) is
      use type Skill.Types.v64;
      use type Skill.Types.Uv64;

      Rang : constant Skill.Internal.Parts.Block :=
        This.Owner.Blocks.Last_Element;
      Data   : constant Skill.Types.Annotation_Array := This.Owner.Base.Data;
      Result : Skill.Types.v64                       := 0;
      Low    : constant Natural                      := Natural (Rang.BPO);
      High   : constant Natural := Natural (Rang.BPO + Rang.Dynamic_Count);
   begin
      This.Ensure_Is_Loaded;
      for I in Low + 1 .. High loop
         Result := Result + This.T.Offset_Box (This.Data.Element (Data (I)));
      end loop;
      This.Future_Offset := Result;
   end Offset;

   procedure Write
     (This   : access Lazy_Field_T;
      Output : Streams.Writer.Sub_Stream)
   is
      Rang : constant Skill.Internal.Parts.Block :=
        This.Owner.Blocks.Last_Element;
      Data : constant Skill.Types.Annotation_Array := This.Owner.Base.Data;
      Low  : constant Natural                      := Natural (Rang.BPO);
      High : constant Natural := Natural (Rang.BPO + Rang.Dynamic_Count);
   begin
      for I in Low + 1 .. High loop
         This.T.Write_Box (Output, This.Data (Data (I)));
      end loop;
   end Write;

   procedure Free (This : access Lazy_Field_T) is
      type T is access all Lazy_Field_T;
      procedure Delete is new Ada.Unchecked_Deallocation (Lazy_Field_T, T);
      D : T := T (This);
   begin
      This.Data_Chunks.Foreach (Delete_Chunk'Access);
      This.Data_Chunks.Free;
      Delete (D);
   end Free;

   procedure Load (This : access Lazy_Field_T'Class) is
      D : Types.Annotation_Array := This.Owner.Base.Data;
      B : Internal.Parts.Block;
   begin
      for Ce of This.Parts loop
         if Ce.C.all in Skill.Internal.Parts.Simple_Chunk then
            for I in
              Skill.Internal.Parts.Simple_Chunk_X (Ce.C).BPO + 1 ..
                  Skill.Internal.Parts.Simple_Chunk_X (Ce.C).BPO + Ce.C.Count
            loop
               This.Data.Include (D (I), This.T.Read_Box (Ce.Input));
            end loop;
         else
            --case bci : BulkChunk ⇒
            for I in
              0 .. Skill.Internal.Parts.Bulk_Chunk_X (Ce.C).Block_Count - 1
            loop
               B := This.Owner.Blocks.Element (I);
               for I in B.BPO + 1 .. B.BPO + B.Dynamic_Count loop
                  This.Data.Include (D (I), This.T.Read_Box (Ce.Input));
               end loop;
            end loop;
         end if;
      end loop;
      This.Parts.Clear;
   exception
      when E : others =>
         raise Skill.Errors.Skill_Error with "failed to parse lazy field";
   end Load;

end Skill.Field_Declarations;
