--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     field handling in skill                             --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Field_Types;
with Skill.Internal.Parts;
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
     (Owner : Owner_T;
      ID    : Natural;
      T     : Field_Types.Field_Type;
      Name  : Skill.Types.String_Access) return Lazy_Field
   is
   begin
      return new Lazy_Field_T'
          (Data_Chunks   => Chunk_List_P.Empty_Vector,
           T             => T,
           Name          => Name,
           Index         => ID,
           Owner         => Owner,
           Future_Offset => 0);
   end Make_Lazy_Field;

   procedure Free (This : access Lazy_Field_T) is
      type T is access all Lazy_Field_T;
      procedure Delete is new Ada.Unchecked_Deallocation (Lazy_Field_T, T);
      D : T := T (This);
   begin
      This.Data_Chunks.Foreach (Delete_Chunk'Access);
      This.Data_Chunks.Free;
      Delete (D);
   end Free;

end Skill.Field_Declarations;
