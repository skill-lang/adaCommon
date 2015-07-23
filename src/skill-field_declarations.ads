--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     field handling in skill                             --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Field_Types;
with Skill.Internal.Parts;
with Skill.Streams.Reader;
with Ada.Containers.Doubly_Linked_Lists;
with Skill.Types.Vectors;
limited with Skill.Types.Pools;
with Skill.Types;

package Skill.Field_Declarations is
   pragma Preelaborate;

   type Field_Declaration_T is abstract tagged private;
   type Field_Declaration is access Field_Declaration_T'Class;

   package Field_Array_P is new Skill.Types.Vectors (Field_Declaration);
   subtype Field_Array is Field_Array_P.Vector;
   type Field_Array_Access is not null access Field_Array;
   Empty_Field_Array : Field_Array_Access := new Field_Array;

   type Lazy_Field_T is new Field_Declaration_T with private;
   type Lazy_Field is access Lazy_Field_T'Class;

   type Auto_Field_T is abstract new Field_Declaration_T with private;
   type Auto_Field is access Auto_Field_T'Class;

   type Chunk_Entry is private;

   type Owner_T is not null access Skill.Types.Pools.Pool_T;

   -- internal use only
   procedure Add_Chunk
     (This : access Field_Declaration_T'Class;
      C    : Skill.Internal.Parts.Chunk);

   -- internal use only
   -- Fix offset and create memory map for field data parsing.
   function Add_Offset_To_Last_Chunk
     (This        : access Field_Declaration_T'Class;
      Input       : Skill.Streams.Reader.Input_Stream;
      File_Offset : Types.v64) return Types.v64;

   -- internal use only
   function Make_Lazy_Field
     (Owner : Owner_T;
      ID    : Natural;
      T     : Field_Types.Field_Type;
      Name  : Skill.Types.String_Access) return Lazy_Field;

private

   --Data chunk information, as it is required for parsing of field data
   type Chunk_Entry_T is record
      C     : Skill.Internal.Parts.Chunk;
      Input : Skill.Streams.Reader.Sub_Stream;
   end record;
   type Chunk_Entry is access Chunk_Entry_T;
   package Chunk_List_P is new Ada.Containers.Doubly_Linked_Lists
     (Chunk_Entry);

   type Field_Declaration_T is tagged record
      Data_Chunks : Chunk_List_P.List := Chunk_List_P.Empty_List;
      T           : Skill.Field_Types.Field_Type;
      Name        : Types.String_Access;
      Index       : Natural;
      Owner       : Owner_T;
   end record;

   type Lazy_Field_T is new Field_Declaration_T with record
      null;
   end record;

   type Auto_Field_T is new Field_Declaration_T with record
      null;
   end record;
   type Auto_Field_Array is array (Integer range <>) of Auto_Field;

end Skill.Field_Declarations;
