--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     field handling in skill                             --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Field_Types;
with Skill.Internal.Parts;
with Skill.Streams.Reader;
with Ada.Containers.Doubly_Linked_Lists;

package Skill.Field_Declarations is
   pragma Preelaborate;

   type Field_Declaration_T is abstract tagged private;
   type Field_Declaration is access Field_Declaration_T'Class;

   type Field_Array is array (Integer range <>) of Field_Declaration;
   type Field_Array_Access is not null access Field_Array;
   Empty_Field_Array : Field_Array_Access := new Field_Array (1 .. 0);

   type Auto_Field_T is abstract new Field_Declaration_T with private;
   type Auto_Field is access Auto_Field_T'Class;

   type Chunk_Entry is private;

   procedure Add_Chunk
     (This : access Field_Declaration_T;
      C    : Skill.Internal.Parts.Chunk);

private

   --Data chunk information, as it is required for parsing of field data
   type Chunk_Entry is record
      C     : Skill.Internal.Parts.Chunk;
      Input : Skill.Streams.Reader.Sub_Stream;
   end record;
   package Chunk_List_P is new Ada.Containers.Doubly_Linked_Lists
     (Chunk_Entry);

   type Field_Declaration_T is tagged record
      Data_Chunks : Chunk_List_P.List := Chunk_List_P.Empty_List;
   end record;

   type Auto_Field_T is new Field_Declaration_T with record
      null;
   end record;
   type Auto_Field_Array is array (Integer range <>) of Auto_Field;

end Skill.Field_Declarations;
