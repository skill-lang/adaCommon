--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     field handling in skill                             --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Field_Types;
with Skill.Internal.Parts;
with Skill.Streams.Reader;
with Skill.Streams.Writer;
with Ada.Containers.Doubly_Linked_Lists;
with Skill.Types.Vectors;
limited with Skill.Types.Pools;
with Skill.Types;

package Skill.Field_Declarations is
--     pragma Preelaborate;

   --Data chunk information, as it is required for parsing of field data
   type Chunk_Entry_T is record
      C     : Skill.Internal.Parts.Chunk;
      Input : Skill.Streams.Reader.Sub_Stream;
   end record;
   type Chunk_Entry is access Chunk_Entry_T;
   package Chunk_List_P is new Skill.Types.Vectors (Natural, Chunk_Entry);

   type Owner_T is not null access Skill.Types.Pools.Pool_T;

   type Field_Declaration_T is abstract tagged record
      Data_Chunks : Chunk_List_P.Vector := Chunk_List_P.Empty_Vector;
      T           : Skill.Field_Types.Field_Type;
      Name        : Types.String_Access;
      Index       : Natural;
      Owner       : Owner_T;

   -- used for offset calculation
   -- note: ada has no futures, thus we will store the value in the field and
   -- synchronize over a barrier
      Future_Offset : Types.v64;
   end record;
   -- can not be not null, because we need to store them in arrays :-/
   type Field_Declaration is access Field_Declaration_T'Class;

   package Field_Vector_P is new Skill.Types.Vectors
     (Positive,
      Field_Declaration);
   subtype Field_Vector is Field_Vector_P.Vector;

   type Lazy_Field_T is new Field_Declaration_T with private;
   type Lazy_Field is access Lazy_Field_T'Class;

   type Auto_Field_T is abstract new Field_Declaration_T with private;
   type Auto_Field is access Auto_Field_T'Class;

   procedure Delete_Chunk (This : Chunk_Entry);

   function Name
     (This : access Field_Declaration_T'Class) return Types.String_Access;

   function Owner
     (This : access Field_Declaration_T'Class) return Types.Pools.Pool;

   procedure Read
     (This : access Field_Declaration_T;
      CE   : Chunk_Entry) is abstract;
   procedure Read (This : access Lazy_Field_T; CE : Chunk_Entry) is null;
   procedure Read (This : access Auto_Field_T; CE : Chunk_Entry) is null;

   -- offset calculation as preparation of writing data belonging to the
   -- owners last block
   procedure Offset (This : access Field_Declaration_T) is abstract;
   procedure Offset (This : access Lazy_Field_T) is null;
   procedure Offset (This : access Auto_Field_T) is null;

   -- write data into a map at the end of a write/append operation
   -- @note this will always write the last chunk, as, in contrast to read, it is impossible to write to fields in
   --       parallel
   -- @note only called, if there actually is field data to be written
   procedure Write
     (This   : access Field_Declaration_T;
      Output : Streams.Writer.Sub_Stream) is abstract;
   procedure Write
     (This   : access Lazy_Field_T;
      Output : Streams.Writer.Sub_Stream) is null;
   procedure Write
     (This   : access Auto_Field_T;
      Output : Streams.Writer.Sub_Stream) is null;

   -- internal use only
   function Field_ID (This : access Field_Declaration_T'Class) return Natural;

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


   procedure Free (This : access Field_Declaration_T) is abstract;
   procedure Free (This : access Lazy_Field_T);
   procedure Free (This : access Auto_Field_T) is null;

private

   type Lazy_Field_T is new Field_Declaration_T with record
      null;
   end record;

   type Auto_Field_T is new Field_Declaration_T with record
      null;
   end record;
   type Auto_Field_Array is array (Integer range <>) of Auto_Field;

end Skill.Field_Declarations;
