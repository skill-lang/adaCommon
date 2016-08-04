--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     field handling in skill                             --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

with Skill.Field_Types;
with Skill.Field_Restrictions;
with Skill.Internal.Parts;
with Skill.Streams.Reader;
with Skill.Streams.Writer;
with Ada.Containers.Doubly_Linked_Lists;
with Skill.Containers.Vectors;
limited with Skill.Types.Pools;
with Skill.Types;
with Ada.Containers.Hashed_Maps;
with Ada.Unchecked_Conversion;
with Ada.Containers.Vectors;

package Skill.Field_Declarations is
--     pragma Preelaborate;

   --Data chunk information, as it is required for parsing of field data
   type Chunk_Entry_T is record
      C     : Skill.Internal.Parts.Chunk;
      Input : Skill.Streams.Reader.Sub_Stream;
   end record;
   type Chunk_Entry is access Chunk_Entry_T;
   package Chunk_List_P is new Skill.Containers.Vectors (Natural, Chunk_Entry);

   type Owner_T is not null access Skill.Types.Pools.Pool_T;

   type Field_Declaration_T is abstract tagged record
      Data_Chunks : Chunk_List_P.Vector := Chunk_List_P.Empty_Vector;
      T           : Skill.Field_Types.Field_Type;
      Name        : Types.String_Access;
      Index       : Natural;
      Owner       : Owner_T;

      -- runtime restrictions
      Restrictions : Field_Restrictions.Vector;

   -- used for offset calculation
   -- note: ada has no futures, thus we will store the value in the field and
   -- synchronize over a barrier
      Future_Offset : Types.v64;
   end record;
   -- can not be not null, because we need to store them in arrays :-/
   type Field_Declaration is access all Field_Declaration_T'Class;
   function Hash (This : Field_Declaration) return Ada.Containers.Hash_Type;

   use type Internal.Parts.Chunk;
   package Chunk_Map_P is new Ada.Containers.Hashed_Maps(Key_Type        => Field_Declaration,
                                                         Element_Type    => Internal.Parts.Chunk,
                                                         Hash            => Hash,
                                                         Equivalent_Keys => "=",
                                                         "="             => "=");
   type Chunk_Map is not null access Chunk_Map_P.Map;

   package Field_Vector_P is new Skill.Containers.Vectors
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

   -- @return ∀ r ∈ restrictinos, i ∈ owner. r.check(i)
   function Check (This : access Field_Declaration_T) return Boolean;
   function Check (This : access Lazy_Field_T) return Boolean;
   function Check (This : access Auto_Field_T) return Boolean is (True);


   procedure Read
     (This : access Field_Declaration_T;
      CE   : Chunk_Entry) is abstract;

   procedure Read (This : access Lazy_Field_T; CE : Chunk_Entry);
   procedure Read (This : access Auto_Field_T; CE : Chunk_Entry) is null;

   -- offset calculation as preparation of writing data belonging to the
   -- owners last block
   procedure Offset (This : access Field_Declaration_T) is abstract;
   procedure Offset (This : access Lazy_Field_T);
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
      Output : Streams.Writer.Sub_Stream);
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
      Name  : Skill.Types.String_Access;
      Restrictions : Field_Restrictions.Vector) return Lazy_Field;

   procedure Ensure_Is_Loaded (This : access Lazy_Field_T);

   procedure Free (This : access Field_Declaration_T) is abstract;
   procedure Free (This : access Lazy_Field_T);
   procedure Free (This : access Auto_Field_T) is null;

private
   pragma Warnings (Off);
   function Hash is new Ada.Unchecked_Conversion(Internal.Parts.Chunk,
                                                 Ada.Containers.Hash_Type);
   use type Streams.Reader.Sub_Stream;
   package Part_P is new Ada.Containers.Vectors(Natural, Chunk_Entry);

   function Hash is new Ada.Unchecked_Conversion(Types.Annotation,
                                                 Ada.Containers.Hash_Type);
   use type Types.Box;
   use type Types.Annotation;
   package Data_P is new Ada.Containers.Hashed_Maps(Types.Annotation, Types.Box,
                                                   Hash, "=", "=");

   type Lazy_Field_T is new Field_Declaration_T with record
      -- data held as in storage pools
      -- @note see paper notes for O(1) implementation
      -- @note in contrast to all other implementations, we deliberately wast
      -- time and space, because it is the only implementation that will make it
      -- through the compiler without me jumping out of the window trying to
      -- find an efficient workaround
      Data : Data_P.Map; -- Sparse Array[T]()

      -- pending parts that have to be loaded
      Parts : Part_P.Vector;
   end record;

   function Is_Loaded (This : access Lazy_Field_T'Class) return Boolean is
     (This.Parts.Is_Empty);

   procedure Load (This : access Lazy_Field_T'Class);

   type Auto_Field_T is new Field_Declaration_T with record
      null;
   end record;
   type Auto_Field_Array is array (Integer range <>) of Auto_Field;

end Skill.Field_Declarations;
