--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     type handling in skill                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;

with Skill.Field_Types;
with Skill.Field_Declarations;
with Skill.Internal.Parts;
limited with Skill.Files;
with Skill.Types.Vectors;

-- TODO push down:
--  type A2 is not null access T;
--  package New_Objects_T is new Ada.Containers.Vectors (Natural, A2);
--
--  -- objects that have not yet been written to disk
--  New_Objects : New_Objects_T.Vector;
--        generic
--     -- type of values stored in a pool
--        type T is tagged private;
--
--
--
--        -- the fields known to the pool (set of strings)
--        Known_Fields : String_Access_Array;
--
--        -- auto fields are a generic parameter as well
--        -- range is allways -XX to 0.
--           Auto_Fields : Skill.Field_Types.Auto_Field_Array;
--     package Sub_Pool
--     is
--        type Pool_T is new T with null record;
--        end Sub_Pool;

-- in contrast to a solution in c++ or java, we will represent data and most of
-- the internal implementation in a type erasure version of the java
-- implementation. The Facade will use the generic type system to create methods
-- of the right types without producing significant excess code
package Skill.Types.Pools is

--     pragma Preelaborate;

   -- abstract pool types
   type Pool_T is abstract new Field_Types.Field_Type_Base with private;
   type Pool is access Pool_T;
   type Pool_Dyn is access Pool_T'Class;
   type Sub_Pool_T is abstract new Pool_T with private;
   type Sub_Pool is access Sub_Pool_T;
   type Base_Pool_T is abstract new Pool_T with private;
   type Base_Pool is access Base_Pool_T;

   -- data structures using pools
   package Sub_Pool_Vector_P is new Types.Vectors (Natural, Sub_Pool);
   subtype Sub_Pool_Vector is Sub_Pool_Vector_P.Vector;

   -- pointer conversions
   function Dynamic (This : access Pool_T) return Pool_Dyn;

   -- pool properties

   function To_String (This : Pool_T) return String;

   function Skill_Name (This : access Pool_T) return String_Access;

   function ID (This : access Pool_T) return Natural;

   function Base (This : access Pool_T) return Base_Pool;

   function Super (This : access Pool_T) return Pool;

   function Size (This : access Pool_T'Class) return Natural;

   -- the number of instances of exactly this type, excluding sub-types
   -- @return size excluding subtypes
   function Static_Size (This : access Pool_T) return Natural is abstract;
   function Static_Size (This : access Base_Pool_T) return Natural is abstract;
   function Static_Size (This : access Sub_Pool_T) return Natural is abstract;

   -- internal use only
   function Blocks (This : access Pool_T) return Skill.Internal.Parts.Blocks;

   -- internal use only
   function Data_Fields
     (This : access Pool_T) return Skill.Field_Declarations.Field_Vector;

   -- internal use only
   function Add_Field
     (This : access Pool_T;
      ID   : Natural;
      T    : Field_Types.Field_Type;
      Name : String_Access) return Skill.Field_Declarations.Field_Declaration;
   function Add_Field
     (This : access Base_Pool_T;
      ID   : Natural;
      T    : Field_Types.Field_Type;
      Name : String_Access) return Skill.Field_Declarations.Field_Declaration;

   -- internal use only
   function Insert_Instance
     (This : access Pool_T;
      ID   : Skill_ID_T) return Boolean is abstract;
   function Insert_Instance
     (This : access Sub_Pool_T;
      ID   : Skill_ID_T) return Boolean is abstract;
   function Insert_Instance
     (This : access Base_Pool_T;
      ID   : Skill_ID_T) return Boolean is abstract;

   -- internal use only
   procedure Free (This : access Pool_T) is abstract;
   procedure Free (This : access Sub_Pool_T) is abstract;
   procedure Free (This : access Base_Pool_T) is abstract;

   -- internal use only
   function Data
     (This : access Base_Pool_T) return Skill.Types.Annotation_Array;
   -- internal use only
   procedure Resize_Data (This : access Base_Pool_T);

private

   type Pool_T is abstract new Field_Types.Field_Type_Base with record

      -- the pools name
      Name : not null String_Access;

      -- the pools type id
      Type_Id : Natural;

      -- representation of the immediate super type (null if none exists)
      Super : Pool;

      -- representation of the base type (nonnull, maybe a self-reference)
      Base : Base_Pool;

      -- a list of sub-pools, mostly used to simplify some algorithms
      Sub_Pools : Sub_Pool_Vector;

      -- the list of all data fields
      Data_Fields_F : Skill.Field_Declarations.Field_Vector;

      -- layout of skill ids of this type
      Blocks : Skill.Internal.Parts.Blocks;

      -- Storage pools can be fixed, i.e. no dynamic instances can be added
      -- to the pool. Fixing a pool requires that it does not contain a new
      -- object. Fixing a pool will fix subpools as well. Un-fixing a pool
      -- will un-fix super pools as well, thus being fixed is a transitive
      -- property over the sub pool relation. Pools will be fixed by flush
      -- operations.
      Fixed       : Boolean := False;
      Cached_Size : Natural;
   end record;

   type Owner_T is access Skill.Files.File_T;

   -- note it is important to have the empty array start at 1, because that way
   -- all descendent arrays will start at 1 as well and thus, no manual index
   -- adjustment is necessary
   Empty_Data : Skill.Types.Annotation_Array :=
     new Skill.Types.Annotation_Array_T (1 .. 0);
   type Base_Pool_T is abstract new Pool_T with record
      Data  : Skill.Types.Annotation_Array;
      Owner : Owner_T;
   end record;

   type Sub_Pool_T is abstract new Pool_T with record
      null;
   end record;

end Skill.Types.Pools;
