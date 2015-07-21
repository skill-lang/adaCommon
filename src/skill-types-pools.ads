--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     type handling in skill                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;

with Skill.Field_Types;
with Skill.Internal.Parts;
limited with Skill.Files;


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

   pragma Preelaborate;

   -- abstract pool types
   type Pool_T is abstract tagged private;
   type Pool is access Pool_T;
   type Sub_Pool_T is abstract tagged private;
   type Sub_Pool is access Sub_Pool_T;
   type Base_Pool_T is abstract tagged private;
   type Base_Pool is access Base_Pool_T;

   -- data structures using pools
   package Sub_Pool_Vector is new Ada.Containers.Vectors (Natural, Sub_Pool);


   -- pool properties

   function To_String (This : Pool_T) return String;

   function Skill_Name (This : access Pool_T) return String_Access;

   function ID (This : access Pool_T) return Natural;

   function Base (This : access Pool_T) return Base_Pool;

   function Super (This : access Pool_T) return Pool;




   -- internal use only
   function Data (This : access Base_Pool_T) return Skill.Types.Annotation_Array;



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
      Sub_Pools : Sub_Pool_Vector.Vector;

      -- the list of all data fields
      Data_Fields : Skill.Field_Types.Field_Array_Access;

      -- layout of skill ids of this type
      Blocks : Skill.Internal.Parts.Blocks.Vector;


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

   type Base_Pool_T is abstract new Pool_T with record
      Data  : Skill.Types.Annotation_Array;
      Owner : Owner_T;
   end record;

   type Sub_Pool_T is abstract new Pool_T with record
      null;
   end record;


end Skill.Types.Pools;
