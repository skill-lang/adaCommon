--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     type handling in skill                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;



generic
   -- type of values stored in a pool
   type T is tagged private;

   -- the pools type id
   Type_Id : Natural;

   -- the pools name
   Name : not null String_Access;

   -- the fields known to the pool (set of strings)
   Known_Fields : String_Access_Array;

   -- auto fields are a generic parameter as well
   -- range is allways -XX to 0.
   Auto_Fields : Auto_Field_Array;
package Skill.Types.Pools is

   package Fts is new Field_Types (T, Type_Id);

   type Pool is tagged private;
   type Pool_Access is access Pool;
   subtype NPool_Access is not null Pool_Access;
   package Pool_Vector is new Ada.Containers.Vectors(Natural, NPool_Access);

private

   type Pool is new Fts.Field_Type with record

      -- representation of the immediate super type (null if none exists)
      Super : Pool_Access;

      -- representation of the base type (nonnull, maybe a self-reference)
      Base : not null Pool_Access;

      -- a list of sub-pools, mostly used to simplify some algorithms
      Sub_Pools : Pool_Vector.Vector;

      -- the list of all data fields
      Data_Fields : Field_Array_Access;

      -- layout of skill ids of this type
      Blocks : Any_Ref;

      -- objects that have not yet been written to disk
      New_Objects : Any_Ref;

      -- Storage pools can be fixed, i.e. no dynamic instances can be added
      -- to the pool. Fixing a pool requires that it does not contain a new
      -- object. Fixing a pool will fix subpools as well. Un-fixing a pool
      -- will un-fix super pools as well, thus being fixed is a transitive
      -- property over the sub pool relation. Pools will be fixed by flush
      -- operations.
      Fixed       : Boolean := False;
      Cached_Size : Natural;
   end record;

end Skill.Types.Pools;
