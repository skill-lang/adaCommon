--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     type handling in skill                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Field_Types;
with Skill.Types.Vectors;

-- parametrization for known types used for the creation of a usable facade
generic

   -- type of values stored in a pool
   type T is new Skill_Object with private;
   type P is access T;

   -- the pools name
   Name : not null String_Access;

   -- the fields known to the pool (set of strings)
--   Known_Fields : String_Access_Array;

   -- auto fields are a generic parameter as well
   -- range is allways -XX to 0.
--   Auto_Fields : Skill.Field_Types.Auto_Field_Array;
package Skill.Types.Pools.Base is
   pragma Preelaborate;

   type Pool_T is new Base_Pool_T with private;
   type Pool is access Pool_T;

   -- constructor invoked by new_pool
   function Make (Type_Id : Natural) return Pools.Pool;

   overriding function Insert_Instance
     (This : access Pool_T;
      ID   : Skill_ID_T) return Boolean;

private

   package A1 is new Vectors (Index_Type   => Natural,
                              Element_Type => P);
   type Static_Data_T is not null access A1.Vector;
   type Pool_T is new Base_Pool_T with record
      Static_Data : Static_data_t;
   end record;

end Skill.Types.Pools.Base;
