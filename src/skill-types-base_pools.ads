--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     type handling in skill                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
with Skill.Types.Pools;

generic
   -- type of the (generated) skill state
   type Skill_State is private;

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
package Skill.Types.Base_Pools is

   package Ps is new Skill.Types.Pools
     (T,
      Type_Id,
      Name,
      Known_Fields,
      Auto_Fields);

   type Base_Pool_Access is private;

   type T_Access is access T;
   type A1 is array (Natural range <>) of T_Access;
   type T_Array is not null access A1;

   type Owner_T is not null access Skill_State;

private

   type Base_Pool is new Ps.Pool with record
      Data  : T_Array;
      Owner : Owner_T;
   end record;
   type Base_Pool_Access is access Base_Pool;

end Skill.Types.Base_Pools;
