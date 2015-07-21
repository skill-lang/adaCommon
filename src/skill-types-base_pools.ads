--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     type handling in skill                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Field_Types;
limited with Skill.Types.Pools;

-- we finally found that the only viable solution is to take the unchecked
-- conversion from pool to base/sub pool whenever applicable
generic
   -- type of the (generated) skill state
   type Super is tagged private;

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
   Auto_Fields : Skill.Field_Types.Auto_Field_Array;
package Skill.Types.Base_Pools is
   pragma Preelaborate;

--     package Ps is new Skill.Types.Pools
--       (T,
--        Type_Id,
--        Name,
--        Known_Fields,
--        Auto_Fields);

   type Pool_T is tagged private;
   type Pool is access Pool_T;


   type T_Access is access T;
   type A1 is array (Natural range <>) of T_Access;
   type T_Array is not null access A1;


private

   type Pool_T is new Super with record
null;
   end record;

end Skill.Types.Base_Pools;
