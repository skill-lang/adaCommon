--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     !! remove after integration into generator !!       --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Files;
with Skill.Internal.File_Parsers;
with Skill.Types.Pools;
with Skill.Types.Pools.Base;
with Skill.Types;
with Skill.Types.Vectors;

with Age;

-- instantiated pool packages
-- GNAT Bug workaround; should be "new Base(...)" instead
package  Skill.Types.Pools.Age_Pools is

   package Age_P is

      type Pool_T is new Base_Pool_T with private;
      type Pool is access Pool_T;

      -- constructor invoked by new_pool
      function Make (Type_Id : Natural) return Pools.Pool;

      overriding function Insert_Instance
        (This : access Pool_T;
         ID   : Skill_ID_T) return Boolean;

   private

      package A1 is new Vectors (Index_Type   => Natural,
                                 Element_Type => Age.Age);
      type Static_Data_T is not null access A1.Vector;
      type Pool_T is new Base_Pool_T with record
         Static_Data : Static_data_t;
      end record;
   end Age_P;

end Skill.Types.Pools.Age_Pools;