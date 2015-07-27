--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     !! remove after integration into generator !!       --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Files;
with Skill.Internal.File_Parsers;
with Skill.Types.Pools;
with Skill.Types;
with Skill.Types.Vectors;

with Age;

-- instantiated pool packages
-- GNAT Bug workaround; should be "new Base(...)" instead
package Skill.Types.Pools.Age_Pools is

   package Age_P is

      type Pool_T is new Base_Pool_T with private;
      type Pool is access Pool_T;

      -- API methods
      function Get (This : access Pool_T; ID : Skill_ID_T) return Age.age;

      ----------------------
      -- internal methods --
      ----------------------

      -- constructor invoked by new_pool
      function Make (Type_Id : Natural) return Pools.Pool;
      -- destructor invoked by close
      procedure Free (This : access Pool_T);

      overriding function Insert_Instance
        (This : access Pool_T;
         ID   : Skill_ID_T) return Boolean;

      overriding function Static_Size (This : access Pool_T) return Natural;

      -- applies F for each element in this
--        procedure Foreach
--          (This : access Pool_T;
--           F    : access procedure (I : Age));

   private

      package A1 is new Vectors (Natural, Age.Age);
      subtype Instance_Vector is A1.Vector;

      type Pool_T is new Base_Pool_T with record
         Static_Data : Instance_Vector;
         New_Objects : Instance_Vector;
      end record;
   end Age_P;

end Skill.Types.Pools.Age_Pools;
