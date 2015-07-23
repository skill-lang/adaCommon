--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     !! remove after integration into generator !!       --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Types.Pools;
with Skill.Files;
with Skill.Streams;
with Skill.Errors;
with Skill.Internal.File_Parsers;
with Skill.String_Pools;
with Skill.Equals;
with Skill.Field_Types;
with Skill.Internal.Parts;
with Ada.Unchecked_Conversion;
with Skill.Types;
with Age.Api;
with Age.Internal_Skill_Names;

-- instantiated pool packages
-- GNAT Bug workaround; should be "new Base(...)" instead
package body Skill.Types.Pools.Age_Pools is

   package body Age_P is

      use Skill.Types;

      -- constructor invoked by new_pool
      function Make (Type_Id : Natural) return Skill.Types.Pools.Pool is
         This : Pool :=
                  new Pool_T'
                    (Name        => Age.Internal_Skill_Names.Age_Skill_Name,
                     Type_Id     => Type_Id,
                     Super       => null,
                     Base        => null,
                     Sub_Pools   => Skill.Types.Pools.Sub_Pool_Vector.Empty_Vector,
                     Data_Fields => Skill.Field_Declarations.Empty_Field_Array,
                     Blocks      => new Skill.Internal.Parts.Blocks_P.Vector,
                     Fixed       => False,
                     Cached_Size => 0,
                     Data        => Skill.Types.Pools.Empty_Data,
                     Owner       => null,
                     Static_Data => new A1.Vector);
         function Convert is new Ada.Unchecked_Conversion
           (Source => Pool,
            Target => Skill.Types.Pools.Base_Pool);
         function Convert is new Ada.Unchecked_Conversion
           (Source => Pool,
            Target => Skill.Types.Pools.Pool);
      begin
         This.Base := Convert (This);
         return Convert (This);
      end Make;

      overriding function Insert_Instance
        (This : access Pool_T;
         ID   : Skill.Types.Skill_ID_T)
      return Boolean
      is
         function Convert is new Ada.Unchecked_Conversion (Source => Age.Age,
                                                           Target => Skill.Types.Annotation);

         I : Natural := Natural (Id) - 1;
         R : Age.Age;
      begin
         if null /= This.Data (I) then
            return false;
         end if;

         R := new Age.Age_T;
         R.Skill_ID := Id;
         This.Data (I) := Convert (R);
         This.Static_Data.Append (R);
         return true;
      end Insert_Instance;

   end Age_P;

end Skill.Types.Pools.Age_Pools;
