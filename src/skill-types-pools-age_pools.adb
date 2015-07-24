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
         function Convert is new Ada.Unchecked_Conversion
           (Source => Pool,
            Target => Skill.Types.Pools.Base_Pool);
         function Convert is new Ada.Unchecked_Conversion
           (Source => Pool,
            Target => Skill.Types.Pools.Pool);

         This : Pool;
      begin
         This := new Pool_T'
           (Name          => Age.Internal_Skill_Names.Age_Skill_Name,
            Type_Id       => Type_Id,
            Super         => null,
            Base          => null,
            Sub_Pools     => Sub_Pool_Vector_P.Empty_Vector,
            Data_Fields_F => Skill.Field_Declarations.Field_Vector_P.Empty_Vector,
            Blocks        => Skill.Internal.Parts.Blocks_P.Empty_Vector,
            Fixed         => False,
            Cached_Size   => 0,
            Data          => Skill.Types.Pools.Empty_Data,
            Owner         => null,
            Static_Data   => A1.Empty_Vector,
            New_Objects   => A1.Empty_Vector);

         This.Base := Convert (This);
         return Convert (This);
      exception
         when E : others =>
            Skill.Errors.Print_Stacktrace(E);
            Skill.Errors.Print_Stacktrace;
            raise Skill.Errors.Skill_Error with "Age pool allocation failed";
      end Make;

      procedure Free (This : access Pool_T) is
      begin
         This.Sub_Pools.Free;
         This.Data_Fields_F.Free;
         This.Blocks.Free;
         This.Static_Data.Free;
         This.New_Objects.Free;
      end;

      overriding function Insert_Instance
        (This : access Pool_T;
         ID   : Skill.Types.Skill_ID_T) return Boolean
      is
         function Convert is new Ada.Unchecked_Conversion
           (Source => Age.Age,
            Target => Skill.Types.Annotation);

         I : Natural := Natural (ID) - 1;
         R : Age.Age;
      begin
         if null /= This.Data (I) then
            return False;
         end if;

         R             := new Age.Age_T;
         R.Skill_ID    := ID;
         This.Data (I) := Convert (R);
         This.Static_Data.Append (R);
         return True;
      end Insert_Instance;

      overriding function Static_Size (This : access Pool_T) return Natural is
      begin
         return This.Static_Data.Length + This.New_Objects.Length;
      end Static_Size;

   end Age_P;

end Skill.Types.Pools.Age_Pools;
