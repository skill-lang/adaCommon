--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Your SKilL Scala Binding                            --
-- \__ \ ' <| | | |__     <<debug>>                                           --
-- |___/_|\_\_|_|____|    by: <<some developer>>                              --
--                                                                            --

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Skill.Equals;
with Skill.Errors;
with Skill.Field_Types;
with Skill.Files;
with Skill.Internal.File_Parsers;
with Skill.Internal.Parts;
with Skill.Streams;
with Skill.String_Pools;
with Skill.Types.Pools;
with Skill.Types;
with Skill.Types.Vectors;

with Age.Api;
with Age.Internal_Skill_Names;
with Age.Known_Field_Age_Age;

-- instantiated pool packages
-- GNAT Bug workaround; should be "new Base(...)" instead
package body Skill.Types.Pools.Age_Pools is

   package body Age_P is

      -- API methods
      function Get (This : access Pool_T; ID : Skill_ID_T) return Age.Age is
      begin
         if 0 = ID then
            return null;
         else
            return Age.To_Age (This.Data (ID));
         end if;
      end Get;

      ----------------------
      -- internal methods --
      ----------------------

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
         This :=
           new Pool_T'
             (Name          => Age.Internal_Skill_Names.Age_Skill_Name,
              Type_Id       => Type_Id,
              Super         => null,
              Base          => null,
              Sub_Pools     => Sub_Pool_Vector_P.Empty_Vector,
              Data_Fields_F =>
                Skill.Field_Declarations.Field_Vector_P.Empty_Vector,
              Blocks      => Skill.Internal.Parts.Blocks_P.Empty_Vector,
              Fixed       => False,
              Cached_Size => 0,
              Data        => Skill.Types.Pools.Empty_Data,
              Owner       => null,
              Static_Data => A1.Empty_Vector,
              New_Objects => A1.Empty_Vector);

         This.Base := Convert (This);
         return Convert (This);
      exception
         when E : others =>
            Skill.Errors.Print_Stacktrace (E);
            Skill.Errors.Print_Stacktrace;
            raise Skill.Errors.Skill_Error with "Age pool allocation failed";
      end Make;

      procedure Free (This : access Pool_T) is

         procedure Delete
           (This : Skill.Field_Declarations.Field_Declaration)
         is
         begin
            This.Free;
         end Delete;

         Data : Annotation_Array := This.Data;
         procedure Delete is new Ada.Unchecked_Deallocation
           (Skill.Types.Skill_Object,
            Skill.Types.Annotation);
         procedure Delete is new Ada.Unchecked_Deallocation
           (Skill.Types.Annotation_Array_T,
            Skill.Types.Annotation_Array);

         type P is access all Pool_T;
         procedure Delete is new Ada.Unchecked_Deallocation (Pool_T, P);
         D : P := P (This);
      begin
         for I in Data'Range loop
            Delete (Data (I));
         end loop;
         Delete (Data);

         This.Sub_Pools.Free;
         This.Data_Fields_F.Foreach (Delete'Access);
         This.Data_Fields_F.Free;
         This.Blocks.Free;
         This.Static_Data.Free;
         This.New_Objects.Free;
         Delete (D);
      end Free;

      function Add_Field
        (This : access Pool_T;
         ID   : Natural;
         T    : Field_Types.Field_Type;
         Name : String_Access)
         return Skill.Field_Declarations.Field_Declaration

      is
         function Convert is new Ada.Unchecked_Conversion
           (Field_Declarations.Lazy_Field,
            Field_Declarations.Field_Declaration);
         type P is access all Pool_T;
         function Convert is new Ada.Unchecked_Conversion
           (P,
            Field_Declarations.Owner_T);

         F : Field_Declarations.Field_Declaration;

         type Super is access all Base_Pool_T;
      begin

         if Skill.Equals.Equals
             (Name,
              Age.Internal_Skill_Names.Age_Skill_Name)
         then
            F := Age.Known_Field_Age_Age.Make (ID, T, Convert (P (This)));
         else
            return Super (This).Add_Field (ID, T, Name);
         end if;

         -- TODO restrictions
         --          for (FieldRestriction<?> r : restrictions)
         --              f.addRestriction(r);
         This.Data_Fields.Append (F);

         return F;
      end Add_Field;

      overriding function Insert_Instance
        (This : access Pool_T;
         ID   : Skill.Types.Skill_ID_T) return Boolean
      is
         function Convert is new Ada.Unchecked_Conversion
           (Source => Age.Age,
            Target => Skill.Types.Annotation);

         I : Natural := Natural (ID);
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
