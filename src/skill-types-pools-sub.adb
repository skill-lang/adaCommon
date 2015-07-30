--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     unknown base pools                                  --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
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
with Ada.Text_IO;

-- instantiated pool packages
-- GNAT Bug workaround; should be "new Base(...)" instead
package body Skill.Types.Pools.Sub is

   -- API methods
   function Get (This : access Pool_T; ID : Skill_ID_T) return P is
   begin
      if 0 = ID then
         return null;
      else
         return To_P (This.Base.Data (ID));
      end if;
   end Get;

   ----------------------
   -- internal methods --
   ----------------------

   -- constructor invoked by new_pool
   function Make
     (Super   : Skill.Types.Pools.Pool;
      Type_Id : Natural;
      Name    : String_Access) return Pools.Pool
   is
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
          (Name          => Name,
           Type_Id       => Type_Id,
           Super         => Super,
           Base          => Super.Base,
           Sub_Pools     => Sub_Pool_Vector_P.Empty_Vector,
           Data_Fields_F =>
             Skill.Field_Declarations.Field_Vector_P.Empty_Vector,
           Blocks      => Skill.Internal.Parts.Blocks_P.Empty_Vector,
           Fixed       => False,
           Cached_Size => 0,
           Static_Data => A1.Empty_Vector,
           New_Objects => A1.Empty_Vector);

      return Convert (This);
   exception
      when E : others =>
         Skill.Errors.Print_Stacktrace (E);
         Skill.Errors.Print_Stacktrace;
         raise Skill.Errors.Skill_Error
           with "Generic sub pool allocation failed";
   end Make;

   procedure Free (This : access Pool_T) is

      procedure Delete (This : Skill.Field_Declarations.Field_Declaration) is
      begin
         This.Free;
      end Delete;

      type P is access all Pool_T;
      procedure Delete is new Ada.Unchecked_Deallocation (Pool_T, P);
      D : P := P (This);
   begin
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
      Name : String_Access) return Skill.Field_Declarations.Field_Declaration
   is
      type Super is access all Sub_Pool_T;
   begin
      return Super (This).Add_Field (ID, T, Name);
   end Add_Field;

   overriding function Insert_Instance
     (This : access Pool_T;
      ID   : Skill.Types.Skill_ID_T) return Boolean
   is
      I : Natural := Natural (ID);
      R : P;
   begin

      if null /= This.Base.Data (I) then
         return False;
      end if;

      R                  := new T;
      R.Skill_ID         := ID;
      This.Base.Data (I) := To_Annotation (R);
      This.Static_Data.Append (R);
      return True;
   end Insert_Instance;

   overriding function Static_Size (This : access Pool_T) return Natural is
   begin
      return This.Static_Data.Length + This.New_Objects.Length;
   end Static_Size;

end Skill.Types.Pools.Sub;