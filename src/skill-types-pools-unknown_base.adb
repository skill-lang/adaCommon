--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     unknown base pools                                  --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Skill.Equals;
with Skill.Errors;
with Skill.Field_Declarations;
with Skill.Field_Types;
with Skill.Files;
with Skill.Internal.File_Parsers;
with Skill.Internal.Parts;
with Skill.Streams;
with Skill.String_Pools;
with Skill.Types.Pools;
with Skill.Types;
with Skill.Containers.Vectors;

-- instantiated pool packages
-- GNAT Bug workaround; should be "new Base(...)" instead
package body Skill.Types.Pools.Unknown_Base is

   -- API methods
   function Get (This : access Pool_T; ID : Skill_ID_T) return Annotation is
   begin
      if 0 = ID then
         return null;
      else
         return This.Data (ID);
      end if;
   end Get;


   overriding
   function Make_Boxed_Instance (This : access Pool_T) return Box is
   begin
      raise Constraint_Error
        with "one must not reflectively allocate an instance of an unknown type!";
      return null;
   end Make_Boxed_Instance;

   ----------------------
   -- internal methods --
   ----------------------

   -- constructor invoked by new_pool
   function Make
     (Type_Id : Natural;
      Name    : String_Access) return Skill.Types.Pools.Pool
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
           Super         => null,
           Base          => null,
           Sub_Pools     => Sub_Pool_Vector_P.Empty_Vector,
           Next     => null, -- can only be calculated after all types are known
           Super_Type_Count => 0,
           Data_Fields_F =>
             Skill.Field_Declarations.Field_Vector_P.Empty_Vector,
           Known_Fields => No_Known_Fields,
           Blocks      => Skill.Internal.Parts.Blocks_P.Empty_Vector,
           Cached_Size => 0,
           Data        => Skill.Types.Pools.Empty_Data,
           Owner       => null,
           New_Objects => New_Objects_P.Empty_Vector,
           Static_Data_instances  => 0,
          others => <>);

      This.Base := Convert (This);
      return Convert (This);
   exception
      when E : others =>
         raise Skill.Errors.Skill_Error with "Age pool allocation failed";
   end Make;

   procedure Free (This : access Pool_T) is

      procedure Delete (This : Skill.Field_Declarations.Field_Declaration) is
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
      if 0 /= Data'Length then
         Delete (Data);
      end if;

      This.Sub_Pools.Free;
      This.Data_Fields_F.Foreach (Delete'Access);
      This.Data_Fields_F.Free;
      This.Blocks.Free;
      This.Book.Free;
      This.New_Objects.Free;
      Delete (D);
   end Free;

   function Add_Field
     (This : access Pool_T;
      ID   : Natural;
      T    : Field_Types.Field_Type;
      Name : String_Access;
      Restrictions : Field_Restrictions.Vector) return Skill.Field_Declarations.Field_Declaration
   is
      type Super is access all Base_Pool_T;
   begin
      return Super (This).Add_Field (ID, T, Name, Restrictions);
   end Add_Field;


   procedure Resize_Pool (This : access Pool_T) is
      ID   : Skill_ID_T := 1 + Skill_ID_T (This.Blocks.Last_Element.BPO);
      Size : Skill_ID_T := This.Blocks.Last_Element.Static_Count;

      Data : Skill.Types.Annotation_Array;

      SD : Book_P.Page;

      use Interfaces;
   begin
      This.Resize_Data;
      Data := This.Data;

      This.Static_Data_Instances := This.Static_Data_Instances + Size;

      if 0 = Size then
         return;
      end if;

      SD := This.Book.Make_Page(Size);

      -- set skill IDs and insert into data
      for R of SD.all loop
         R.Skill_ID := ID;
         Data (ID)  := R.To_Annotation;
         ID         := ID + 1;
      end loop;
   end Resize_Pool;

   procedure Write_Box
     (This   : access Pool_T;
      Output : Streams.Writer.Sub_Stream;
      Target : Types.Box)
   is
   begin
      if null = Unboxed (Target) then
         Output.I8 (0);
      else
         Output.V64 (Types.v64 (Unboxed (Target).Skill_ID));
      end if;
   end Write_Box;

end Skill.Types.Pools.Unknown_Base;
