--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     unknown base pools                                  --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

with Ada.Unchecked_Conversion;

with Skill.Containers.Vectors;
with Skill.Field_Declarations;
with Skill.Field_Types.Builtin;
with Skill.Field_Types.Builtin.String_Type_P;
with Skill.Files;
with Skill.Internal.File_Parsers;
with Skill.Streams.Reader;
with Skill.Streams.Writer;
with Skill.Types;
with Skill.Types.Pools;
with Skill.Types.Pools.Sub;
with Skill.Books;

-- instantiated pool packages
-- GNAT Bug workaround; should be "new Base(Annotation...)" instead
package Skill.Types.Pools.Unknown_Base is
   pragma Warnings (Off);

   type Pool_T is new Base_Pool_T with private;
   type Pool is access Pool_T;

   -- API methods
   function Get (This : access Pool_T; ID : Skill_ID_T) return Annotation;


   overriding
   function Make_Boxed_Instance (This : access Pool_T) return Box;

   ----------------------
   -- internal methods --
   ----------------------

   -- constructor invoked by new_pool
   function Make (Type_Id : Natural; Name : String_Access) return Pools.Pool;
   -- destructor invoked by close
   procedure Free (This : access Pool_T);

   overriding function Add_Field
     (This : access Pool_T;
      ID   : Natural;
      T    : Field_Types.Field_Type;
      Name : String_Access;
      Restrictions : Field_Restrictions.Vector) return Skill.Field_Declarations.Field_Declaration;

   procedure Add_Known_Field
     (This            : access Pool_T;
      Name            : String_Access;
      String_Type     : Field_Types.Builtin.String_Type_P.Field_Type;
      Annotation_Type : Field_Types.Builtin.Annotation_Type_P
        .Field_Type) is null;

   overriding procedure Resize_Pool (This : access Pool_T);

   function Cast_Annotation (This : Annotation) return Annotation is (This);
   pragma Inline (Cast_Annotation);

   package Sub_Pools is new Pools.Sub
     (Skill_Object,
      Annotation,
      Cast_Annotation);

   function Make_Sub_Pool
     (This : access Pool_T;
      ID   : Natural;
      Name : String_Access) return Skill.Types.Pools.Pool is
     (Sub_Pools.Make (This.To_Pool, ID, Name));

   -- RTTI implementation
   function Boxed is new Ada.Unchecked_Conversion
     (Types.Annotation,
      Types.Box);
   function Unboxed is new Ada.Unchecked_Conversion
     (Types.Box,
      Types.Annotation);

   function Read_Box
     (This  : access Pool_T;
      Input : Skill.Streams.Reader.Stream) return Types.Box is
     (Boxed (This.Get (Skill_ID_T (Input.V64))));

   function Offset_Box
     (This   : access Pool_T;
      Target : Types.Box) return Types.v64 is
     (Field_Types.Builtin.Offset_Single_V64
        (Types.v64 (Unboxed (Target).Skill_ID)));

   procedure Write_Box
     (This   : access Pool_T;
      Output : Streams.Writer.Sub_Stream;
      Target : Types.Box);

   function Content_Tag
     (This : access Pool_T) return Ada.Tags.Tag is
     (Skill_Object'Tag);
private

   package Book_P is new Skill.Books(Skill.Types.Skill_Object, Skill.Types.Annotation);

   type Pool_T is new Base_Pool_T with record
      Book : aliased Book_P.Book;
   end record;

end Skill.Types.Pools.Unknown_Base;
