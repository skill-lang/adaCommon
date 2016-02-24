--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     generic subpools                                    --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;
with Ada.Unchecked_Conversion;

with Skill.Containers.Vectors;
with Skill.Field_Types;
with Skill.Field_Types.Builtin;
with Skill.Field_Types.Builtin.String_Type_P;
with Skill.Files;
with Skill.Internal.File_Parsers;
with Skill.Streams.Reader;
with Skill.Streams.Writer;
with Skill.Types.Pools;
with Skill.Types;
with Skill.Books;

-- generic sub pool packages
generic
   type T is new Skill_Object with private;
   type P is access all t;

   with function To_P (This : Annotation) return P;

package Skill.Types.Pools.Sub is
   type Pool_T is new Sub_Pool_T with private;
   type Pool is access Pool_T;

   -- API methods
   function Get (This : access Pool_T; ID : Skill_ID_T) return P;

   overriding
   function Make_Boxed_Instance (This : access Pool_T) return Box;


   ----------------------
   -- internal methods --
   ----------------------

   -- constructor invoked by new_pool
   function Make
     (Super   : Skill.Types.Pools.Pool;
      Type_Id : Natural;
      Name    : String_Access) return Pools.Pool;
   -- destructor invoked by close
   procedure Free (This : access Pool_T);

   overriding function Add_Field
     (This : access Pool_T;
      ID   : Natural;
      T    : Field_Types.Field_Type;
      Name : String_Access) return Skill.Field_Declarations.Field_Declaration;

   procedure Add_Known_Field
     (This            : access Pool_T;
      Name            : String_Access;
      String_Type     : Field_Types.Builtin.String_Type_P.Field_Type;
      Annotation_Type : Field_Types.Builtin.Annotation_Type_P
        .Field_Type) is null;

   overriding procedure Resize_Pool (This : access Pool_T);


   function Make_Sub_Pool
     (This : access Pool_T;
      ID   : Natural;
      Name : String_Access) return Skill.Types.Pools.Pool is
     (Make (This.To_Pool, ID, Name));

   function First_Dynamic_New_Instance
     (This : access Pool_T) return Annotation is (null);

   -- RTTI implementation
   function Boxed is new Ada.Unchecked_Conversion (P, Types.Box);
   function Unboxed is new Ada.Unchecked_Conversion (Types.Box, P);

   function Read_Box
     (This  : access Pool_T;
      Input : Skill.Streams.Reader.Sub_Stream) return Types.Box is
     (Boxed (This.Get (Skill_ID_T (Input.V64))));

   function Offset_Box
     (This   : access Pool_T;
      Target : Types.Box) return Types.v64;

   procedure Write_Box
     (This   : access Pool_T;
      Output : Streams.Writer.Sub_Stream;
      Target : Types.Box);

   function Content_Tag
     (This : access Pool_T) return Ada.Tags.Tag is
     (This.To_Pool.Super.Dynamic.Content_Tag);
private

   package Book_P is new Skill.Books(T, P);

   type Pool_T is new Sub_Pool_T with record
      Book : aliased Book_P.Book;
   end record;

end Skill.Types.Pools.Sub;
