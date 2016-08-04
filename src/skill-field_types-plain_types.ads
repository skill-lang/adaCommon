--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     implementation of builtin field types               --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

with Skill.Types;
with Skill.Hashes; use Skill.Hashes;
with Skill.Equals; use Skill.Equals;
with Skill.Streams;
with Skill.Streams.Writer;
with Skill.Types.Pools;
with Ada.Unchecked_Conversion;
with Ada.Tags;


-- generic stateless types
generic
   type T is private;
   Type_Id : Natural;
   Image : String;
   with function Read_Single
     (Input : access Streams.Reader.Abstract_Stream'Class) return T;
   with procedure Write_Single
     (This : access Streams.Writer.Sub_Stream_T; V : T);
   with function Offset_Single
     (Input : T) return Types.V64 is <>;
package Skill.Field_Types.Plain_Types is
   pragma Warnings (Off);

   package A1 is new Field_Types (T, Type_Id);

   type Field_Type is new A1.Field_Type with null record;

   overriding function To_String (This : Field_Type) return String is
     (Image);

   function Boxed is new Ada.Unchecked_Conversion(T, Types.Box);
   function Unboxed is new Ada.Unchecked_Conversion(Types.Box, T);

   overriding
   function Read_Box
     (This : access Field_Type;
         Input : Streams.Reader.Sub_Stream) return Types.Box is
     (Boxed(Read_Single(Input)));

   overriding
   function Offset_Box
     (This : access Field_Type;
      Target : Types.Box) return Types.V64 is
     (Offset_Single(Unboxed(Target)));

   overriding
   procedure Write_Box
     (This : access Field_Type;
      Output : Streams.Writer.Sub_Stream;
      Target : Types.Box);

end Skill.Field_Types.Plain_Types;
