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
with Ada.Tags;
with Ada.Unchecked_Conversion;

with Skill.Types;
with Skill.Hashes; use Skill.Hashes;
with Skill.Equals; use Skill.Equals;
with Skill.Streams;
with Skill.Streams.Writer;
with Skill.Types.Pools;

generic
   type T is private;
   Type_Id : Natural;
   Image : String;
package Skill.Field_Types.Constant_Types is

   package A1 is new Field_Types (T, Type_Id);

   type Field_Type is new A1.Field_Type with record
      Value : T;
   end record;

   overriding function To_String (This : Field_Type) return String is
     (Image);

      overriding
   function Read_Box
     (This : access Field_Type;
      Input : Streams.Reader.Stream) return Types.Box;

   overriding
   function Offset_Box
     (This : access Field_Type;
      Target : Types.Box) return Types.V64 is
     (0);

   overriding
   procedure Write_Box
     (This : access Field_Type;
      Output : Streams.Writer.Sub_Stream;
      Target : Types.Box) is null;

end Skill.Field_Types.Constant_Types;
