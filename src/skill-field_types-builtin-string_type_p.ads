--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     implementation of builtin field types               --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;
with Ada.Containers.Hashed_Maps;
with Ada.Tags;

with Skill.Types;
with Skill.Hashes; use Skill.Hashes;
with Skill.Equals; use Skill.Equals;
with Skill.String_Pools;
with Skill.Streams.Reader;
with Skill.Streams.Writer;
with Ada.Unchecked_Deallocation;


package Skill.Field_Types.Builtin.String_Type_P is
   pragma Warnings (Off);

   package A1 is new Field_Types (Types.String_Access, 14);

   package IDs is new Ada.Containers.Hashed_Maps
     (Key_Type        => Types.String_Access,
      Element_Type    => Types.Skill_ID_T,
      Hash            => Hash,
      Equivalent_Keys => Skill.Equals.Equals,
      "="             => "=");

   -- we need to pass a pointer to the map around
   type ID_Map is not null access all IDs.Map;

   type Field_Type_T is new A1.Field_Type with record
      Strings : Skill.String_Pools.Pool;
      String_IDs : aliased IDs.Map;
   end record;

   type Field_Type is access all Field_Type_T;

   function Make
     (Strings : String_Pools.Pool) return String_Type_P.Field_Type is
     (new Field_Type_T'(Strings, String_Type_P.Ids.Empty_Map));

   function Boxed is new Ada.Unchecked_Conversion(Types.String_Access, Types.Box);
   function Unboxed is new Ada.Unchecked_Conversion(Types.Box, Types.String_Access);


   function Read_Box
     (This : access Field_Type_T;
      Input : Streams.Reader.Stream) return Types.Box is
      (Boxed (This.Strings.Get (Input.V64)));

   function Offset_Box
     (This : access Field_Type_T;
         Target : Types.Box) return Types.V64 is
     (Offset_Single_V64(Types.V64(This.String_Ids.Element(Unboxed(Target)))));

   procedure Write_Box
     (This : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream; Target : Types.Box);

   procedure Write_Single_Field
     (THis : access Field_Type_T;
      V : Types.String_Access;
      Output : Skill.Streams.Writer.Sub_Stream);

   function Get_Id_Map (THis : access Field_Type_T) return ID_Map;

   overriding function To_String (This : Field_Type_T) return String is
     ("string");

end Skill.Field_Types.Builtin.String_Type_P;
