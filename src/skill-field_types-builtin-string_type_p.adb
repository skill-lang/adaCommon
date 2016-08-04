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
with Skill.Types.Pools;
with Ada.Tags;

package body Skill.Field_Types.Builtin.String_Type_P is

   function Get_Id_Map (THis : access Field_Type_T) return ID_Map is
   begin
      return THis.String_IDs'Access;
   end Get_Id_Map;

   procedure Write_Box
     (This   : access Field_Type_T;
      Output : Streams.Writer.Sub_Stream;
      Target : Types.Box)
   is
      V : Types.String_Access := Unboxed (Target);
      use type Types.String_Access;
   begin
      if null = V then
         Output.I8 (0);
      else
         Output.V64 (Types.v64 (This.String_IDs.Element (V)));
      end if;
   end Write_Box;

   procedure Write_Single_Field
     (THis   : access Field_Type_T;
      V      : Types.String_Access;
      Output : Skill.Streams.Writer.Sub_Stream)
   is
      use type Types.String_Access;
   begin
      if null = V then
         Output.I8 (0);
      else
         Output.V64 (Types.v64 (THis.String_IDs.Element (V)));
      end if;
   end Write_Single_Field;

end Skill.Field_Types.Builtin.String_Type_P;
