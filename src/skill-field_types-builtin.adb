--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     implementation of builtin field types               --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

with Skill.Types;
with Skill.Hashes; use Skill.Hashes;
with Skill.Types.Pools;

package body Skill.Field_Types.Builtin is

   package body Annotation_Type_P is

   procedure Fix_Types (This : access Field_Type_T; Tbn : Skill.Types.Pools.Type_Map) is
      begin
         This.Types_By_Name := Tbn;
      end Fix_Types;
   end Annotation_Type_P;

   package body String_Type_T is

      function Get_Id_Map (THis : access Field_Type_T) return ID_Map is
      Begin
         return This.String_IDs'access;
      end Get_Id_Map;

      procedure Write_Single_Field
        (THis   : access Field_Type_T;
         V      : Types.String_Access;
         Output : Skill.Streams.Writer.Sub_Stream)
      is
      begin
         Output.V64 (Types.V64 (THis.String_IDs.Element (V)));
      end Write_Single_Field;

   end String_Type_T;

end Skill.Field_Types.Builtin;
