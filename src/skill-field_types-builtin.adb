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

package body Skill.Field_Types.Builtin is

   package body String_Type_T is

      procedure Clear_IDs
        (THis   : access Field_Type_T;
         V      : Types.String_Access;
         Output : Skill.Streams.Writer.Output_Stream)
      is
      begin
         Output.V64 (Types.v64 (THis.String_IDs.Element (V)));
      end Clear_IDs;

   end String_Type_T;

end Skill.Field_Types.Builtin;
