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
with Ada.Tags;

package body Skill.Field_Types.Plain_Types is

   procedure Write_Box
     (This   : access Field_Type;
      Output : Streams.Writer.Sub_Stream;
      Target : Types.Box)
   is
   begin
      Write_Single (Output, Unboxed (Target));
   end Write_Box;

end Skill.Field_Types.Plain_Types;
