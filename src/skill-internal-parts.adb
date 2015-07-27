--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file partitioning info                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;

with Skill.Types;
with Skill.Types.Vectors;
with Ada.Unchecked_Deallocation;

-- documentation can be found in java common
package body Skill.Internal.Parts is

   procedure Free (This : access Simple_Chunk) is
      type T is access all Simple_Chunk;
      procedure Delete is new Ada.Unchecked_Deallocation (Simple_Chunk, T);
      D : T := T (This);
   begin
      Delete (D);
   end Free;

   procedure Free (This : access Bulk_Chunk) is
      type T is access all Bulk_Chunk;
      procedure Delete is new Ada.Unchecked_Deallocation (Bulk_Chunk, T);
      D : T := T (This);
   begin
      Delete (D);
   end Free;

end Skill.Internal.Parts;
