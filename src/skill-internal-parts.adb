--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file partitioning info                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;

with Skill.Types;
with Skill.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

-- documentation can be found in java common
package body Skill.Internal.Parts is

   function To_Simple (This : access Chunk_T'Class) return Simple_Chunk_X is
      type P is access all Chunk_T'Class;
      function Cast is new Ada.Unchecked_Conversion (P, Simple_Chunk_X);
   begin
      return Cast (This);
   end To_Simple;

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
