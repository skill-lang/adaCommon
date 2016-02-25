--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file partitioning info                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;

with Skill.Types;
with Skill.Containers.Vectors;

-- documentation can be found in java common
package Skill.Internal.Parts is

   type Block is record
      BPO   : Skill.Types.Skill_ID_T;
      Static_Count : Skill.Types.Skill_ID_T;
      Dynamic_Count : Skill.Types.Skill_ID_T;
   end record;

   package Blocks_P is new Skill.Containers.Vectors (Natural, Block);
   subtype Blocks is Blocks_P.Vector;

   type Chunk_T is abstract tagged record
      First : Skill.Types.v64;
      Last  : Skill.Types.v64;
      Count : Skill.Types.Skill_ID_T;
   end record;
   type Chunk is access Chunk_T'Class;

   procedure Free (This : access Chunk_T) is abstract;

   type Simple_Chunk is new Chunk_T with record
      BPO : Skill.Types.Skill_ID_T;
   end record;
   type Simple_Chunk_X is not null access all Simple_Chunk;

   function To_Simple (This : access Chunk_T'Class) return Simple_Chunk_X;

   type Bulk_Chunk is new Chunk_T with record
      -- The Number of Blocks, Starting From The First That Have To Be Updated
      Block_Count : Natural;
   end record;
   type Bulk_Chunk_X is not null access all Bulk_Chunk;
   function To_Bulk (This : access Chunk_T'Class) return Bulk_Chunk_X;

   package Chunks is new Ada.Containers.Vectors (Natural, Chunk);

   procedure Free (This : access Simple_Chunk);
   procedure Free (This : access Bulk_Chunk);
end Skill.Internal.Parts;
