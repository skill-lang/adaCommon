--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file partitioning info                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;

with Skill.Types;
with Skill.Types.Vectors;

-- documentation can be found in java common
package Skill.Internal.Parts is
   pragma Preelaborate;

   type Block is record
      BPO   : Skill.Types.v64;
      Count : Skill.Types.v64;
   end record;

   package Blocks_P is new Skill.Types.Vectors (Natural, Block);
   subtype Blocks is Blocks_P.Vector;

   type Chunk_T is abstract tagged record
      First : Skill.Types.v64;
      Last  : Skill.Types.v64;
      Count : Skill.Types.v64;
   end record;
   type Chunk is access Chunk_T'Class;

   type Simple_Chunk is new Chunk_T with record
      BPO : Skill.Types.v64;
   end record;

   type Bulck_Chunk is new Chunk_T with null record;

   package Chunks is new Ada.Containers.Vectors (Natural, Chunk);
end Skill.Internal.Parts;
