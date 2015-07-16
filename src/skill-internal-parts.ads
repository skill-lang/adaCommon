--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file partitioning info                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;

-- documentation can be found in java common
package Skill.Internal.Parts is
   pragma Preelaborate;

   type Block is record
      BPO   : Long_Integer;
      Count : Long_Integer;
   end record;

   package Blocks is new Ada.Containers.Vectors (Natural, Block);

   type Chunk is abstract tagged record
      First : Long_Integer;
      Last  : Long_Integer;
      Count : Long_Integer;
   end record;

   type Simple_Chunk is new Chunk with record
      BPO : Long_Integer;
   end record;

   type Bulck_Chunk is new Chunk with null record;

   type A1 is access Chunk'Class;
   package Chunks is new Ada.Containers.Vectors(Natural, A1);
end Skill.Internal.Parts;
