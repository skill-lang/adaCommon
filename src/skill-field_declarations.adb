--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     field handling in skill                             --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Field_Types;
with Skill.Internal.Parts;

package body Skill.Field_Declarations is

   procedure Add_Chunk
     (This : access Field_Declaration_T;
      C    : Skill.Internal.Parts.Chunk)
   is
   begin
      This.Data_Chunks.Append (Chunk_Entry'(C, null));
   end Add_Chunk;

end Skill.Field_Declarations;
