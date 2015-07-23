--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     field handling in skill                             --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Field_Types;
with Skill.Internal.Parts;
with Interfaces;

package body Skill.Field_Declarations is

   procedure Add_Chunk
     (This : access Field_Declaration_T'Class;
      C    : Skill.Internal.Parts.Chunk)
   is
   begin
      This.Data_Chunks.Append (new Chunk_Entry_T'(C, null));
   end Add_Chunk;

   function Add_Offset_To_Last_Chunk
     (This        : access Field_Declaration_T'Class;
      Input       : Skill.Streams.Reader.Input_Stream;
      File_Offset : Types.v64) return Types.v64
   is
      CE : Chunk_Entry                := This.Data_Chunks.Last_Element;
      C  : Skill.Internal.Parts.Chunk := CE.C;

      use type Interfaces.Integer_64;
   begin
      C.First  := C.First + File_Offset;
      C.Last   := C.Last + File_Offset;
      CE.Input := Input.Map (0, C.First, C.Last);

      return C.Last;
   end Add_Offset_To_Last_Chunk;

   function Make_Lazy_Field
     (Owner : Owner_T;
      ID    : Natural;
      T     : Field_Types.Field_Type;
      Name  : Skill.Types.String_Access) return Lazy_Field
   is
   begin
      return new Lazy_Field_T'
          (Data_Chunks => Chunk_List_P.Empty_List,
           T           => T,
           Name        => Name,
           Index       => ID,
           Owner       => Owner);
   end Make_Lazy_Field;

end Skill.Field_Declarations;
