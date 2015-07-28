--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Your SKilL Scala Binding                            --
-- \__ \ ' <| | | |__     <<debug>>                                           --
-- |___/_|\_\_|_|____|    by: <<some developer>>                              --
--                                                                            --
with Ada.Unchecked_Deallocation;

with Skill.Files;
with Skill.Field_Declarations;
with Skill.Field_Types;

with Age.Internal_Skill_Names;

package body Age.Known_Field_Age_Age is

   function Make
     (ID    : Natural;
      T     : Skill.Field_Types.Field_Type;
      Owner : Skill.Field_Declarations.Owner_T)
      return Skill.Field_Declarations.Field_Declaration
   is
   begin
      return new Known_Field_Age_Age_T'
          (Data_Chunks => Skill.Field_Declarations.Chunk_List_P.Empty_Vector,
           T           => T,
           Name        => Internal_Skill_Names.Age_Skill_Name,
           Index       => ID,
           Owner       => Owner);
   end Make;

   procedure Free (This : access Known_Field_Age_Age_T) is
      type P is access all Known_Field_Age_Age_T;

      procedure Delete is new Ada.Unchecked_Deallocation
        (Known_Field_Age_Age_T,
         P);
      D : P := P (This);
   begin
      This.Data_Chunks.Foreach (Skill.Field_Declarations.Delete_Chunk'Access);
      This.Data_Chunks.Free;
      Delete (D);
   end Free;

end Age.Known_Field_Age_Age;
