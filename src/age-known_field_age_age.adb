--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Your SKilL Scala Binding                            --
-- \__ \ ' <| | | |__     <<debug>>                                           --
-- |___/_|\_\_|_|____|    by: <<some developer>>                              --
--                                                                            --

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Skill.Files;
with Skill.Field_Declarations;
with Skill.Field_Types;
with Skill.Internal.Parts;
with Skill.Streams.Reader;
with Skill.Types;
with Skill.Types.Pools.Age_Pools;

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

   function Owner_Dyn
     (This : access Known_Field_Age_Age_T)
      return Skill.Types.Pools.Age_Pools.Age_P.Pool
   is
      function Cast is new Ada.Unchecked_Conversion
        (Skill.Field_Declarations.Owner_T,
         Skill.Types.Pools.Age_Pools.Age_P.Pool);
   begin
      return Cast (This.Owner);
   end Owner_Dyn;

   procedure Read
     (This : access Known_Field_Age_Age_T;
      CE   : Skill.Field_Declarations.Chunk_Entry)
   is
      First : Natural;
      Last  : Natural;
      Data  : Skill.Types.Annotation_Array    := Owner_Dyn (This).Data;
      Input : Skill.Streams.Reader.Sub_Stream := CE.Input;
   begin
      if CE.C.all in Skill.Internal.Parts.Simple_Chunk then
         First := Natural (CE.C.To_Simple.BPO);
         Last  := First + Natural (CE.C.Count);
      else
         First := Natural (This.Owner.Blocks.Last_Element.BPO);
         Last  := First + Natural (This.Owner.Blocks.Last_Element.Count);
         -- TODO This is horribly incorrect!!!
      end if;

      for I in First + 1 .. Last loop
         To_Age (Data (I)).Set_Age (Input.V64);
      end loop;
   end Read;
end Age.Known_Field_Age_Age;
