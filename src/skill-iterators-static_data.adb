--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     iterator over types                                 --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;
with Skill.Internal.Parts;

package body Skill.Iterators.Static_Data is

   function Make (First : Skill.Types.Pools.Pool := null) return Iterator is
      Rval : Iterator;
      B : Skill.Internal.Parts.Block;
   begin
      if null = First then
         return Iterator'(null, others => 0);
      else
         Rval := Iterator'(First, 0,
                           First.Blocks.Length, others => 0);
         While Rval.index = Rval.last and then Rval.secondIndex < Rval.LastBlock loop
            B := Rval.Current.Blocks.Element(Rval.SecondIndex);
            Rval.Index := Skill_ID_T(B.Bpo);
            Rval.Last := Rval.Index + Skill_ID_T(B.Static_Count);
            Rval.SecondIndex := Rval.SecondIndex + 1;
         end loop;
         -- mode switch, if there is no other block
         if Rval.Index= Rval.Last and then Rval.SecondIndex = Rval.LastBlock then
            Rval.SecondIndex := Rval.SecondIndex + 1;
            Rval.Index := 0;
            Rval.Last := Rval.Current.New_Objects_Size;
         end if;
         return Rval;
      end if;
   end Make;

   procedure Init (This : access Iterator'Class;
                   First : Skill.Types.Pools.Pool := null) is
      B : Skill.Internal.Parts.Block;
   begin
      This.Current := First;
      This.SecondIndex := 0;
      This.Index := 0;
      This.Last := 0;

      if null = First then
         This.LastBlock := 0;
         return;
      end if;
      This.LastBlock := First.Blocks.Length;

      While This.index = This.last and then This.secondIndex < This.LastBlock loop
         B := This.Current.Blocks.Element(This.SecondIndex);
         This.Index := Skill_ID_T(B.Bpo);
         This.Last := This.Index + Skill_ID_T(B.Static_Count);
         This.SecondIndex := This.SecondIndex + 1;
      end loop;
      -- mode switch, if there is no other block
      if This.Index= This.Last and then This.SecondIndex = This.LastBlock then
         This.SecondIndex := This.SecondIndex + 1;
         This.Index := 0;
         This.Last := This.Current.New_Objects_Size;
      end if;
   end;

   function Element
     (This : access Iterator'Class) return Annotation is
   begin
      if This.SecondIndex <= This.LastBlock then
         return This.Current.Base.Data(This.Index + 1);
      else
         return This.Current.New_Objects_Element (Natural (This.Index));
      end if;
   end Element;

   function Next
     (This : access Iterator'Class) return Annotation
   is
      Rval : Annotation;
      B : Skill.Internal.Parts.Block;
   begin
      This.Index := This.Index + 1;
      if This.SecondIndex <= This.LastBlock then
         Rval := This.Current.Base.Data(This.Index);
         if This.Index = This.Last then
            While This.Index = This.Last and then This.SecondIndex < This.LastBlock loop
               B := This.Current.Blocks.Element(This.SecondIndex);
               This.Index := Skill_ID_T(B.Bpo);
               This.Last := This.Index + Skill_ID_T(B.Static_Count);
               This.SecondIndex := This.SecondIndex + 1;
            end loop;
            -- mode switch, if there is no other block
            if This.Index= This.Last and then This.SecondIndex = This.LastBlock then
               This.SecondIndex := This.SecondIndex + 1;
               This.Index := 0;
               This.Last := This.Current.New_Objects_Size;
            end if;
         end if;
         return Rval;
      else
         return This.Current.New_Objects_Element (This.Index - 1);
      end if;
   end Next;
end Skill.Iterators.Static_Data;
