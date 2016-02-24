--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     iterator over types                                 --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
with Skill.Internal.Parts;

package body Skill.Iterators.Dynamic_New_Instances is

   procedure Init (This : access Iterator'Class;
                   First : Skill.Types.Pools.Pool) is
   begin
      This.Current.Init (First);
      This.Index := 0;
      This.Last := First.New_Objects_Size;
      while 0 = This.Last loop
         This.Current.Next;
         if This.Current.Has_Next then
            This.Last := This.Current.Element.New_Objects_Size;
         else
            return;
         end if;
      end loop;
   end Init;

   function Element
     (This : access Iterator'Class) return Annotation is
   begin
      return This.Current.Element.New_Objects_Element(This.Index);
   end Element;

   function Next
     (This : access Iterator'Class) return Annotation
   is
      Rval : Annotation := This.Current.Element.New_Objects_Element(This.Index);
   begin
      This.Index := This.Index + 1;
      if This.Index = This.Last then
         This.Index := 0;
         This.Last := 0;
         while 0 = This.Last loop
            This.Current.Next;
            if This.Current.Has_Next then
               This.Last := This.Current.Element.New_Objects_Size;
            else
               return Rval;
            end if;
         end loop;
      end if;
      return Rval;
   end Next;
end Skill.Iterators.dynamic_new_instances;
