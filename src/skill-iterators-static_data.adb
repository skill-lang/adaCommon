--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     iterator over types                                 --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

package body Skill.Iterators.Static_Data is

   function Make (First : Skill.Types.Pools.Pool := null) return Iterator is
   begin
      if null = First then
         return Iterator'(null, others => 0);
      else
         return Iterator'(First, 0,
                          First.Blocks.Length, others => 0);
      end if;
   end Make;

   function Element
     (This : access Iterator'Class) return Annotation is
   begin
      if This.SecondIndex <= This.LastBlock then
         return This.Current.Base.Data(This.Index + 1);
      else
         return null;--This.Current.New_Objects(This.Index);
      end if;
   end Element;

   function Next
     (This : access Iterator'Class) return Annotation
   is
      Rval : constant Skill.Types.Pools.Pool := This.Current;
   begin
      return null;
   end Next;
end Skill.Iterators.Static_Data;
