--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     iterator over types                                 --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

package body Skill.Iterators.Type_Hierarchy_Iterator is

   function Make (First : Skill.Types.Pools.Pool := null) return Iterator is
   begin
      if null = First then
         return Iterator'(null, 0);
      else
         return Iterator'(First, First.Type_Hierarchy_Height);
      end if;
   end Make;

   function Next
     (This : access Iterator'Class) return Skill.Types.Pools.Pool
   is
      Rval : constant Skill.Types.Pools.Pool := This.Current;
   begin
      This.Current := This.Current.Next;
      if null = This.Current
        or else This.End_Height >= This.Current.Type_Hierarchy_Height
      then
         This.Current := null;
      end if;
      return Rval;
   end Next;
end Skill.Iterators.Type_Hierarchy_Iterator;
