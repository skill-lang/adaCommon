--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     iterator over types                                 --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

package body Skill.Iterators.Type_Hierarchy_Iterator is

   procedure Init (This : access Iterator'Class;
                   First : Skill.Types.Pools.Pool := null) is
   begin
      This.Current := First;
      if null /= First then
         This.End_Height := First.Type_Hierarchy_Height;
      end if;
   end Init;

   procedure Next
     (This : access Iterator'Class)
   is
   begin
      This.Current := This.Current.Next;
      if null = This.Current
        or else This.End_Height >= This.Current.Type_Hierarchy_Height
      then
         This.Current := null;
      end if;
   end Next;

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
