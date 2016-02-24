--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     iterator over types                                 --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Types.Pools;

package Skill.Iterators.Type_Hierarchy_Iterator is
   use type Skill.Types.Pools.Pool;

   type Iterator is tagged record
      Current : Skill.Types.Pools.Pool;
      End_Height : Natural;
   end record;

   procedure Init (This : access Iterator'Class;
                  First : Skill.Types.Pools.Pool := null);

   function Element (This : access Iterator'Class)
                     return Skill.Types.Pools.Pool is
     (This.Current);

   function Has_Next (This : access Iterator'Class) return Boolean is
     (null /= This.Current);

   procedure Next (This : access Iterator'Class);
   function Next (This : access Iterator'Class)
                  return Skill.Types.Pools.Pool;

end Skill.Iterators.Type_Hierarchy_Iterator;
