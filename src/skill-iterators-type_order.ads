--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     iterator over all instances                         --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Types;
with Skill.Types.Pools;
with Skill.Iterators.Type_Hierarchy_Iterator;
with Skill.Iterators.Static_Data;

package Skill.Iterators.Type_Order is
   use Skill.Types;
   use Skill.Iterators;


   -- @note in contrast to c++, this implementation uses type erasure as I dont
   -- see how to solve elaboration otherwise
   type Iterator is tagged record
      Ts : aliased Type_Hierarchy_Iterator.Iterator;
      Data : aliased Static_Data.Iterator;
   end record;

   procedure Init (This : access Iterator'Class; First : Skill.Types.Pools.Pool);

   function Element
     (This : access Iterator'Class) return Annotation is
     (This.Data.Element);

   function Has_Next
     (This : access Iterator'Class) return Boolean is
     (This.Data.Has_Next);

   function Next (This : access Iterator'Class) return Annotation;

end Skill.Iterators.Type_Order;
