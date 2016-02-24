--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     iterator over static instances                      --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Types;
with Skill.Types.Pools;
with Skill.Iterators.Type_Hierarchy_Iterator;

package Skill.Iterators.Dynamic_New_Instances is
   use Skill.Types;
   use type Skill.Types.Pools.Pool;

   -- @note in contrast to c++, this implementation uses type erasure as I dont
   -- see how to solve elaboration otherwise
   type Iterator is tagged record
      -- current type
      Current : aliased Type_Hierarchy_Iterator.Iterator;
      Index       : Skill_ID_T;
      Last        : Skill_ID_T;
   end record;

   procedure Init (This : access Iterator'Class;
                  First : Skill.Types.Pools.Pool);

   function Element (This : access Iterator'Class) return Annotation;

   function Has_Next
     (This : access Iterator'Class) return Boolean is
     (This.Index /= This.Last);

   function Next (This : access Iterator'Class) return Annotation;

end Skill.Iterators.Dynamic_New_Instances;
