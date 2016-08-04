--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     iterator over static instances                      --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

with Skill.Types;
with Skill.Types.Pools;

package Skill.Iterators.Static_Data is
   use Skill.Types;
   use type Skill.Types.Pools.Pool;

   -- @note in contrast to c++, this implementation uses type erasure as I dont
   -- see how to solve elaboration otherwise
   type Iterator is tagged record
      Current : Skill.Types.Pools.Pool;

      SecondIndex : Skill_ID_T;
      LastBlock   : Skill_ID_T;
      Index       : Skill_ID_T;
      Last        : Skill_ID_T;
   end record;

   function Make (First : Skill.Types.Pools.Pool := null) return Iterator;
   procedure Init (This : access Iterator'Class;
                   First : Skill.Types.Pools.Pool := null);

   function Element
     (This : access Iterator'Class) return Annotation;

   function Has_Next
     (This : access Iterator'Class) return Boolean is
     (This.Index /= This.Last);

   function Next (This : access Iterator'Class) return Annotation;

end Skill.Iterators.Static_Data;
