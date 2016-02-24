--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     iterator over types                                 --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
with Skill.Internal.Parts;

package body Skill.Iterators.Type_Order is

   procedure Init (This : access Iterator'Class; First : Skill.Types.Pools.Pool)
   is
      T : Skill.Types.Pools.Pool;
   begin
      This.Ts.Init(First);
      while This.Ts.Has_Next loop
         T := This.Ts.Next;
         if T.Static_Size > 0 then
            This.Data.Init(T);
            return;
         end if;
      end loop;
      -- initialize with empty
      This.Data.Init(First);
   end Init;

   function Next
     (This : access Iterator'Class) return Annotation
   is
      Rval : Annotation := This.Data.Next;
      T : Skill.Types.Pools.Pool;
   begin
      if not This.Data.Has_Next then
         while This.Ts.Has_Next loop
            T := This.Ts.Next;
            if T.Static_Size > 0 then
               This.Data.Init(T);
               return Rval;
            end if;
         end loop;
      end if;

      return Rval;
   end Next;
end Skill.Iterators.Type_Order;
