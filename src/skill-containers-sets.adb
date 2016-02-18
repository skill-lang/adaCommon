--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     skills vector container implementation              --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski, Timm Felden                  --
--                                                                            --
with Ada.Unchecked_Deallocation;

package body Skill.Containers.Sets is

   function Next (This : access Iterator_T) return Skill.Types.Box is
      Result : Skill.Types.Box := Cast (HS.Element (This.Cursor));
   begin
      HS.Next (This.Cursor);
      return Result;
   end Next;

   procedure Free (This : access Iterator_T) is
      type T is access all Iterator_T;
      X : T := T (This);
      procedure Delete is new Ada.Unchecked_Deallocation (Iterator_T, T);
   begin
      Delete (X);
   end Free;

   procedure Add (This : access Set_T; V : Skill.Types.Box) is
   begin
      This.This.Include (Cast (V));
   end Add;

   function Make return Ref is
   begin
      return new Set_T'(This => HS.Empty_Set);
   end Make;

end Skill.Containers.Sets;
