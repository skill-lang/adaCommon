--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     skills vector container implementation              --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski, Timm Felden                  --
--                                                                            --
pragma Ada_2012;
with Ada.Unchecked_Deallocation;

package body Skill.Containers.Maps is

   procedure Advance (This : access Iterator_T) is
   begin
      HS.Next (This.Cursor);
   end Advance;

   procedure Free (This : access Iterator_T) is
      type T is access all Iterator_T;
      X : T := T (This);
      procedure Delete is new Ada.Unchecked_Deallocation (Iterator_T, T);
   begin
      Delete (X);
   end Free;

   procedure Update
     (This : access Map_T;
      K    : Skill.Types.Box;
      V    : Skill.Types.Box)
   is
   begin
      HS.Include (This.This, CastK (K), CastV (V));
   end Update;

   procedure Remove (This : access Map_T; K : Skill.Types.Box) is
   begin
      HS.Exclude (This.This, CastK (K));
   end Remove;

   function Make return Ref is
   begin
      return new Map_T'(This => HS.Empty_Map);
   end Make;

end Skill.Containers.Maps;
