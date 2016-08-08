--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     skills vector container implementation              --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski, Timm Felden                  --
--                                                                            --
pragma Ada_2012;
with Ada.Unchecked_Deallocation;

package body Skill.Containers.Arrays is

   function Next (This : access Iterator_T) return Skill.Types.Box is
      Result : Skill.Types.Box := Cast (This.This.Element (This.Cursor));
   begin
      This.Cursor := This.Cursor + 1;
      return Result;
   end Next;

   procedure Free (This : access Iterator_T) is
      type T is access all Iterator_T;
      X : T := T (This);
      procedure Delete is new Ada.Unchecked_Deallocation (Iterator_T, T);
   begin
      Delete (X);
   end Free;

   procedure Append (This : access Array_T; V : Box) is
   begin
      This.This.Append (Cast (V));
   end Append;

   procedure Add (This : access Array_T; V : Box) is
   begin
      This.This.Append (Cast (V));
   end Add;

   procedure Update (This : access Array_T; I : Natural; V : Box) is
   begin
      This.This.Replace_Element (I, Cast (V));
   end Update;

   procedure Ensure_Size (This : access Array_T; I : Natural) is
   begin
      This.This.Ensure_Index (I);
   end Ensure_Size;

   function Make return Ref is
   begin
      return new Array_T'(This => Vec.Empty_Vector);
   end Make;

end Skill.Containers.Arrays;
