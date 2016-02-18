--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     skills vector container implementation              --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski, Timm Felden                  --
--                                                                            --

package body Skill.Containers.Arrays is

   procedure Append (This : access Array_T; V : Box) is
   begin
      This.This.Append (Cast (V));
   end Append;

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
