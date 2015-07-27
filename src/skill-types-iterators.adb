--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     API types for skill types                           --
-- |___/_|\_\_|_|____|    by: Timm Felden, Dennis Przytarski                  --
--                                                                            --

with Interfaces;

package body Skill.Types.Iterators is

   function New_Array
     (Data : Array_Iterator_T_Array) return Iterator is
     (new Array_Iterator_T'(Data, Data'First, Data'Last));

   function New_Array
     (Data  : Array_Iterator_T_Array;
      First : Index_Type;
      Last  : Index_Type) return Iterator is
     (new Array_Iterator_T'(Data, First, Last));

   function Next (This : access Array_Iterator_T) return T is
      R : T := This.Data (This.Position);
   begin
      This.Position := This.Position + 1;
      return R;
   end Next;

   function Has_Next
     (This : access Array_Iterator_T) return Boolean is
     (This.Position < This.Last);

   The_Empty_Iterator : Iterator := new Empty_Iterator_T;
   function New_Empty return Iterator is (The_Empty_Iterator);

end Skill.Types.Iterators;
