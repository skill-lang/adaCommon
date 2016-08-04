--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     Iterators for SKilL API                             --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

generic
   type T is private;
   type Index_Type is range <>;
package Skill.Types.Iterators is
   subtype Index_Base is Index_Type'Base;

   type Abstract_Iterator is abstract tagged private;
   type Iterator is not null access Abstract_Iterator'Class;

   function Next (This : access Abstract_Iterator) return T is abstract;
   function Has_Next
     (This : access Abstract_Iterator) return Boolean is abstract;

   type Array_Iterator_T is new Abstract_Iterator with private;
   type Array_Iterator_T_Array_T is array (Index_Type range <>) of T;
   type Array_Iterator_T_Array is not null access all Array_Iterator_T_Array_T;

   function New_Array (Data : Array_Iterator_T_Array) return Iterator;
   function New_Array
     (Data  : Array_Iterator_T_Array;
      First : Index_Type;
      Last  : Index_Type) return Iterator;
   function Next (This : access Array_Iterator_T) return T;
   function Has_Next (This : access Array_Iterator_T) return Boolean;

   type Empty_Iterator_T is new Abstract_Iterator with private;

   function New_Empty return Iterator;
   function Next
     (This : access Empty_Iterator_T) return T is
     (raise Constraint_Error with "empty iterator has no next element");
   function Has_Next
     (This : access Empty_Iterator_T) return Boolean is
     (False);

private
   type Abstract_Iterator is abstract tagged null record;
   type Array_Iterator_T is new Abstract_Iterator with record
      Data     : Array_Iterator_T_Array;
      Position : Index_Type;
      -- exclusive last position
      Last : Index_Base;
   end record;
   type Empty_Iterator_T is new Abstract_Iterator with null record;

end Skill.Types.Iterators;
