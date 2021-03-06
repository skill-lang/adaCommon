--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     skills vector container implementation              --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski, Timm Felden                  --
--                                                                            --
pragma Ada_2012;

with Ada.Finalization;

-- vector, can also be used as a stack
-- vector element operation is total, i.e. it will never raise an exception
-- instead of exception, Err_Val will be returned
generic
   type Index_Type is range <>;
   type Element_Type is private;
--     Err_Val : Element_Type;
package Skill.Containers.Vectors is

   type Vector_T is tagged limited private;
   type Vector is access Vector_T;

   function Empty_Vector return Vector;

   procedure Free (This : access Vector_T);

   -- applies F for each element in this
   procedure Foreach
     (This : not null access Vector_T'Class;
      F    : not null access procedure (I : Element_Type));

   -- appends element to the vector
   procedure Append
     (This        : not null access Vector_T'Class;
      New_Element : Element_Type);

-- appends element to the vector and assumes that the vector has a spare slot
   procedure Append_Unsafe
     (This        : not null access Vector_T'Class;
      New_Element : Element_Type);

   -- apppends all elements stored in argument vector
   procedure Append_All (This : access Vector_T'Class; Other : Vector);

   -- prepends all elements stored in argument vector
   procedure Prepend_All (This : access Vector_T'Class; Other : Vector);

   -- prepends a number of undefined elements to this vector
   procedure Append_Undefined
     (This  : access Vector_T'Class;
      Count : Natural);

   -- prepends a number of undefined elements to this vector
   procedure Prepend_Undefined
     (This  : access Vector_T'Class;
      Count : Natural);

   -- remove the last element
   function Pop (This : access Vector_T'Class) return Element_Type;

   -- get element at argument index
   function Element
     (This  : access Vector_T'Class;
      Index : Index_Type) return Element_Type with
      Pre => Check_Index (This, Index);

-- returns the last element in the vector or raises constraint error if empty
   function Last_Element (This : access Vector_T'Class) return Element_Type;

-- returns the first element in the vector or raises constraint error if empty
   function First_Element (This : access Vector_T'Class) return Element_Type;

   -- ensures that an index can be allocated
   procedure Ensure_Index
     (This      : access Vector_T'Class;
      New_Index : Index_Type);

   -- allocates an index, filling previous elements with random garbage!
   procedure Ensure_Allocation
     (This      : access Vector_T'Class;
      New_Index : Index_Type);

   -- length of the container
   function Length (This : access Vector_T'Class) return Natural;

   -- true iff empty
   function Is_Empty (This : access Vector_T'Class) return Boolean;

   -- remove all elements
   procedure Clear (This : access Vector_T'Class);

   -- checks if an index is used
   function Check_Index
     (This  : access Vector_T'Class;
      Index : Index_Type) return Boolean;

   -- replace element at given index
   procedure Replace_Element
     (This    : access Vector_T'Class;
      Index   : Index_Type;
      Element : Element_Type);

   pragma Inline (Foreach);
   --     pragma Inline (Append);
   pragma Inline (Append_Unsafe);
   pragma Inline (Pop);
   pragma Inline (Element);
   pragma Inline (Last_Element);
   pragma Inline (Ensure_Index);
   pragma Inline (Ensure_Allocation);
   pragma Inline (Length);
   pragma Inline (Is_Empty);
   pragma Inline (Clear);
   pragma Inline (Check_Index);
   pragma Inline (Replace_Element);

private
   subtype Index_Base is Index_Type'Base;

   type Element_Array_T is array (Index_Type range <>) of Element_Type;
   type Element_Array is not null access Element_Array_T;
   type Element_Array_Access is access all Element_Array_T;

   type Vector_T is tagged limited record
      -- access to the actual data stored in the vector
      Data : Element_Array;
      -- the next index to be used, i.e. an exclusive border
      Next_Index : Index_Base;
   end record;

end Skill.Containers.Vectors;
