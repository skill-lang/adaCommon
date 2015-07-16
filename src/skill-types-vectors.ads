--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     skills vector container implementation              --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski                               --
--                                                                            --

with Ada.Finalization;

generic
   type Index_Type is range <>;
   type Element_Type is private;
package Skill.Types.Vectors is
   pragma Preelaborate;

   type Vector is new Ada.Finalization.Limited_Controlled with private;
--   pragma Preelaborable_Initialization (Vector);

   procedure Append (
      Container   : in out Vector;
      New_Element : Element_Type
   );

   procedure Append_Unsafe (
      Container   : in out Vector;
      New_Element : Element_Type
   );

   function Check_Index (
      Container : in out Vector;
      Index     : Index_Type
   ) return Boolean;

   function Element (
      Container : in out Vector;
      Index     : Index_Type
   ) return Element_Type with
      Pre => Check_Index (Container, Index);

   procedure Ensure_Size (
      Container : in out Vector;
      N         : Index_Type
   );

   procedure Ensure_Allocation (
      Container : in out Vector;
      N         : Index_Type
   );

   function Length (
      Container : in out Vector
   ) return Index_Type;

   procedure Replace_Element (
      Container : in out Vector;
      Index     : Index_Type;
      Element   : Element_Type
   );

   overriding
   procedure Initialize (Object : in out Vector);

   --  Release the vector elements.
   overriding
   procedure Finalize (Object : in out Vector);

   pragma Inline (
      Element,
      Ensure_Size,
      Replace_Element,
      Initialize,
      Finalize
   );

private

   type Element_Array is array (Index_Type range <>) of Element_Type;
   type Element_Array_Access is access Element_Array;

   Null_Element_Array : constant Element_Array_Access := null;

   type Vector is new Ada.Finalization.Limited_Controlled with
      record
         Elements : Element_Array_Access;
         Size     : Index_Type := 2;
         Size_0   : Index_Type := 0;
      end record;

end Skill.Types.Vectors;
