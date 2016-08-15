--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     skills vector container implementation              --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski, Timm Felden                  --
--                                                                            --
pragma Ada_2012;

with Ada.Unchecked_Deallocation;

package body Skill.Containers.Vectors is

   function Empty_Vector return Vector is
   begin
      return new Vector_T'
          (Data =>
             new Element_Array_T (Index_Type'First .. Index_Type'First + 4),
           Next_Index => Index_Type'First);
   end Empty_Vector;

   procedure Free (This : access Vector_T) is

      type V is access all Vector_T;

      procedure Delete is new Ada.Unchecked_Deallocation
        (Element_Array_T,
         Element_Array_Access);

      procedure Delete is new Ada.Unchecked_Deallocation (Vector_T, V);
      D : Element_Array_Access := Element_Array_Access (This.Data);
      T : V                    := V (This);
   begin
      Delete (D);
      Delete (T);
   end Free;

   procedure Foreach
     (This : not null access Vector_T'Class;
      F    : not null access procedure (I : Element_Type))
   is
   begin
      for I in Index_Type'First .. Index_Type (This.Next_Index) - 1 loop
         F (This.Data (I));
      end loop;
   end Foreach;

   procedure Append
     (This        : not null access Vector_T'Class;
      New_Element : Element_Type)
   is
   begin
--        if not (Index_Type (This.Next_Index) < This.Data'Last) then
      This.Ensure_Index (Index_Type (This.Next_Index));
--        end if;
      This.Append_Unsafe (New_Element);
   end Append;

   procedure Append_Unsafe
     (This        : not null access Vector_T'Class;
      New_Element : Element_Type)
   is
   begin
      This.Data (Index_Type (This.Next_Index)) := New_Element;
      This.Next_Index                          := This.Next_Index + 1;
   end Append_Unsafe;

   procedure Append_All (This : access Vector_T'Class; Other : Vector) is
   begin
      if Other.Is_Empty then
         return;
      end if;

      This.Ensure_Index
      (Index_Base (This.Next_Index) + Index_Base (Other.Length));
      for I in Index_Type'First .. Other.Next_Index - 1 loop
         This.Append_Unsafe (Other.Data (I));
      end loop;
   end Append_All;

   procedure Prepend_All (This : access Vector_T'Class; Other : Vector) is
      I            : Index_Type;
      Other_Length : Index_Type;
   begin
      if Other.Is_Empty then
         return;
      end if;

      Other_Length := Index_Type (Other.Length);
      I            := Index_Base (This.Next_Index) + Index_Base (Other_Length);
      This.Ensure_Index (I);
      This.Next_Index := I;

      -- move elements from the back, so we can do it in one iteration
      loop
         I := I - 1;

         if I - Other_Length >= Index_Type'First then
            This.Data (I) := This.Data (I - Other_Length);
         else
            This.Data (I) := Other.Data (I);
         end if;

         exit when I = Index_Type'First;
      end loop;

   end Prepend_All;

   function Pop (This : access Vector_T'Class) return Element_Type is
   begin
      This.Next_Index := This.Next_Index - 1;
      return This.Data (Index_Type (This.Next_Index));
   end Pop;

   function Element
     (This  : access Vector_T'Class;
      Index : Index_Type) return Element_Type
   is
   begin
      if (Index < Index_Type (This.Next_Index)) then
         return This.Data (Index);
      else
         raise Constraint_Error
           with "index check failed: " &
           Integer'Image (Integer (Index)) &
           " >= " &
           Integer'Image (Integer (Index_Type (This.Next_Index)));
      end if;
   end Element;

   function Last_Element (This : access Vector_T'Class) return Element_Type is
   begin
      if Index_Type'First /= This.Next_Index then
         return This.Data (This.Next_Index - 1);
      else
         raise Constraint_Error with "empty vector has no last element";
      end if;
   end Last_Element;

-- returns the first element in the vector or raises constraint error if empty
   function First_Element
     (This : access Vector_T'Class) return Element_Type is
     (This.Data (Index_Type'First));

   procedure Ensure_Index
     (This      : access Vector_T'Class;
      New_Index : Index_Type)
   is
      procedure Delete is new Ada.Unchecked_Deallocation
        (Element_Array_T,
         Element_Array_Access);
   begin
      if New_Index < This.Data'Last then
         return;
      end if;

      declare
         New_Size : Index_Type'Base := 2 * This.Data'Length;
      begin
         if New_Index - Index_Type'First > New_Size then
            New_Size := 1 + New_Index - Index_Type'First;
         end if;
         New_Size := New_Size + Index_Type'First;

         declare
            New_Container : Element_Array :=
              new Element_Array_T (Index_Type'First .. New_Size);

            D : Element_Array_Access := Element_Array_Access (This.Data);
         begin
            New_Container (Index_Type'First .. This.Data'Last) :=
              This.Data (Index_Type'First .. This.Data'Last);
            This.Data := New_Container;
            Delete (D);
         end;
      end;
   end Ensure_Index;

   procedure Ensure_Allocation
     (This      : access Vector_T'Class;
      New_Index : Index_Type)
   is
   begin
      This.Ensure_Index (New_Index);
      This.Next_Index := Index_Base (New_Index + 1);
   end Ensure_Allocation;

   function Length (This : access Vector_T'Class) return Natural is
   begin
      return Natural (Index_Type (This.Next_Index) - Index_Type'First);
   end Length;

   function Is_Empty
     (This : access Vector_T'Class) return Boolean is
     (Index_Type'First = Index_Type (This.Next_Index));

   procedure Clear (This : access Vector_T'Class) is
   begin
      This.Next_Index := Index_Type'First;
   end Clear;

   procedure Replace_Element
     (This    : access Vector_T'Class;
      Index   : Index_Type;
      Element : Element_Type)
   is
   begin
      This.Data (Index) := Element;
   end Replace_Element;

   function Check_Index
     (This  : access Vector_T'Class;
      Index : Index_Type) return Boolean is
     (Index < Index_Type (This.Next_Index));

end Skill.Containers.Vectors;
