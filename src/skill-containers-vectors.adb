--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     skills vector container implementation              --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski, Timm Felden                  --
--                                                                            --

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
     (This : access Vector_T;
      F    : access procedure (I : Element_Type))
   is
   begin
      for I in Index_Type'First .. Index_Type (This.Next_Index) - 1 loop
         F (This.Data (I));
      end loop;
   end Foreach;

   procedure Append (This : access Vector_T; New_Element : Element_Type) is
   begin
      This.Ensure_Index (Index_Type (This.Next_Index));
      This.Append_Unsafe (New_Element);
   end Append;

   procedure Append_Unsafe
     (This        : access Vector_T;
      New_Element : Element_Type)
   is
   begin
      This.Data (Index_Type (This.Next_Index)) := New_Element;
      This.Next_Index                          := This.Next_Index + 1;
   end Append_Unsafe;

   procedure Append_All (This : access Vector_T; Other : Vector) is
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

   function Pop (This : access Vector_T) return Element_Type is
   begin
      This.Next_Index := This.Next_Index - 1;
      return This.Data (Index_Type (This.Next_Index));
   end Pop;

   function Element
     (This  : access Vector_T;
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

   function Last_Element (This : access Vector_T) return Element_Type is
   begin
      if Index_Type'First /= This.Next_Index then
         return This.Data (This.Next_Index - 1);
      else
         raise Constraint_Error with "empty vector has no last element";
      end if;
   end Last_Element;

   procedure Ensure_Index (This : access Vector_T; New_Index : Index_Type) is
   begin
      if New_Index < This.Data'Last then
         return;
      end if;

      declare
         New_Size : Index_Type'Base := 2 * This.Data'Length;
      begin
         while (New_Index - Index_Type'First > New_Size) loop
            New_Size := 2 * New_Size;
         end loop;
         New_Size := New_Size + Index_Type'First;

         declare
            New_Container : Element_Array :=
              new Element_Array_T (Index_Type'First .. New_Size);

            procedure Free is new Ada.Unchecked_Deallocation
              (Element_Array_T,
               Element_Array_Access);
            D : Element_Array_Access := Element_Array_Access (This.Data);
         begin
            New_Container (Index_Type'First .. This.Data'Last) :=
              This.Data (Index_Type'First .. This.Data'Last);
            This.Data := New_Container;
            Free (D);
         end;
      end;
   end Ensure_Index;

   procedure Ensure_Allocation
     (This      : access Vector_T;
      New_Index : Index_Type)
   is
   begin
      This.Ensure_Index (New_Index);
      This.Next_Index := Index_Base (New_Index + 1);
   end Ensure_Allocation;

   function Length (This : access Vector_T) return Natural is
   begin
      return Natural (Index_Type (This.Next_Index) - Index_Type'First);
   end Length;

   function Is_Empty
     (This : access Vector_T) return Boolean is
     (Index_Type'First = Index_Type (This.Next_Index));

   procedure Clear (This : access Vector_T) is
   begin
      This.Next_Index := Index_Type'First;
   end Clear;

   procedure Replace_Element
     (This    : access Vector_T;
      Index   : Index_Type;
      Element : Element_Type)
   is
   begin
      This.Data (Index) := Element;
   end Replace_Element;

   function Check_Index
     (This  : access Vector_T;
      Index : Index_Type) return Boolean is
     (Index < Index_Type (This.Next_Index));

end Skill.Containers.Vectors;
