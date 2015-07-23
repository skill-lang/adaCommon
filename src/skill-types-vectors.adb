--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     skills vector container implementation              --
-- |___/_|\_\_|_|____|    by: Dennis Przytarski                               --
--                                                                            --

with Ada.Unchecked_Deallocation;

package body Skill.Types.Vectors is

   procedure Append (Container : in out Vector; New_Element : Element_Type) is
   begin
      Container.Ensure_Size (1 + Container.Size_0);
      Container.Append_Unsafe (New_Element);
   end Append;

   procedure Append_Unsafe
     (Container   : in out Vector;
      New_Element :        Element_Type)
   is
   begin
      Container.Size_0                      := 1 + Container.Size_0;
      Container.Elements (Container.Size_0) := New_Element;
   end Append_Unsafe;

   function Pop (Container : in out Vector) return Element_Type is
   begin
      Container.Size_0 := Container.Size_0 - 1;
      return Container.Elements (Container.Size_0 + 1);
   end Pop;

   function Element
     (Container : in out Vector;
      Index     :        Index_Type) return Element_Type
   is
   begin
      if (Index <= Container.Size_0) then
         return Container.Elements (Index);
      else
         raise Constraint_Error
           with "index check failed: " &
           Integer'Image (Integer (Index)) &
           " >= " &
           Integer'Image (Integer (Container.Size_0));
      end if;
   end Element;

   procedure Ensure_Size (Container : in out Vector; N : Index_Type) is
   begin
      if (N > Container.Elements'Length) then
         declare
            New_Size : Index_Type := 2 * Container.Size;
         begin
            while (N > New_Size) loop
               New_Size := 2 * New_Size;
            end loop;

            declare
               New_Container : Element_Array_Access :=
                 new Element_Array (1 .. New_Size);
               procedure Free is new Ada.Unchecked_Deallocation
                 (Element_Array,
                  Element_Array_Access);
            begin
               New_Container (1 .. Container.Size) :=
                 Container.Elements (1 .. Container.Size);
               Free (Container.Elements);
               Container.Elements := New_Container;
            end;

            Container.Size := New_Size;
         end;
      end if;
   end Ensure_Size;

   procedure Ensure_Allocation (Container : in out Vector; N : Index_Type) is
   begin
      Container.Ensure_Size (Container.Size_0 + N);
      Container.Size_0 := N + Container.Size_0;
   end Ensure_Allocation;

   function Length (Container : in Vector) return Index_Type is
   begin
      return Container.Size_0;
   end Length;

   function Is_Empty
     (Container : in Vector) return Boolean is
     (0 = Container.Size_0);

   procedure Clear (Container : in out Vector) is
   begin
      Container.Size_0 := 0;
   end Clear;

   procedure Replace_Element
     (Container : in out Vector;
      Index     :        Index_Type;
      Element   :        Element_Type)
   is
   begin
      Container.Elements (Index) := Element;
   end Replace_Element;

   function Check_Index
     (Container : in out Vector;
      Index     :        Index_Type) return Boolean is
     (Index <= Container.Size_0);

   overriding procedure Initialize (Object : in out Vector) is
   begin
      Object.Elements := new Element_Array (1 .. Object.Size);
   end Initialize;

   overriding procedure Finalize (Object : in out Vector) is
   begin
      null;
   end Finalize;

end Skill.Types.Vectors;
