--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     memory manager for ada ported from c++ skill        --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;
with Ada.Unchecked_Deallocation;

package body Skill.Books is

   function Make_Page
     (This          : access Book'Class;
      Expected_Size : Natural) return Page
   is
   begin
      if 0 /= This.Current_Remaining then
         raise Constraint_Error with "make_page contract violated";
      end if;
      This.Current_Page := new P (0 .. Expected_Size - 1);
      This.Pages.Append (This.Current_Page);
      return This.Current_Page;
   end Make_Page;

   procedure Free (This : access Book'Class) is
      procedure Delete is new Ada.Unchecked_Deallocation (P, Page);
      Ref : Page;

   begin
      for I in 0 .. This.Pages.Length - 1 loop
         Ref := This.Pages.Element (I);
         Delete (Ref);
      end loop;
   end Free;

   function Next (This : access Book'Class) return T_Access is
   begin
      -- first we try to take from current page
      if This.Current_Remaining > 0 then
         This.Current_Remaining := This.Current_Remaining - 1;
         return This.Current_Page
             (Default_Page_Size - This.Current_Remaining)'
             Access;

      elsif This.Freelist.Length > 0 then
         -- Deplete freelist before allocating a new page
         return This.Freelist.Pop;
      else
         -- we have to allocate a new page
         This.Current_Page := new P (1 .. Default_Page_Size);
         This.Pages.Append (This.Current_Page);
         -- return first object
         This.Current_Remaining := Default_Page_Size - 1;
         return This.Current_Page (1)'Access;
      end if;
   end Next;

   procedure free (This : access Book'Class; Target : T_Access) is
   begin
      This.Freelist.Append (Target);
   end free;

end Skill.Books;
