--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     memory manager for ada ported from c++ skill        --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Containers.Vectors;

generic
   type T is private;
   type T_Access is access all T;
package Skill.Books is
   package Vec is new Skill.Containers.Vectors (Natural, T_Access);
   type P is array (Natural range <>) of aliased T;
   type Page is access P;
   package Pages_P is new Skill.Containers.Vectors (Natural, Page);

   Default_Page_Size : constant := 128;

   -- @see c++ for documentation
   type Book is tagged record
      Freelist : Vec.Vector := Vec.Empty_Vector;
      --! @invariant: if not current page then, T is used or T is in freeList
      Pages             : Pages_P.Vector := Pages_P.Empty_Vector;
      Current_Page      : Page           := null;
      Current_Remaining : Natural        := 0;
   end record;

   -- create a new page of expected size
   -- @pre current_remaining == 0
   -- @post current_remaining == 0
   -- @return current_page
   -- @note the caller has to use the whole page, as it is marked as used
   function Make_Page (This : access Book'Class; Expected_Size : Natural)
                       return Page;

   -- note won't free the book, but only the pages
   procedure Free (This : access Book'Class);

   -- can only be called directly after creation of the book
   function First_Page
     (This : access Book'Class) return Page is
     (This.Current_Page);

   -- return the next free instance
   --
   -- @note must never be called, while using the first page
   function Next (This : access Book'Class) return T_Access;

   -- recycle the argument instance
   procedure free (This : access Book'Class; Target : T_Access);

end Skill.Books;
