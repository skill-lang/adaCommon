--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     memory manager for ada ported from c++ skill        --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Containers.Vectors;

generic
   type T is private;
package Skill.Books is
   package Vec is new Skill.Containers.Vectors(Natural, T);
   type P is array(Natural range <>) of T;
   type Page is access P;
   package Pages_P is new Skill.Containers.Vectors(Natural, Page);

   Default_Page_Size : constant := 128;

   -- @see c++ for documentation
   type Book is tagged record
      Freelist : Vec.Vector;
      --! @invariant: if not current page then, T is used or T is in freeList
      Pages : Pages_P.Vector;
      Current_Page : Page;
      Current_Remaining : Natural;
   end record;

   function Make(Expected_Size : Natural) return Book;


   --☢☢ Hier Weiter Functionen Portieren☢☢

end Skill.Books;
