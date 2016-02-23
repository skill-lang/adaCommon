--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     memory manager for ada ported from c++ skill        --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --


package body Skill.Books is

   function Make(Expected_Size : Natural) return Book is
     Rval : Book := Book'(Freelist          => Vec.Empty_Vector,
            Pages             => Pages_P.Empty_Vector,
            Current_Page      => new P(0..Expected_Size),
                          Current_Remaining => 0);
   begin
      Rval.Pages.Append(Rval.Current_Page);
      return Rval;
   end Make;



end Skill.Books;
