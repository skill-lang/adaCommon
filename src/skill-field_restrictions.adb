--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     runtime field restriction handling                  --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

package body Skill.Field_Restrictions is

   The_Nonnull : aliased Nonnull_T;

   function Nonnull return Base is
     (The_Nonnull'Access);

   The_Constant_Length_Pointer : aliased Constant_Length_Pointer_T;

   function Constant_Length_Pointer return Base is
      (The_Constant_Length_Pointer'Access);

end Skill.Field_Restrictions;
