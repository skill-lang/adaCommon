--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Your SKilL Scala Binding                            --
-- \__ \ ' <| | | |__     <<debug>>                                           --
-- |___/_|\_\_|_|____|    by: <<some developer>>                              --
--                                                                            --

with Skill.Types;

-- skill names used to represent types
-- ensures rather fast string comparisons
package Age.Internal_Skill_Names is
   pragma Preelaborate;

   Age_Skill_Name : not null Skill.Types.String_Access :=
                           new String'("age");

end Age.Internal_Skill_Names;
