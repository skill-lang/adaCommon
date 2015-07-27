--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     equals used in skill                                --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers;
with Ada.Strings.Hash;
with Skill.Types;
with Skill.Types.Pools;

-- the trick of this package is to instantiate equals codes as Skill.Equals.equals
-- independent of the type! :)
package Skill.Equals is
--     pragma Preelaborate;

   use type Skill.Types.String_Access;

   function Equals
     (A, B : Skill.Types.String_Access) return Boolean is
     (A = B
      or else ((null /= A and null /= B) and then A.all = B.all)
     );

   use type Skill.Types.Pools.Pool;
   function Equals
     (A, B : Skill.Types.Pools.Pool) return Boolean is
     (A = B);

end Skill.Equals;
