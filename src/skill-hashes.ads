--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     hashes used in skill                                --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers;
with Ada.Strings.Hash;
with Skill.Types;
with Skill.Types.Pools;

-- the trick of this package is to instantiate hash codes as Skill.hashes.hash
-- independent of the type! :)
package Skill.Hashes is
--     pragma Preelaborate;

   function Hash
     (Element : Skill.Types.String_Access) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (Element.all));

   function Hash
     (Element : Skill.Types.Pools.Pool) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type(Element.Id));

end Skill.Hashes;
