--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     hashes used in skill                                --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

with Ada.Containers;
with Ada.Strings.Hash;
with Skill.Types;
with Skill.Types.Pools;
with Ada.Unchecked_Conversion;
with Interfaces;

-- the trick of this package is to instantiate hash codes as Skill.hashes.hash
-- independent of the type! :)
package Skill.Hashes is
   --     pragma Preelaborate;
   pragma Warnings(Off);

   function Hash
     (Element : Skill.Types.String_Access) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (Element.all));

   function Hash
     (Element : Skill.Types.Pools.Pool) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type(Element.Id));

   function Hash
     (Element : Interfaces.Integer_32) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type(Element));

   function Hash is new Ada.Unchecked_Conversion(Interfaces.Integer_64, Ada.Containers.Hash_Type);

end Skill.Hashes;
