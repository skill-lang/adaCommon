--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     implementation of builtin field types               --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

with Skill.Types;
with Skill.Hashes; use Skill.Hashes;
with Skill.Types.Pools;
with Ada.Tags;

package body Skill.Field_Types.Constant_Types is
   pragma Warnings (Off);

   function Read_Box
     (This : access Field_Type;
      Input : Streams.Reader.Sub_Stream) return Types.Box is

      function Boxed is new Ada.Unchecked_Conversion(Types.Annotation, Types.Box);
   begin
      raise Constraint_Error with "can not read a constant!";
      return Boxed(null);
   end Read_Box;

end Skill.Field_Types.Constant_Types;
