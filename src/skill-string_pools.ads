--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     string pool management                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

with Ada.Containers.Hashed_Sets;
with Interfaces;

with Skill.Equals;
with Skill.Hashes;
with Skill.Streams.Reader;
with Skill.Streams.Writer;
with Skill.Types;
with Skill.Containers.Vectors;
with Skill.Synchronization;
with Ada.Exceptions;
with Ada.Characters.Latin_1;
with Ada.Containers.Vectors;
limited with Skill.Field_Types.Builtin.String_Type_P;

package Skill.String_Pools is

   type Pool_T is tagged limited private;
   type Pool is not null access Pool_T;

   -- internal use only
   function Create (Input : Skill.Streams.Reader.Input_Stream) return Pool;
   procedure Free (This : access Pool_T);

   function Size (This : access Pool_T) return Integer;

   -- internal use only
   procedure AddPosition
     (This : access Pool_T;
      Pos  : Types.v64;
      Len  : Types.i32);

   function Get
     (This  : access Pool_T;
      Index : Types.v64) return Skill.Types.String_Access;

   -- adds a string to the pool
   procedure Add (This : access Pool_T; S : Types.String_Access);
   -- adds a string to the pool
   function Add (This : access Pool_T; S : String) return Types.String_Access;

   -- internal use only
   function InvalidPoolIndexException
     (Idx       : Natural;
      Size      : Natural;
      Type_Name : String;
      Cause     : Ada.Exceptions.Exception_Occurrence) return String is
     ("Invalid index " &
      Natural'Image (Idx) &
      " into pool " &
      Type_Name &
      " of size " &
      Natural'Image (Size) &
      ". Caused by: " &
      Ada.Characters.Latin_1.LF &
      Ada.Exceptions.Exception_Information (Cause));

   -- internal use only
   procedure Prepare_And_Write
     (This              : access Pool_T;
      Output            : Skill.Streams.Writer.Output_Stream;
      Serialization_IDs : Skill.Field_Types.Builtin.String_Type_P.ID_Map);

   -- internal use only
   procedure Prepare_And_Append
     (This              : access Pool_T;
      Output            : Skill.Streams.Writer.Output_Stream;
      Serialization_IDs : Skill.Field_Types.Builtin.String_Type_P.ID_Map);
private

   use type Skill.Types.String_Access;
   package A1 is new Ada.Containers.Hashed_Sets
     (Skill.Types.String_Access,
      Skill.Hashes.Hash,
      Skill.Equals.Equals);

   use type Interfaces.Integer_32;
   use type Interfaces.Integer_64;

   type Position is record
      AbsoluteOffset : Types.v64 := -1;
      Length         : Types.i32 := -1;
   end record;

   package A2 is new Skill.Containers.Vectors (Natural, Position);
   package A3 is new Skill.Containers.Vectors
     (Natural,
      Skill.Types.String_Access);

   type Pool_T is tagged limited record
      Input            : Skill.Streams.Reader.Input_Stream;
      New_Strings      : A1.Set;
      String_Positions : A2.Vector;
      Id_Map           : A3.Vector;
      Mutex            : Skill.Synchronization.Mutex;
   end record;

end Skill.String_Pools;
