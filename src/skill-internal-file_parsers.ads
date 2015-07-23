--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file parser implementation                          --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Streams;
with Skill.Files;
with Skill.Types;
with Skill.String_Pools;
with Skill.Types.Pools;
with Skill.Field_Types;
with Skill.Streams.Reader;

-- documentation can be found in java common
package Skill.Internal.File_Parsers is

   generic
      type Result_T is new Skill.Files.File_T with private;
      type Result is access Result_T;

      with function New_Pool
        (Type_ID : Natural;
         Name    : Skill.Types.String_Access;
         Super   : Skill.Types.Pools.Pool) return Skill.Types.Pools.Pool is <>;

      with function Make_State
        (Path          : Types.String_Access;
         Mode          : Files.Write_Mode;
         Strings       : String_Pools.Pool;
         Types         : Files.Type_Vector;
         Types_By_Name : Files.Type_Map) return Result is <>;

      -- type factories are generic arguments because of the way that Ada works
      with function Constant_Length_Array
        (Length : Types.v64;
         Base_T : Skill.Field_Types.Field_Type)
         return Skill.Field_Types.Field_Type is <>;
      with function Variable_Length_Array
        (Base_T : Skill.Field_Types.Field_Type)
         return Skill.Field_Types.Field_Type is <>;
      with function List_Type
        (Base_T : Skill.Field_Types.Field_Type)
         return Skill.Field_Types.Field_Type is <>;
      with function Set_Type
        (Base_T : Skill.Field_Types.Field_Type)
         return Skill.Field_Types.Field_Type is <>;
      with function Map_Type
        (Key_T   : Skill.Field_Types.Field_Type;
         Value_T : Skill.Field_Types.Field_Type)
         return Skill.Field_Types.Field_Type is <>;
   function Read
     (Input : Skill.Streams.Reader.Input_Stream;
      Mode  : Skill.Files.Write_Mode) return Result;
end Skill.Internal.File_Parsers;
