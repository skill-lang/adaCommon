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

-- documentation can be found in java common
package Skill.Internal.File_Parsers is

   generic
      type Result_T is new Skill.Files.File_T with private;
      type Result is access Result_T;

      with function New_Pool (Type_ID : Natural;
                              Name : Skill.Types.String_Access;
                              Super : Skill.Types.Pools.Pool) return Skill.Types.Pools.Pool;
        with function Make_State (Path : Types.String_Access;
                                    Mode : Files.Write_Mode;
                                    Strings : String_Pools.Pool;
                                    Types    : Files.Type_Vector;
                                    Types_By_Name : Files.Type_Map) return Result;
   function Read
     (Input : Skill.Streams.Input_Stream;
      Mode  : Skill.Files.Write_Mode) return Result;
end Skill.Internal.File_Parsers;
