--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     !! remove after integration into generator !!       --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Types.Pools;
with Skill.Files;
with Skill.Streams;
with Skill.Errors;
with Skill.Internal.File_Parsers;
with Skill.String_Pools;

-- parametrization of file, read/write and pool code
package body Age.Api is

   use Skill;

   function New_Pool
     (N : Skill.Types.String_Access;
      P : Skill.Types.Pools.Pool) return Skill.Types.Pools.Pool
   is
   begin
      return null;
   end New_Pool;

   -- build a state from intermediate information
   function Make_State
     (Path          : Skill.Types.String_Access;
      Mode          : Skill.Files.Write_Mode;
      Strings       : Skill.String_Pools.Pool;
      Types         : Skill.Files.Type_Vector;
      Types_By_Name : Skill.Files.Type_Map) return File
   is
   begin
      return new File_T'(
                         Path          => Path,
         Mode          => Mode,
         Strings       => Strings,
         Types         => Types,
         Types_By_Name => Types_By_Name);
   end Make_State;

   function Read is new Skill.Internal.File_Parsers.Read
     (Result_T   => File_T,
      Result     => File,
      New_Pool   => New_Pool,
      Make_State => Make_State);

   function Open
     (Path    : String;
      Read_M  : Files.Read_Mode  := Skill.Files.Read;
      Write_M : Files.Write_Mode := Skill.Files.Write) return File
   is
   begin
      case Read_M is

         when Files.Read =>
            return Read (Skill.Streams.Input (new String'(Path)), Write_M);

         when Files.Create =>
            raise Skill.Errors.Skill_Error with "TBD";

            --          case Create:
            --              // initialization order of type information has to match file parser
            --              // and can not be done in place
            --              StringPool strings = new StringPool(null);
      --              ArrayList<StoragePool<?, ?>> types = new ArrayList<>(1);
            --              StringType stringType = new StringType(strings);
            --              Annotation annotation = new Annotation(types);
            --
            --              // create type information
            --              AgeAccess Age = new AgeAccess(0);
            --              types.add(Age);
            --              return new SkillState(strings, types, stringType, annotation, path, actualMode.close);
            --
      end case;
   end Open;

end Age.Api;
