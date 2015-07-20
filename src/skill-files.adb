--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     general file interaction                            --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Errors;
with Skill.Internal.File_Parsers;
with Skill.Streams;
with Skill.Types;

package body Skill.Files is

   package FileParser renames Skill.Internal.File_Parsers;

   function Open
     (Path    : String;
      Read_M  : Read_Mode  := Read;
      Write_M : Write_Mode := Write) return File
   is
   begin
      case Read_M is

         when Read =>
            return FileParser.Read
                (Skill.Streams.Input (new String'(Path)),
                 Write_M);

         when Create =>
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

   function Strings (This : access File_T) return Skill.String_Pools.Pool is
   begin
      return This.Strings;
   end Strings;

   function Finish_Allocation
     (Path          : Skill.Types.String_Access;
      Mode          : Write_Mode;
      Strings       : Skill.String_Pools.Pool;
      Types         : Type_Vector;
      Types_By_Name : Type_Map) return File
   is
   begin
      return new File_T'
          (Path          => Path,
           Mode          => Mode,
           Strings       => Strings,
           Types         => Types,
           Types_By_Name => Types_By_Name);
   end Finish_Allocation;

end Skill.Files;
