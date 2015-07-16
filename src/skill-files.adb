--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     general file interaction                            --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Errors;
with Skill.Internal.File_Parsers;
with Skill.Streams;

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
            return FileParser.Read (Skill.Streams.Input (new String'(Path)), Write_M);

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

end Skill.Files;
