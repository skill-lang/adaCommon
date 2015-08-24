--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     implementation of builtin field types               --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

with Skill.Types;
with Skill.Hashes; use Skill.Hashes;
with Skill.Types.Pools;

package body Skill.Field_Types.Builtin is

   package body Plain_Types is

      procedure Write_Box
        (This : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream;
         Target : Types.Box) is
      begin
         Write_Single(Output, Unboxed(Target));
      end Write_Box;

   end Plain_Types;


   package body Annotation_Type_P is

   procedure Fix_Types (This : access Field_Type_T; Tbn : Skill.Types.Pools.Type_Map) is
      begin
         This.Types_By_Name := Tbn;
      end Fix_Types;



--      @Override
--      public SkillObject readSingleField(InStream in) {
--          final int t = (int) in.v64();
--          final long f = in.v64();
--          if (0 == t)
--              return null;
--          return types.get(t - 1).getByID(f);
--      }
--
--      @Override
--      public long calculateOffset(Collection<SkillObject> xs) {
--          long result = 0L;
--          for (SkillObject ref : xs) {
--              if (null == ref)
--                  result += 2;
--              else {
--                  if (ref instanceof NamedType)
--                      result += V64.singleV64Offset(((NamedType) ref).τPool().typeID() - 31);
--                 else
--                      result += V64
--                              .singleV64Offset(typeByName.get(ref.getClass().getSimpleName().toLowerCase()).typeID() - 31);
--
--                  result += V64.singleV64Offset(ref.getSkillID());
--              }
--          }
--
--          return result;
--      }
--
--      /**
--       * used for simple offset calculation
--       */
--      public long singleOffset(SkillObject ref) {
--          if (null == ref)
--              return 2L;
--
--          final long name;
--          if (ref instanceof NamedType)
--              name = V64.singleV64Offset(((NamedType) ref).τPool().typeID() - 31);
--         else
--              name = V64.singleV64Offset(typeByName.get(ref.getClass().getSimpleName().toLowerCase()).typeID() - 31);
--
--          return name + V64.singleV64Offset(ref.getSkillID());
--      }
--
--      @Override
--      public void writeSingleField(SkillObject ref, OutStream out) throws IOException {
--          if (null == ref) {
--              // magic trick!
--              out.i16((short) 0);
--              return;
--          }
--
--          if (ref instanceof NamedType)
--              out.v64(((NamedType) ref).τPool().typeID() - 31);
--         else
--              out.v64(typeByName.get(ref.getClass().getSimpleName().toLowerCase()).typeID() - 31);
--          out.v64(ref.getSkillID());
--
--      }

   end Annotation_Type_P;

   package body String_Type_T is

      function Get_Id_Map (THis : access Field_Type_T) return ID_Map is
      Begin
         return This.String_IDs'access;
      end Get_Id_Map;

      procedure Write_Box
        (This : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream; Target : Types.Box) is
         V      : Types.String_Access := Unboxed(Target);
         use type Types.String_Access;
      begin
         if null = V then
            Output.I8(0);
         else
            Output.V64 (Types.V64 (THis.String_IDs.Element (V)));
         end if;
      end Write_Box;

      procedure Write_Single_Field
        (THis   : access Field_Type_T;
         V      : Types.String_Access;
         Output : Skill.Streams.Writer.Sub_Stream)
      is
         use type Types.String_Access;
      begin
         if null = V then
            Output.I8(0);
         else
            Output.V64 (Types.V64 (THis.String_IDs.Element (V)));
         end if;
      end Write_Single_Field;

   end String_Type_T;

end Skill.Field_Types.Builtin;
