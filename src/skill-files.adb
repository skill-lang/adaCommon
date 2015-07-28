--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     general file interaction                            --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
with Ada.Unchecked_Conversion;

with Skill.Errors;
with Skill.Internal.File_Parsers;
with Skill.Streams;
with Skill.Types;
with Skill.Synchronization;
with Skill.Types.Pools;
with Skill.Field_Declarations;
with Skill.Tasks;

package body Skill.Files is

   package FileParser renames Skill.Internal.File_Parsers;

   function Strings
     (This : access File_T'Class) return Skill.String_Pools.Pool
   is
   begin
      return This.Strings;
   end Strings;

   procedure Finalize_Pools (This : access File_T'Class) is
      Read_Barrier : Skill.Synchronization.Barrier;

      procedure Finish (F : Skill.Field_Declarations.Field_Declaration) is
         procedure Read_Chunk (CE : Skill.Field_Declarations.Chunk_Entry) is

            procedure Read is
            begin
               F.Read (CE);

               -- TODO error reporting? (see Java Field.finish)

               Read_Barrier.Complete;
            end Read;

            T : Skill.Tasks.Run (Read'Access);
         begin
            Read_Barrier.Start;

            T.Start;
         end Read_Chunk;
      begin
         F.Data_Chunks.Foreach (Read_Chunk'Access);
      end Finish;

      procedure Start_Read (P_Static : Skill.Types.Pools.Pool) is
         function Base is new Ada.Unchecked_Conversion
           (Skill.Types.Pools.Pool_Dyn,
            Skill.Types.Pools.Base_Pool);

         P : Skill.Types.Pools.Pool_Dyn := P_Static.Dynamic;
      begin
         -- note: this loop must happen in type order!

         -- set owner
         if P.all in Skill.Types.Pools.Base_Pool_T'Class then
            Base (P).Set_Owner (This);
         end if;

         -- add missing field declarations
--              HashSet<String> fieldNames = new HashSet<>();
--              for (de.ust.skill.common.java.api.FieldDeclaration<?> f : p.dataFields)
--                  fieldNames.add(f.name());

         -- ensure existence of known fields
--              for (String n : p.knownFields) {
--                  if (!fieldNames.contains(n))
--                      p.addKnownField(n, stringType, annotationType);
--              }

         -- read known fields
         P.Data_Fields.Foreach (Finish'Access);
         null;
      end Start_Read;
   begin
      This.Types.Foreach (Start_Read'Access);

-- fix types in the Annotation-runtime type, because we need it in offset calculation
-- TODO         this.annotationType.fixTypes(this.poolByName());

      -- await async reads
      Read_Barrier.Await;
   end Finalize_Pools;

end Skill.Files;
