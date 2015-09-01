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
with Skill.Internal.File_Writers;
with Skill.Equals;
with Skill.Hashes;
with Ada.Containers.Hashed_Sets;
with Ada.Text_IO;

package body Skill.Files is

   package FileParser renames Skill.Internal.File_Parsers;

   -- read file closure
   type Cl is new Tasks.Closure_T with record
      F  : Skill.Field_Declarations.Field_Declaration;
      CE : Skill.Field_Declarations.Chunk_Entry;
   end record;
   type Cx is not null access Cl;
   function Cast is new Ada.Unchecked_Conversion (Skill.Tasks.Closure, Cx);

   function Strings
     (This : access File_T'Class) return Skill.String_Pools.Pool
   is
   begin
      return This.Strings;
   end Strings;

   procedure Change_Path (This : access File_T'Class; New_Path : String) is
   begin
      pragma Assert (This.Mode = Write);
      This.Path := new String'(New_Path);
   end Change_Path;

   procedure Flush (This : access File_T'Class) is
      type T is access all File_T;
      function Cast is new Ada.Unchecked_Conversion (T, File);
   begin
      case This.Mode is
         when Write =>
            Skill.Internal.File_Writers.Write
              (Cast (T (This)),
               Streams.Write (This.Path));

         when Append =>
            Skill.Internal.File_Writers.Append
              (Cast (T (This)),
               Streams.Append (This.Path));

         when Destroyed =>
            raise Skill.Errors.Skill_Error
              with "state gets destroyed after first flush. I could fix this if you gave me 1m$.";
      end case;

      This.Mode := Destroyed;
   end Flush;

   procedure Finalize_Pools (This : access File_T'Class) is
      Read_Barrier : Skill.Synchronization.Barrier;

      procedure Finish (F : Skill.Field_Declarations.Field_Declaration) is

         procedure Read (C : Skill.Tasks.Closure) is
         begin
            F.Read (Cast (C).CE);

            -- TODO error reporting? (see Java Field.finish)

            Read_Barrier.Complete;
         exception
            when E : others =>
               Read_Barrier.Complete;

               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Current_Error,
                  "A task crashed during read data: " & F.Name.all);

--                 Skill.Errors.Print_Stacktrace (E);

               return;
         end Read;

         procedure Read_Chunk (CE : Skill.Field_Declarations.Chunk_Entry) is

            T : Skill.Tasks.Run (Read'Access);

            C : Skill.Tasks.Closure := new Cl'(F => F, CE => CE);
         begin
            Read_Barrier.Start;

            T.Start (C);
         end Read_Chunk;
      begin
         F.Data_Chunks.Foreach (Read_Chunk'Access);
      end Finish;

      procedure Start_Read (P_Static : Skill.Types.Pools.Pool) is
         function Base is new Ada.Unchecked_Conversion
           (Skill.Types.Pools.Pool_Dyn,
            Skill.Types.Pools.Base_Pool);

         use type Types.String_Access;
         package A1 is new Ada.Containers.Hashed_Sets
           (Element_Type        => Types.String_Access,
            Hash                => Skill.Hashes.Hash,
            Equivalent_Elements => Skill.Equals.Equals,
            "="                 => "=");

         P           : Skill.Types.Pools.Pool_Dyn := P_Static.Dynamic;
         Field_Names : A1.Set;
      begin
         -- note: this loop must happen in type order!

         -- set owner
         if P.all in Skill.Types.Pools.Base_Pool_T'Class then
            Base (P).Set_Owner (This);
         end if;

         -- add missing field declarations
         Field_Names := A1.Empty_Set;
         for I in 1 .. Natural (P.Data_Fields.Length) loop
            Field_Names.Insert (P.Data_Fields.Element (I).Name);
         end loop;

         -- ensure existence of known fields
         for N of P.Known_Fields.all loop
            if not Field_Names.Contains (N) then
               P.Dynamic.Add_Known_Field
               (N, This.String_Type, This.Annotation_Type);
            end if;
         end loop;

         -- read known fields
         P.Data_Fields.Foreach (Finish'Access);
         null;
      end Start_Read;
   begin
      This.Types.Foreach (Start_Read'Access);

      -- fix types in the Annotation-runtime type, because we need it in offset calculation
      This.Annotation_Type.Fix_Types;

      -- await async reads
      Read_Barrier.Await;
   end Finalize_Pools;

end Skill.Files;
