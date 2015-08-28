--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file writer implementation                          --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
with Ada.Unchecked_Conversion;

with Interfaces;

with Skill.String_Pools;
with Skill.Types.Pools;
with Skill.Field_Types.Builtin;
with Skill.Field_Types.Builtin.String_Type_P;
with Skill.Containers.Vectors;
with Skill.Field_Declarations; use Skill.Field_Declarations;
with Skill.Internal.Parts;
with Skill.Streams.Reader;
with Skill.Synchronization;
with Skill.Tasks;
with Skill.Errors;
with Ada.Text_IO;

-- documentation can be found in java common
-- this is a combination of serialization functions, write and append
package body Skill.Internal.File_Writers is

   function Make_LBPO_Map
     (P        :        Types.Pools.Pool;
      Lbpo_Map : in out Lbpo_Map_T;
      Next     :        Integer) return Integer
   is
      Result : Integer := Next + P.Dynamic.Static_Size;

      procedure Children (sub : Types.Pools.Sub_Pool) is
      begin
         Result := Make_LBPO_Map (sub.To_Pool, Lbpo_Map, Result);
      end Children;
   begin
      Lbpo_Map (P.Pool_Offset) := Next;
      P.Sub_Pools.Foreach (Children'Access);
      return Result;
   end Make_LBPO_Map;

   -- write a file to disk
   procedure Write
     (State  : Skill.Files.File;
      Output : Skill.Streams.Writer.Output_Stream)
   is
      String_Type : Skill.Field_Types.Builtin.String_Type_P.Field_Type :=
        State.String_Type;

      procedure String (S : Types.String_Access) is
      begin
         Output.V64 (Types.v64 (String_Type.String_IDs.Element (S)));
      end String;

      procedure Write_Type (T : Field_Types.Field_Type) is
      begin
         Output.V64 (Types.v64 (T.ID));
      end Write_Type;

      procedure Restrictions (S : Types.Pools.Pool) is
      begin
         -- todo
         Output.V64 (0);
      end Restrictions;

      procedure Restrictions (F : Field_Declaration) is
      begin
         -- todo
         Output.V64 (0);
      end Restrictions;

      -- index → bpo
      --  @note pools.par would not be possible if it were an actual
      Lbpo_Map : Lbpo_Map_T (0 .. Natural(State.Types.Length) - 1);

      -- barrier used for parallel processing
      Write_Barrier : Skill.Synchronization.Barrier;
   begin
      ----------------------
      -- PHASE 1: Collect --
      ----------------------

   -- collect String instances from known string types; this is required,
   -- because we use plain strings
   -- @note this is a O(σ) operation:)
   -- @note we do not use generation time type info, because we want to treat
   -- generic fields as well
      declare
         Strings : Skill.String_Pools.Pool := State.Strings;


         procedure Add (This : Skill.Types.Pools.Pool) is

            procedure Add_Field(F : Field_Declarations.Field_Declaration) is

               procedure Add_String (I : Types.Annotation) is
               begin
                  Strings.Add
                    (Field_Types.Builtin.String_Type_P.Unboxed
                       (I.Dynamic.Reflective_Get(F)));
               end;
            begin
            Strings.Add (F.Name);
               -- add string data
               if F.T.ID = 14 then
                  This.Do_In_Type_Order(Add_String'Access);
               end if;
            end Add_Field;
         begin
            Strings.Add (This.Skill_Name);
            This.Data_Fields.Foreach(Add_Field'Access);
         end Add;
      begin
         State.Types.Foreach (Add'Access);
      end;

      ------------------------------
      -- PHASE 2: Check & Reorder --
      ------------------------------

      --  check consistency of the state, now that we aggregated all instances
      -- TODO State.Check;

      -- make lbpo map, update data map to contain dynamic instances and create skill IDs for serialization
      declare
         procedure Make (This : Types.Pools.Pool) is
            function Cast is new Ada.Unchecked_Conversion
              (Types.Pools.Pool,
               Types.Pools.Base_Pool);
            R : Integer;
         begin
            This.Fixed (True);
            if This.Dynamic.all in Types.Pools.Base_Pool_T'Class then
               R := Make_LBPO_Map (This, Lbpo_Map, 0);
               Cast (This).Compress (Lbpo_Map);
            end if;
         end Make;
      begin
         State.Types.Foreach (Make'Access);
      end;

      -- Calculate Offsets
      declare

         procedure Make (F : Field_Declaration) is
            procedure Calculate is
            begin
               F.Offset;
               Write_Barrier.Complete;
            exception
               when E : others =>
                  Skill.Errors.Print_Stacktrace (E);
                  Ada.Text_IO.Put_Line(Ada.Text_IO.Current_Error, "A task crashed during offset calculation: " & F.Name.all);
                  Write_Barrier.Complete;
            end Calculate;
            T : Skill.Tasks.Run (Calculate'Access);
         begin
            Write_Barrier.Start;
            T.Start;
         end Make;

         procedure Off (This : Types.Pools.Pool) is
         begin
            This.Data_Fields.Foreach (Make'Access);
         end Off;
      begin
         State.Types.Foreach (Off'Access);
      end;

      --------------------
      -- PHASE 3: Write --
      --------------------

      --  write string block
      State.Strings.Prepare_And_Write
      (Output, State.String_Type.String_IDs'Access);

      -- write count of the type block
      Output.V64 (Types.v64 (State.Types.Length));

      -- write types
      declare
         use type Types.Pools.Pool;
         use type Interfaces.Integer_64;

         package A1 renames Field_Declarations.Field_Vector_P;
         Field_Queue : A1.Vector := A1.Empty_Vector;

         procedure Write_Type (This : Types.Pools.Pool) is

            Lcount : Types.v64 := This.Blocks.Last_Element.Count;

         begin
            String (This.Skill_Name);
            Output.V64 (Lcount);
            Restrictions (This);
            if null = This.Super then
               Output.I8 (0);
            else
               Output.V64 (Types.v64 (This.Super.ID + 1));
               if 0 /= Lcount then
                  Output.V64 (Types.v64 (Lbpo_Map (This.Pool_Offset)));
               end if;
            end if;

            Output.V64 (Types.v64 (This.Data_Fields.Length));
            Field_Queue.Append_All (This.Data_Fields);
         end Write_Type;

      begin
         State.Types.Foreach (Write_Type'Access);

         -- await offsets before we can write fields
         Write_Barrier.Await;

         -- write fields
         declare
--          ArrayList<Task> data = new ArrayList<>();
            Ende, Offset : Types.v64 := 0;

            procedure Write_Field (F : Field_Declaration) is
               P : Types.Pools.Pool := F.Owner.To_Pool;
            begin
               -- write info
               Output.V64 (Types.v64 (F.Index));
               String (F.Name);
               Write_Type (F.T);
               Restrictions (F);
               Ende := Offset + F.Future_Offset;
               Output.V64 (Ende);

               -- update chunks and prepare write data
               F.Data_Chunks.Clear;
               F.Data_Chunks.Append
               (new Chunk_Entry_T'
                  (C =>
                     new Skill.Internal.Parts.Bulk_Chunk'
                       (Offset, 0, Types.v64 (P.Size), 1),
                   Input => Skill.Streams.Reader.Empty_Sub_Stream));

               --              data.add(new Task(f, offset, end));
               Offset := Ende;
            end Write_Field;
         begin
            Field_Queue.Foreach (Write_Field'Access);
         end;

         -- write field data
         declare
            procedure Write_Field (F : Field_Declarations.Field_Declaration) is

               Map : Streams.Writer.Sub_Stream := Output.Map (F.Future_Offset);

               procedure Job is
               begin
                  F.Write (Map);
                  Map.Close;
                  Write_Barrier.Complete;
            exception
               when E : others =>
                  Skill.Errors.Print_Stacktrace (E);
                  Ada.Text_IO.Put_Line(Ada.Text_IO.Current_Error, "A task crashed during write data: " & F.Name.all);
                  Write_Barrier.Complete;
               end Job;

               T : Skill.Tasks.Run (Job'Access);
            begin
               Write_Barrier.Start;
               T.Start;
            end Write_Field;
         begin
            Field_Queue.Foreach (Write_Field'Access);

            -- await writing of actual field data
            Write_Barrier.Await;
         end;
      end;

      -- we are done
      Output.Close;

      -----------------------
      -- PHASE 4: Cleaning --
      -----------------------

      -- release data structures
      State.String_Type.String_IDs.Clear;
      -- unfix pools
--          for (StoragePool<?, ?> p : state.types) {
--              p.fixed(false);
--          }
--      }
   end Write;

   -- append a file to an existing one
   procedure Append
     (State  : Skill.Files.File;
      Output : Skill.Streams.Writer.Output_Stream)
   is
   begin
      null;
   end Append;

end Skill.Internal.File_Writers;
