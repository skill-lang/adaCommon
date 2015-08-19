--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file writer implementation                          --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
with Skill.String_Pools;
with Skill.Types.Pools;
with Skill.Field_Types.Builtin;
with Skill.Types.Vectors;
with Skill.Field_Declarations; use Skill.Field_Declarations;
with Interfaces;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with Skill.Internal.Parts;
with Skill.Streams.Reader;

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
      String_Type : Skill.Field_Types.Builtin.String_Type_T.Field_Type :=
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
      Lbpo_Map : Lbpo_Map_T (0 .. State.Types.Length - 1);
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
         begin
            Strings.Add (This.Skill_Name);
            --              for (FieldDeclaration<?, ?> f : p.dataFields) {
            --
            --                  strings.add(f.name);
            --                  if (f.type instanceof StringType) {
            --                      for (SkillObject i : p)
            --                          strings.add((String) i.get(f));
            --                  }
            --              }

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
            if This.Dynamic.all in Types.Pools.Base_Pool_T'Class then
               R := Make_LBPO_Map (This, Lbpo_Map, 0);
               Cast (This).Compress (Lbpo_Map);
--                  p.fixed(true);
            end if;
         end Make;
      begin
         State.Types.Foreach (Make'Access);
      end;

      --------------------
      -- PHASE 3: Write --
      --------------------

      --  write string block
      State.Strings.Prepare_And_Write
      (Output, State.String_Type.String_IDs'Access);

      -- write count of the type block
      Output.V64 (Types.v64 (State.Types.Length));

--        -- Calculate Offsets
--          HashMap<StoragePool<?, ?>, HashMap<FieldDeclaration<?, ?>, Future<Long>>> offsets = new HashMap<>();
--          for (final StoragePool<?, ?> p : state.types) {
--              HashMap<FieldDeclaration<?, ?>, Future<Long>> vs = new HashMap<>();
--              for (final FieldDeclaration<?, ?> f : p.dataFields)
--                  vs.put(f, SkillState.pool.submit(new Callable<Long>() {
--                      @Override
--                      public Long call() throws Exception {
--                          return f.offset(p.blocks.getLast());
--                      }
--                  }));
--              offsets.put(p, vs);
--          }
--

      -- write types
      declare
         use type Types.Pools.Pool;
         use type Interfaces.Integer_64;

         package A1 renames Field_Declarations.Field_Vector_P;
         Field_Queue : A1.Vector := A1.Empty_Vector;

         procedure Write_Type (This : Types.Pools.Pool) is

            Lcount : Types.v64 := This.Blocks.Last_Element.Count;
--              if (null == p.superPool)
--                  out.i8((byte) 0);
--              else {
--                  out.v64(p.superPool.typeID - 31);
--                  if (0L != LCount)
--                      out.v64(lbpoMap[p.typeID - 32]);
--              }
--
--              out.v64(p.dataFields.size());
--              fieldQueue.addAll(p.dataFields);
         begin
            String (This.Skill_Name);
            Output.V64 (Lcount);
            Restrictions (This);
            if null = This.Super then
               Output.I8 (0);
            else
               Output.V64 (Types.v64 (This.Super.ID - 31));
               if 0 /= Lcount then
                  -- TODO
                  Output.V64 (-1);
               end if;
            end if;

            Output.V64 (Types.v64 (This.Data_Fields.Length));
            Field_Queue.Append_All (This.Data_Fields);
         end Write_Type;

      begin
         State.Types.Foreach (Write_Type'Access);

         -- write fields
         declare
--          ArrayList<Task> data = new ArrayList<>();
            Offset : Types.v64 := 0;

            procedure Write_Field (F : Field_Declaration) is
               P : Types.Pools.Pool := F.Owner.To_Pool;
--              HashMap<FieldDeclaration<?, ?>, Future<Long>> vs = offsets.get(p);
            begin
               -- write info
               Output.V64 (Types.v64 (F.Index));
               String (F.Name);
               Write_Type (F.T);
               Restrictions (F);
               -- TODO
               --                 Long end = Offset + Vs.Get(F).Get();
               --              out.V64(end);
               Output.V64 (0);

               -- update chunks and prepare write data
               F.Data_Chunks.Clear;
               F.Data_Chunks.Append
               (new Chunk_Entry_T'
                  (C =>
                     new Skill.Internal.Parts.Bulk_Chunk'
                       (Offset, 0, Types.v64 (P.Size)),
                   Input => Skill.Streams.Reader.Empty_Sub_Stream));
               --              data.add(new Task(f, offset, end));
               --              offset = end;
            end Write_Field;
         begin
            Field_Queue.Foreach (Write_Field'Access);
         end;
      end;
--
--          writeFieldData(state, out, data);

--  protected final static void writeFieldData(SkillState state, FileOutputStream out, ArrayList<Task> data)
--              throws IOException, InterruptedException {
--
--          final Semaphore barrier = new Semaphore(0);
--          -- async reads will post their errors in this queue
--          final ConcurrentLinkedQueue<SkillException> writeErrors = new ConcurrentLinkedQueue<SkillException>();
--
--          long baseOffset = out.position();
--          for (Task t : data) {
--              final FieldDeclaration<?, ?> f = t.f;
--              final MappedOutStream outMap = out.map(baseOffset, t.begin, t.end);
--              -- @note use semaphore instead of data.par, because map is not thread-safe
--              SkillState.pool.execute(new Runnable() {
--
--                  @Override
--                  public void run() {
--                      try {
--                          f.write(outMap);
--                      } catch (SkillException e) {
--                          writeErrors.add(e);
--                      } catch (IOException e) {
--                          writeErrors.add(new SkillException("failed to write field " + f.toString(), e));
--                      } catch (Throwable e) {
--                          writeErrors.add(new SkillException("unexpected failure while writing field " + f.toString(), e));
--                      } finally {
--                          -- ensure that writer can terminate, errors will be printed to command line anyway, and we wont
--                          -- be able to recover, because errors can only happen if the skill implementation itself is
--                          -- broken
--                          barrier.release(1);
--                      }
--                  }
--              });
--          }
--          barrier.acquire(data.size());
      Output.Close;
--
--          -- report errors
--          for (SkillException e : writeErrors) {
--              e.printStackTrace();
--          }
--          if (!writeErrors.isEmpty())
--              throw writeErrors.peek();
--
--          /**
--           * **************** PHASE 4: CLEANING * ****************
--           */
--          -- release data structures
--          state.stringType.clearIDs();
--          -- unfix pools
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
