with Skill.String_Pools;
--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file writer implementation                          --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

-- documentation can be found in java common
-- this is a combination of serialization functions, write and append
package body Skill.Internal.File_Writers is

   -- write a file to disk
   procedure Write
     (State  : Skill.Files.File;
      Output : Skill.Streams.Writer.Output_Stream)
   is
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
      begin
         null;
         -- TODO
--          for (StoragePool<?, ?> p : state.types) {
--              strings.add(p.name);
--              for (FieldDeclaration<?, ?> f : p.dataFields) {
--
--                  strings.add(f.name);
--                  if (f.type instanceof StringType) {
--                      for (SkillObject i : p)
--                          strings.add((String) i.get(f));
--                  }
--              }
         --          }

      end;


      ------------------------------
      -- PHASE 2: Check & Reorder --
      ------------------------------

      --  check consistency of the state, now that we aggregated all instances
      -- TODO State.Check;

--          stringIDs = state.stringType.resetIDs();

--                // make lbpo map, update data map to contain dynamic instances and create skill IDs for serialization
--          // index → bpo
--        // @note pools.par would not be possible if it were an actual map:)
--          final int[] lbpoMap = new int[state.types.size()];
--          state.types.stream().parallel().forEach(p -> {
--              if (p instanceof BasePool<?>) {
--                  makeLBPOMap(p, lbpoMap, 0);
--                  ((BasePool<?>) p).compress(lbpoMap);
--                  p.fixed(true);
--              }
--          });
--

      --------------------
      -- PHASE 3: Write --
      --------------------

      --  write string block
      State.Strings.Prepare_And_Write (Output);

--
--          // write count of the type block
--          out.v64(state.types.size());
--
--          // calculate offsets
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
--          // write types
--          ArrayList<FieldDeclaration<?, ?>> fieldQueue = new ArrayList<>();
--          for (StoragePool<?, ?> p : state.types) {
--              string(p.name, out);
--              long LCount = p.blocks.getLast().count;
--              out.v64(LCount);
--              restrictions(p, out);
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
--          }
--
--          // write fields
--          ArrayList<Task> data = new ArrayList<>();
--          long offset = 0L;
--          for (FieldDeclaration<?, ?> f : fieldQueue) {
--              StoragePool<?, ?> p = f.owner;
--              HashMap<FieldDeclaration<?, ?>, Future<Long>> vs = offsets.get(p);
--
--              // write info
--              out.v64(f.index);
--              string(f.name, out);
--              writeType(f.type, out);
--              restrictions(f, out);
--              long end = offset + vs.get(f).get();
--              out.v64(end);
--
--              // update chunks and prepare write data
--              f.dataChunks.clear();
--              f.dataChunks.add(new ChunkEntry(new BulkChunk(offset, end, p.size())));
--              data.add(new Task(f, offset, end));
--              offset = end;
--          }
--
--          writeFieldData(state, out, data);

--  protected final static void writeFieldData(SkillState state, FileOutputStream out, ArrayList<Task> data)
--              throws IOException, InterruptedException {
--
--          final Semaphore barrier = new Semaphore(0);
--          // async reads will post their errors in this queue
--          final ConcurrentLinkedQueue<SkillException> writeErrors = new ConcurrentLinkedQueue<SkillException>();
--
--          long baseOffset = out.position();
--          for (Task t : data) {
--              final FieldDeclaration<?, ?> f = t.f;
--              final MappedOutStream outMap = out.map(baseOffset, t.begin, t.end);
--              // @note use semaphore instead of data.par, because map is not thread-safe
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
--                          // ensure that writer can terminate, errors will be printed to command line anyway, and we wont
--                          // be able to recover, because errors can only happen if the skill implementation itself is
--                          // broken
--                          barrier.release(1);
--                      }
--                  }
--              });
--          }
--          barrier.acquire(data.size());
--          out.close();
--
--          // report errors
--          for (SkillException e : writeErrors) {
--              e.printStackTrace();
--          }
--          if (!writeErrors.isEmpty())
--              throw writeErrors.peek();
--
--          /**
--           * **************** PHASE 4: CLEANING * ****************
--           */
--          // release data structures
--          state.stringType.clearIDs();
--          // unfix pools
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
