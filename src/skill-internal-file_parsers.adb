--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file parser implementation                          --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Containers.Vectors;
with Skill.Files;
with Skill.Types;
with Skill.Streams.Reader;
with Ada.Text_IO;
with Interfaces;
with Skill.Errors;
with Ada.Unchecked_Conversion;
with Skill.String_Pools;
with Skill.Types.Pools;
with Ada.Characters.Latin_1;

-- documentation can be found in java common
package body Skill.Internal.File_Parsers is

   use Skill;
   use type Types.String_Access;
   use type Interfaces.Integer_32;
   use type Interfaces.Integer_64;

   procedure Print (I : Types.i8) is
   begin
      Ada.Text_IO.Put_Line (Integer'Image (Integer (I)));
   end Print;

   -- TODO parametrization
   function Read
     (Input : Streams.Input_Stream;
      Mode  : Files.Write_Mode) return Skill.Files.File
   is
      -- begin error reporting
      Block_Counter : Positive := 1;
      -- end error reporting

      Strings       : String_Pools.Pool := String_Pools.Create (Input);
      Type_Vector   : Files.Type_Vector := new Files.P_Type_Vector.Vector;
      Types_By_Name : Files.Type_Map    := new Files.P_Type_Map.Map;

      -- read an entire string block
      procedure String_Block is
         Count : Types.v64 := Input.V64;
      begin
         if 0 = Count then
            return;
         end if;

         -- read offsets
         declare
            Last : Types.i32 := 0;
            Off  : Types.i32;
            type Offset_Array is
              array (Types.v64 range 0 .. Count - 1) of Types.i32;
            Offsets : Offset_Array;
         begin
            for I in Offset_Array'Range loop
               Offsets (I) := Input.I32;
            end loop;

            for I in Offset_Array'Range loop
               Off := Offsets (I);
               Strings.AddPosition
               (Input.Position + Types.v64 (Last), Off - Last);
               Last := Off;
            end loop;

            Input.Jump (Input.Position + Types.v64 (Last));
         end;

      exception
         when E : others =>
            raise Skill.Errors.Skill_Error
              with Input.Parse_Exception
              (Block_Counter, E, "corrupted string block");
      end String_Block;

      -- read an entire type block
      procedure Type_Block is
         Offset     : Types.v64 := 0;
         Type_Count : Types.v64 := Input.V64;

         -- reads a single type declaration
         procedure Type_Definition is
            Name : constant Types.String_Access := Strings.Get (Input.V64);

            procedure Type_Restriction is
               Count : Types.v64 := Input.V64;
            begin
               -- TODO
               null;
            end Type_Restriction;
         begin
            if null = Name then
               raise Errors.Skill_Error
                 with Input.Parse_Exception
                 (Block_Counter, "corrupted file: nullptr in typename");
            end if;
--          // type duplication error detection
--          if (seenTypes.contains(name))
--              throw new ParseException(in, blockCounter, null, "Duplicate definition of type %s", name);
--          seenTypes.add(name);
--
--          // try to parse the type definition
            declare
               Count      : Types.v64 := Input.V64;
               Definition : Types.Pools.Pool;
               Super_Pool : Types.Pools.Pool;
               Super_Id   : Integer;
            begin
               if Types_By_Name.Contains (Name) then
                  Definition := Types_By_Name.Element (Name);
               else
                  -- type restrictions
                  -- TODO use the result!
                  Type_Restriction;

                  -- super
                  Super_Id := Integer (Input.V64);
                  if 0 = Super_Id then
                     Super_Pool := null;
                  else
                     if Super_Id > Type_Vector.Length then
                        raise Errors.Skill_Error
                          with Input.Parse_Exception
                          (Block_Counter, "Type " &
                           Name.all &
                           " refers to an ill-formed super type." &
                           Ada.Characters.Latin_1.LF &
                           "          found: " &
                           Integer'Image (Super_Id) &
                           "; current number of other types " &
                           Integer'Image (Type_Vector.Length));
                     else
                        Super_Pool := Type_Vector.Element (Super_Id - 1);
                     end if;
                  end if;

                  -- allocate pool
               end if;

               -- bpo

               -- store block info and prepare resize
            end;
            --          try {

--                 [[...]]
--
--                  // allocate pool
--                  definition = newPool(name, superDef, rest);
--              }
--
--              final long bpo = definition.basePool.data.length
--                      + ((0L != count && null != definition.superPool) ? in.v64() : 0L);
--
--              // store block info and prepare resize
--              definition.blocks.add(new Block(bpo, count));
--              resizeQueue.add(definition);
--
--              localFields.put(definition, (int) in.v64());
--          } catch (java.nio.BufferUnderflowException e) {
--              throw new ParseException(in, blockCounter, e, "unexpected end of file");
--          }
         end Type_Definition;
      begin
--          // reset counters and queues
--          seenTypes.clear();
--          resizeQueue.clear();
--          localFields.clear();
--          fieldDataQueue.clear();

         -- parse types
         for I in 1 .. Type_Count loop
            Type_Definition;
         end loop;

--
--          // resize pools
--          {
--              Stack<StoragePool<?, ?>> resizeStack = new Stack<>();
--
--              // resize base pools and push entries to stack
--              for (StoragePool<?, ?> p : resizeQueue) {
--                  if (p instanceof BasePool<?>) {
--                      final Block last = p.blocks.getLast();
--                      ((BasePool<?>) p).resizeData((int) last.count);
--                  }
--                  resizeStack.push(p);
--              }
--
--              // create instances from stack
--              while (!resizeStack.isEmpty()) {
--                  StoragePool<?, ?> p = resizeStack.pop();
--                  final Block last = p.blocks.getLast();
--                  int i = (int) last.bpo;
--                  int high = (int) (last.bpo + last.count);
--                  while (i < high && p.insertInstance(i + 1))
--                      i += 1;
--              }
--          }
--
--          // parse fields
--          for (StoragePool<?, ?> p : localFields.keySet()) {
--
--              // read field part
--              int legalFieldIDBarrier = 1 + p.dataFields.size();
--
--              final Block lastBlock = p.blocks.get(p.blocks.size() - 1);
--
--              for (int fieldCounter = localFields.get(p); fieldCounter != 0; fieldCounter--) {
--                  final int ID = (int) in.v64();
--                  if (ID > legalFieldIDBarrier || ID <= 0)
--                      throw new ParseException(in, blockCounter, null, "Found an illegal field ID: %d", ID);
--
--                  final long end;
--                  if (ID == legalFieldIDBarrier) {
--                      // new field
--                      final String fieldName = Strings.get(in.v64());
--                      if (null == fieldName)
--                          throw new ParseException(in, blockCounter, null, "corrupted file: nullptr in fieldname");
--
--                      FieldType<?> t = fieldType();
--                      HashSet<FieldRestriction<?>> rest = fieldRestrictions(t);
--                      end = in.v64();
--
--                      try {
--                          p.addField(ID, t, fieldName, rest).addChunk(new BulkChunk(offset, end, p.size()));
--                      } catch (SkillException e) {
--                          // transform to parse exception with propper values
--                          throw new ParseException(in, blockCounter, null, e.getMessage());
--                      }
--                      legalFieldIDBarrier += 1;
--
--                  } else {
--                      // field already seen
--                      end = in.v64();
--                      p.dataFields.get(ID - 1).addChunk(new SimpleChunk(offset, end, lastBlock.bpo, lastBlock.count));
--
--                  }
--                  offset = end;
--                  fieldDataQueue.add(new DataEntry(p, ID));
--              }
--          }
--
--          processFieldData();
      end Type_Block;

      -- build a state from intermediate information
      function Make_State return Files.File is
      begin
         return Skill.Files.Finish_Allocation
             (Path          => Input.Path,
              Mode          => Mode,
              Strings       => Strings,
              Types         => Type_Vector,
              Types_By_Name => Types_By_Name);
      end Make_State;

   begin

      while not Input.Eof loop
         String_Block;
         Type_Block;
         Block_Counter := Block_Counter + 1;
      end loop;

      return Make_State;
   end Read;
end Skill.Internal.File_Parsers;
