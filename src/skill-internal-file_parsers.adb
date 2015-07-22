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
with Skill.Internal.Parts;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Skill.Hashes;
with Skill.Equals;
with Ada.Containers.Hashed_Sets;
with Skill.Types.Vectors;

-- documentation can be found in java common
package body Skill.Internal.File_Parsers is

   use type Interfaces.Integer_32;
   use type Interfaces.Integer_64;

   use Skill;
   use type Types.String_Access;
   use type Types.Pools.Pool;

   procedure Print (I : Types.i8) is
   begin
      Ada.Text_IO.Put_Line (Integer'Image (Integer (I)));
   end Print;

   function Read
     (Input : Skill.Streams.Input_Stream;
      Mode  : Skill.Files.Write_Mode) return Result
   is
      -- begin error reporting
      Block_Counter : Positive := 1;

      package A3 is new Ada.Containers.Hashed_Sets
        (Types.String_Access, Hashes.Hash, Equals.Equals);
      Seen_Types : A3.Set;
      -- end error reporting

      -- preliminary file
      Strings       : String_Pools.Pool := String_Pools.Create (Input);
      Type_Vector   : Files.Type_Vector := new Files.P_Type_Vector.Vector;
      Types_By_Name : Files.Type_Map    := new Files.P_Type_Map.Map;

      -- parser state
      package A1 is new Ada.Containers.Doubly_Linked_Lists(Types.Pools.Pool);
    -- deferred pool resize requests
      Resize_Queue : A1.List;

      package A2 is new Ada.Containers.Hashed_Maps
        (Key_Type        => Types.Pools.Pool,
         Element_Type    => Types.V64,
         Hash            => SKill.Hashes.Hash,
         Equivalent_Keys => Skill.Equals.Equals,
         "="             => "=");
      -- pool ⇒ local field count
      Local_Fields : A2.Map;


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
            --  type duplication error detection
            if Seen_Types.Contains (Name) then
               raise Errors.Skill_Error
               with Input.Parse_Exception
                 (Block_Counter, "Duplicate definition of type "& Name.all);
            end if;
            Seen_Types.Include (Name);
--
--          // try to parse the type definition
            declare
               Block      : Skill.Internal.Parts.Block;
               Definition : Types.Pools.Pool;
               Super_Pool : Types.Pools.Pool;
               Super_Id   : Integer;
            begin
               Block.Count := Input.V64;

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
                  -- TODO add restrictions as parameter
                  --     definition = newPool(name, superDef, rest);
                  Definition := New_Pool (Type_Vector.Length + 32, Name, Super_Pool);
                  Type_Vector.Append (Definition);
                  Types_By_Name.Include(Name, Definition);
               end if;

               -- bpo
               if 0 /= Block.Count and then null /= Definition.Super then
                  Block.Bpo := Definition.Base.Data'Length + Input.V64;
               else
                  Block.Bpo := Definition.Base.Data'Length;
               end if;

               -- store block info and prepare resize
               Definition.Blocks.Append (Block);
               Resize_Queue.Append (Definition);
               Local_Fields.Include(Definition, Input.V64);
            end;
         exception
            when E : Constraint_Error =>
               Skill.Errors.Print_Stacktrace(E);
               raise Errors.Skill_Error
               with Input.Parse_Exception
                 (Block_Counter, E, "unexpected corruption of parse state");
            when E : Storage_Error =>
               raise Errors.Skill_Error
               with Input.Parse_Exception
                 (Block_Counter, E, "unexpected end of file");
         end Type_Definition;
      begin
         -- reset counters and queues

         -- seenTypes.clear();
         Seen_Types := A3.Empty_Set;
         -- resizeQueue.clear();
         Resize_Queue := A1.Empty_List;
         -- localFields.clear();
         Local_Fields := A2.Empty_Map;

--          fieldDataQueue.clear();

         -- parse types
         for I in 1 .. Type_Count loop
            Type_Definition;
         end loop;


         -- resize pools
         declare
            package A4 is new Types.Vectors(Natural, Skill.Types.Pools.Pool);
            Resize_Stack : A4.Vector;

            use type A1.Cursor;
            I : A1.Cursor := Resize_Queue.First;
         begin
            -- resize base pools and push entries to stack
            while I /= Resize_Queue.Last loop
               declare
                  function Convert is new Ada.Unchecked_Conversion
                    (Source => Types.Pools.Pool,
                     Target => Types.Pools.Pool_Dyn);
                  function Convert is new Ada.Unchecked_Conversion
                    (Source => Types.Pools.Pool_Dyn,
                     Target => Types.Pools.Base_Pool);
                  P : Skill.Types.Pools.Pool_Dyn := Convert(A1.Element(I));
               begin
                  if P.all in Skill.Types.Pools.Base_Pool_T'Class then
                     Convert(P).Resize_Data;
                  end if;
                  Resize_Stack.Append(A1.Element(I));
               end;
            end loop;

            -- create instances from stack
            while not Resize_Stack.Is_Empty loop
               declare
                  function Convert is new Ada.Unchecked_Conversion
                    (Source => Types.Pools.Pool,
                     Target => Types.Pools.Pool_Dyn);
                  function Convert is new Ada.Unchecked_Conversion
                    (Source => Types.v64,
                     Target => Types.Skill_ID_T);
                  P : Types.Pools.Pool_Dyn := Convert(Resize_Stack.Pop);
                  Last : Parts.Block := P.Blocks.Last_Element;
               begin
                  Insert_Loop :
                  for I in (Last.Bpo + 1) .. (Last.Bpo + Last.Count) loop
                     exit Insert_Loop when not P.Insert_Instance (Convert (I));
                  end loop Insert_Loop;
               end;
            end loop;
         end;



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

   begin

      while not Input.Eof loop
         String_Block;
         Type_Block;
         Block_Counter := Block_Counter + 1;
      end loop;

      return Make_State (Input.Path, Mode, Strings, Type_Vector, Types_By_Name);

   exception
      when E : Storage_Error =>
         raise Errors.Skill_Error
         with Input.Parse_Exception
           (Block_Counter, E, "unexpected end of file");
   end Read;
end Skill.Internal.File_Parsers;
