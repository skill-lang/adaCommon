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
with Skill.Field_Types;
with Skill.Field_Types.Builtin;
with Skill.Field_Declarations;

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
     (Input : Skill.Streams.Reader.Input_Stream;
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

      -- entries of local fields
      type LF_Entry is record
         Pool : Types.Pools.Pool;
         Count : Types.V64;
      end record;
      package A2 is new Types.Vectors(LF_Entry);
      -- pool -> local field count
      Local_Fields : A2.Vector;


      -- field data updates: pool x fieldID
      type FD_Entry is record
         Pool : Types.Pools.Pool;
         ID   : Integer;
      end record;
      package A4 is new Types.Vectors (FD_Entry);
      -- field data updates: pool x fieldID
      Field_Data_Queue : A4.Vector;


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

         -- update field data information, so that it can be read in parallel or
         -- even lazy
         procedure Process_Field_Data is
         -- We Have To Add The File Offset To all Begins and Ends We Encounter
            File_Offset : constant Types.V64 := Input.Position;
            Data_End    : Types.V64 := File_Offset;
         begin

            -- process field data declarations in order of appearance and update
            -- offsets to absolute positions
            While not Field_Data_Queue.Is_Empty loop
               declare
                  E          : FD_Entry := Field_Data_Queue.Pop;
                  F          : Skill.Field_Declarations.Field_Declaration := E.Pool.Data_Fields.Element (E.ID - 1);
                  -- make begin/end absolute
                  End_Offset : Types.V64 := F.Add_Offset_To_Last_Chunk (Input, File_Offset);
               begin
                  if Data_End < End_Offset then
                     Data_End := End_Offset;
                  end if;
               end;
            end loop;
            Input.jump (Data_End);
         end Process_Field_Data;


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

            -- try to parse the type definition
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
                  Definition := New_Pool
                    (Type_Vector.Length + 32, Name, Super_Pool);

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
               Local_Fields.Append(LF_Entry'(Definition, Input.V64));
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

         function Parse_Field_Type return Skill.Field_Types.Field_Type is
            ID : Natural := Natural (Input.V64);

            function Convert is new Ada.Unchecked_Conversion
              (Source => Types.Pools.Pool,
               Target => Field_Types.Field_Type);
         begin
            case Id is
            when 0 =>
               return new Field_Types.Builtin.Constant_I8.Field_Type'
                 (Value => Input.I8);
            when 1 =>
               return new Field_Types.Builtin.Constant_I16.Field_Type'
                 (Value => Input.I16);
            when 2 =>
               return new Field_Types.Builtin.Constant_I32.Field_Type'
                 (Value => Input.I32);
            when 3 =>
               return new Field_Types.Builtin.Constant_I64.Field_Type'
                 (Value => Input.I64);
            when 4 =>
               return new Field_Types.Builtin.Constant_V64.Field_Type'
                 (Value => Input.V64);
            when 5 =>
               return Field_Types.Builtin.Annotation;
            when 6 =>
               return Field_Types.Builtin.Bool;
            when 7 =>
               return Field_Types.Builtin.I8;
            when 8 =>
               return Field_Types.Builtin.I16;
            when 9 =>
               return Field_Types.Builtin.I32;
            when 10 =>
               return Field_Types.Builtin.I64;
            when 11 =>
               return Field_Types.Builtin.V64;
            when 12 =>
               return Field_Types.Builtin.F32;
            when 13 =>
               return Field_Types.Builtin.F64;
            when 14 =>
               return Field_Types.Builtin.String_Type;
            when 15 =>
               return Constant_Length_Array(Input.V64, Parse_Field_Type);
            when 17 =>
               return Variable_Length_Array(Parse_Field_Type);
            when 18 =>
               return List_Type(Parse_Field_Type);
            when 19 =>
               return Set_Type(Parse_Field_Type);
            when 20 =>
               return Map_Type(Parse_Field_Type, Parse_Field_Type);
            when others =>
               if ID >= 32 and Id < Type_Vector.Length - 32 then
                  return Convert(Type_Vector.Element (ID - 32));
               end if;

               raise Errors.Skill_Error
               with Input.Parse_Exception
                 (Block_Counter,
                  "Invalid type ID: " & Natural'Image (ID));
            end case;
         end Parse_Field_Type;

      begin
         -- reset counters and queues

         -- seenTypes.clear();
         Seen_Types := A3.Empty_Set;
         -- resizeQueue.clear();
         Resize_Queue := A1.Empty_List;

         Local_Fields.Clear;

         Field_Data_Queue.Clear;

         -- parse types
         for I in 1 .. Type_Count loop
            Type_Definition;
         end loop;


         -- resize pools
         declare
            package A4 is new Types.Vectors(Skill.Types.Pools.Pool);
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


         -- parse fields
         while not Local_Fields.Is_Empty loop
            declare
               E : LF_Entry := Local_Fields.Pop;
               Legal_Field_ID_Barrier : Integer :=
                                          1 + E.Pool.Data_Fields.Length;
               Last_Block             : Skill.Internal.Parts.Block :=
                                          E.Pool.Blocks.Last_Element;
               End_Offset             : Types.V64;
            begin
               for Field_Counter in 1 .. E.Count loop
                  declare
                     Id : Integer := Integer(Input.V64);
                  begin
                     if Id <= 0 or else Id > Legal_Field_ID_Barrier then
                        raise Errors.Skill_Error
                        with Input.Parse_Exception
                          (Block_Counter,
                           "Found an illegal field ID: " & Integer'Image (ID));
                     end if;

                     if Id = Legal_Field_ID_Barrier then
                        -- new field
                        declare
                           Field_Name : Types.String_Access := Strings.Get
                             (Input.V64);
                           T          : Field_Types.Field_Type;

                           procedure Field_Restriction is
                              Count : Types.v64 := Input.V64;
                           begin
                              -- TODO
                              null;
                           end Field_Restriction;
                        begin
                           if null = Field_Name then
                              raise Errors.Skill_Error
                              with Input.Parse_Exception
                                (Block_Counter,
                                 "corrupted file: nullptr in fieldname");
                              end if;

                           T := Parse_Field_Type;
                           Field_Restriction;
                           End_Offset := Input.V64;

                           declare begin
                              -- TODO restrictions
                              E.Pool.Add_Field (ID, T, Field_Name).Add_Chunk
                                (new Internal.Parts.Bulck_Chunk
                                   '(Offset, End_Offset, Types.V64 (E.Pool.Size)));

                           exception
                              when E : Errors.Skill_Error =>
                                 raise Errors.Skill_Error
                                 with Input.Parse_Exception
                                   (Block_Counter, E,
                                    "failed to add field");
                           end;

                           Legal_Field_ID_Barrier := Legal_Field_ID_Barrier + 1;
                        end;
                     else
                        -- field already seen
                        End_Offset := Input.V64;
                        E.Pool.Data_Fields.Element (ID - 1).Add_Chunk
                          (new Internal.Parts.Simple_Chunk'
                             (Offset, End_Offset, Last_Block.Count, Last_Block.Bpo));
                     end if;
                     Offset := End_Offset;

                     Field_Data_Queue.Append (FD_Entry'(E.Pool, ID));
                  end;
               end loop;
            end;
         end loop;

         Process_Field_Data;
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
