--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file parser implementation                          --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

with Ada.Characters.Latin_1;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces;

with Skill.Containers.Vectors;
with Skill.Files;
with Skill.Types;
with Skill.Streams.Reader;
with Skill.Errors;
with Skill.String_Pools;
with Skill.Types.Pools;
with Skill.Internal.Parts;
with Skill.Hashes;
with Skill.Equals;
with Skill.Field_Declarations;
with Skill.Field_Restrictions;
with Skill.Field_Types;
with Skill.Field_Types.Builtin;

-- documentation can be found in java common
package body Skill.Internal.File_Parsers is

   use type Interfaces.Integer_32;
   use type Interfaces.Integer_64;
   use type Types.Annotation;

   use Skill;
   use type Types.String_Access;
   use type Types.Pools.Pool;

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
      String_Type : Field_Types.Builtin.String_Type_P.Field_Type :=
                      Field_Types.Builtin.String_Type_P.Make(Strings);
      Type_Vector   : Types.Pools.Type_Vector :=
                        Types.Pools.P_Type_Vector.Empty_Vector;
      Types_By_Name : Skill.Types.Pools.Type_Map :=
                        Skill.Types.Pools.P_Type_Map.Empty_Map;
      Annotation_Type : Field_Types.Builtin.Annotation_Type_P.Field_Type :=
                          Field_Types.Builtin.Annotation(Type_Vector);

      -- parser state --

      -- deferred pool resize requests
      Resize_Queue : Types.Pools.Type_Vector :=
                       Types.Pools.P_Type_Vector.Empty_Vector;

      -- entries of local fields
      type LF_Entry is record
         Pool : Types.Pools.Pool_Dyn;
         Count : Types.V64;
      end record;
      package A2 is new Containers.Vectors(Natural, LF_Entry);
      -- pool -> local field count
      Local_Fields : A2.Vector := A2.Empty_Vector;


      -- field data updates: pool x fieldID
      type FD_Entry is record
         Pool : Types.Pools.Pool_Dyn;
         ID   : Positive;
      end record;
      package A4 is new Containers.Vectors (Natural, FD_Entry);
      -- field data updates: pool x fieldID
      Field_Data_Queue : A4.Vector := A4.Empty_Vector;


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
               Off := Input.I32;
               Offsets (I) := Off;
               Input.Check_Offset(Types.V64(Off));
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
               Id : Types.V64;
            begin
               for I in 1 .. Count loop
                  Id := Input.V64;
                  case Id is
                  when 0 =>
                     -- unique
                     null;

                  when 1 =>
                     -- singleton
                     null;

                  when 2 =>
                     -- monotone
                     null;

                  when others =>
                     if Id <= 5 or else  1 = (id mod 2) then
                        raise Skill.Errors.Skill_Error
                        with Input.Parse_Exception
                          (Block_Counter,
                           "Found unknown type restriction " &
                             Integer'Image (Integer (Id)) &
                             ". Please regenerate your binding, if possible.");
                     end if;

                     Ada.Text_IO.Put_Line
                       ("Skiped unknown skippable type restriction." &
                          " Please update the SKilL implementation.");

                  end case;
               end loop;

               -- TODO result creation!!
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
               Block.Dynamic_Count := Types.Skill_ID_T(Input.V64);
               Block.Static_Count := Block.Dynamic_Count;

               if Types_By_Name.Contains (Name) then
                  Definition := Types_By_Name.Element (Name);
                  Super_Pool := Definition.Super;
               else
                  -- type restrictions
                  -- TODO use the result!
                  Type_Restriction;

                  -- super
                  Super_Id := Integer (Input.V64);
                  if 0 = Super_Id then
                     Super_Pool := null;
                  else
                     if Super_Id > Natural(Type_Vector.Length) then
                        raise Errors.Skill_Error
                          with Input.Parse_Exception
                          (Block_Counter, "Type " &
                           Name.all &
                           " refers to an ill-formed super type." &
                           Ada.Characters.Latin_1.LF &
                           "          found: " &
                           Integer'Image (Super_Id) &
                           "; current number of other types " &
                           Integer'Image (Natural(Type_Vector.Length)));
                     else
                        Super_Pool := Type_Vector.Element (Super_Id - 1);
                        pragma Assert (null /= Super_Pool);
                     end if;
                  end if;

                  -- allocate pool
                  -- TODO add restrictions as parameter
                  --     definition = newPool(name, superDef, rest);
                  Definition := New_Pool
                    (Natural(Type_Vector.Length) + 32, Name, Super_Pool);

                  Type_Vector.Append (Definition);
                  Types_By_Name.Include (Name, Definition);
               end if;

               -- bpo
               if 0 /= Block.Dynamic_Count and then null /= Definition.Super then
                  Block.Bpo := Definition.Base.Data'Length + Types.Skill_ID_T(Input.V64);
               elsif null /= Definition.Super and then Seen_Types.Contains (Definition.Super.Skill_Name) then
                  Block.Bpo := Definition.Super.Blocks.Last_Element.Bpo;
               else
                  Block.Bpo := Definition.Base.Data'Length;
               end if;

               pragma Assert(Definition /= null);

               -- store block info and prepare resize
               Definition.Blocks.Append (Block);
               Resize_Queue.Append (Definition); -- <-TODO hier nur base pools einfügen!
               Local_Fields.Append (LF_Entry'(Definition.Dynamic, Input.V64));

               -- fix parent static count
               if 0 /= Block.Dynamic_Count and then null /= Super_Pool then
                  -- calculate static count of our parent
                  declare
                     Sb : Internal.Parts.Block := Super_Pool.Blocks.Last_Element;
                     -- assumed static instances, minus what static instances would be, if p were the first sub pool.
                     Diff : constant Types.Skill_ID_T := sb.Static_Count -
                       (Block.bpo - sb.bpo);

                  begin
                     -- if positive, then we have to subtract it from the assumed static count (local and global)
                     if Diff > 0 then
                        Sb.Static_Count := Sb.Static_Count - Diff;
                        Super_Pool.Blocks.Replace_Element
                          (Super_Pool.Blocks.Length-1, Sb);
                     end if;
                  end;
               end if;
            end;
         exception
            when E : Constraint_Error =>
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
               return Field_Types.Field_Type(Annotation_Type);
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
               return Field_Types.Field_Type(String_Type);
            when 15 =>
               declare
                  Length : Types.V64 := Input.V64;
                  T : Field_Types.Field_Type := Parse_Field_Type;
               begin
                  return Field_Types.Builtin.Const_Array(Length, T);
               end;
            when 17 =>
               return Field_Types.Builtin.Var_Array (Parse_Field_Type);
            when 18 =>
               return Skill.Field_Types.Builtin.List_Type(Parse_Field_Type);
            when 19 =>
               return Skill.Field_Types.Builtin.Set_Type (Parse_Field_Type);
            when 20 =>
               declare
                  K : Field_Types.Field_Type := Parse_Field_Type;
                  V : Field_Types.Field_Type := Parse_Field_Type;
               begin
                  return Skill.Field_Types.Builtin.Map_Type (K, V);
               end;
            when others =>
               if ID >= 32 and Id < Natural(Type_Vector.Length) + 32 then
                  return Convert (Type_Vector.Element (ID - 32));
               end if;

               raise Errors.Skill_Error
               with Input.Parse_Exception
                 (Block_Counter,
                  "Invalid type ID: " & Natural'Image (ID) & " largest is: "
                  & Natural'Image (Natural(Type_Vector.Length) + 32));
            end case;
         end Parse_Field_Type;

         procedure Parse_Fields (E : LF_Entry) is
            Legal_Field_ID_Barrier : Positive :=
                                       1 + Natural(E.Pool.Data_Fields.Length);
            Last_Block             : Skill.Internal.Parts.Block :=
                                       E.Pool.Blocks.Last_Element;
            End_Offset             : Types.V64;
         begin
            for Field_Counter in 1 .. E.Count loop
               declare
                  Id : Integer := Integer (Input.V64);
               begin

                  if Id <= 0 or else Legal_Field_ID_Barrier < ID then
                     raise Errors.Skill_Error
                     with Input.Parse_Exception
                       (Block_Counter,
                        "Found an illegal field ID: " & Integer'Image (ID));
                  end if;

                  if Id = Legal_Field_ID_Barrier then
                     Legal_Field_ID_Barrier := Legal_Field_ID_Barrier + 1;
                     -- new field
                     declare
                        Field_Name : Types.String_Access := Strings.Get
                          (Input.V64);
                        T          : Field_Types.Field_Type;
                        Restrictions : Field_Restrictions.Vector;

                        function Field_Restriction return Field_Restrictions.Vector
                        Is
                           Count : Types.v64 := Input.V64;
                           Id    : Types.V64;
                           Rval : Field_Restrictions.Vector :=
                                    Field_Restrictions.Vector_P.Empty_Vector;

                           B : Types.Box;
                           L : Types.V64;
                        begin
                           for I in 1 .. Count loop
                              Id := Input.V64;
                              case Id is
                                 when 0 =>
                                    -- nonnull
                                    Rval.Append (Field_Restrictions.Nonnull);

                                 when 1 =>
                                    -- default
                                    if 5 = T.ID or else T.Id >= 32 then
                                       -- via type ID
                                       L := Input.V64;
                                    else
                                       -- via value
                                       B := T.Read_Box (Input.To);
                                    end if;
                                    null;

                                 when 3 =>
                                    -- range
                                    B := T.Read_Box (Input.To);
                                    B := T.Read_Box (Input.To);

                                 when 5 =>
                                    -- coding
                                    Rval.Append (new Field_Restrictions.Coding_T'(
                                                Name => Strings.Get(Input.V64)));

                                 when 7 =>
                                    Rval.Append
                                      (Field_Restrictions.Constant_Length_Pointer);

                                 when 9 =>
                                    -- one of
                                    for I in 1 .. Input.V64 loop
                                       -- type IDs
                                       L := Input.V64;
                                    end loop;

                                 when others =>
                                    if Id <= 9 or else 1 = (Id mod 2) then
                                       raise Skill.Errors.Skill_Error
                                       with Input.Parse_Exception
                                         (Block_Counter,
                                          "Found unknown field restriction " &
                                            Integer'Image (Integer (Id)) &
                                            ". Please regenerate your binding, if possible.");
                                    end if;

                                    Ada.Text_IO.Put_Line
                                      ("Skiped unknown skippable field restriction." &
                                         " Please update the SKilL implementation.");

                              end case;
                           end loop;

                           return Rval;
                        end Field_Restriction;
                     begin
                        if null = Field_Name then
                           raise Errors.Skill_Error
                           with Input.Parse_Exception
                             (Block_Counter,
                              "corrupted file: nullptr in fieldname");
                        end if;

                        T := Parse_Field_Type;
                        Restrictions := Field_Restriction;
                        End_Offset := Input.V64;

                        declare
                           -- TODO restrictions
                           F : Field_Declarations.Field_Declaration :=
                                 E.Pool.Add_Field (ID, T, Field_Name, Restrictions);
                        begin
                           F.Add_Chunk
                             (new Internal.Parts.Bulk_Chunk'
                                (Offset,
                                 End_Offset,
                                 E.Pool.Size,
                                 E.Pool.Blocks.Length)
                             );

                        exception
                           when E : Errors.Skill_Error =>
                              raise Errors.Skill_Error
                              with Input.Parse_Exception
                                (Block_Counter, E,
                                 "failed to add field");
                        end;
                     end;
                  else
                     -- field already seen
                     End_Offset := Input.V64;
                     E.Pool.Data_Fields.Element (ID).Add_Chunk
                       (new Internal.Parts.Simple_Chunk'
                          (Offset,
                           End_Offset,
                           Last_Block.Dynamic_Count,
                           Last_Block.Bpo));
                  end if;
                  Offset := End_Offset;

                  Field_Data_Queue.Append (FD_Entry'(E.Pool, ID));
               end;
            end loop;
         end;

      begin
         -- reset counters and queues

         -- seenTypes.clear();
         Seen_Types := A3.Empty_Set;

         Resize_Queue.Clear;

         Local_Fields.Clear;

         Field_Data_Queue.Clear;

         -- parse types
         for I in 1 .. Type_Count loop
            Type_Definition;
         end loop;


         -- resize pools
         declare
            Index : Natural := Natural'First;
            procedure Resize (E : Skill.Types.Pools.Pool) is
            begin
               E.Dynamic.Resize_Pool;
            end;
         begin
            Resize_Queue.Foreach (Resize'Access);
         end;

         -- parse fields
         Local_Fields.Foreach(Parse_Fields'Access);


         -- update field data information, so that it can be read in parallel or
         -- even lazy
         -- Process Field Data
         declare
            -- We Have To Add The File Offset To all Begins and Ends We Encounter
            File_Offset : constant Types.V64 := Input.Position;
            Data_End    : Types.V64 := File_Offset;
         begin

            -- process field data declarations in order of appearance and update
            -- offsets to absolute positions
            while not Field_Data_Queue.Is_Empty loop
               declare
                  use type Skill.Field_Declarations.Field_Declaration;

                  E : FD_Entry := Field_Data_Queue.Pop;
                  F : Skill.Field_Declarations.Field_Declaration :=
                        E.Pool.Data_Fields.Element (E.ID);
                  pragma Assert (F /= null);

                  -- make begin/end absolute
                  End_Offset : Types.V64 :=
                                 F.Add_Offset_To_Last_Chunk (Input, File_Offset);
               begin
                  if Data_End < End_Offset then
                     Data_End := End_Offset;
                  end if;
               end;
            end loop;
            Input.Jump (Data_End);
         end;
      end Type_Block;

      procedure Free_State is
      begin
         Local_Fields.Free;
         Field_Data_Queue.Free;
         Resize_Queue.Free;
      end;
   begin

      while not Input.Eof loop
         String_Block;
         Type_Block;
         Block_Counter := Block_Counter + 1;
      end loop;

      Free_State;

      return Make_State
        (Input.Path,
         Mode,
         Strings,
         String_Type,
         Annotation_Type,
         Type_Vector,
         Types_By_Name);

   exception
      when E : Storage_Error =>
         raise Errors.Skill_Error
         with Input.Parse_Exception
           (Block_Counter, E, "unexpected end of file");
   end Read;
end Skill.Internal.File_Parsers;
