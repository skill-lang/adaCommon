--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     file writer implementation                          --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
with Ada.Text_IO;
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

-- documentation can be found in java common
-- this is a combination of serialization functions, write and append
package body Skill.Internal.File_Writers is

   -- offset calculation closure
   type Cl_Offset is new Tasks.Closure_T with record
      F : Skill.Field_Declarations.Field_Declaration;
   end record;
   type Cx_Offset is not null access Cl_Offset;
   function Cast is new Ada.Unchecked_Conversion
     (Skill.Tasks.Closure,
      Cx_Offset);

   -- write field closure
   type Cl_Write is new Tasks.Closure_T with record
      F   : Skill.Field_Declarations.Field_Declaration;
      Map : Streams.Writer.Sub_Stream;
   end record;
   type Cx_Write is not null access Cl_Write;
   function Convert is new Ada.Unchecked_Conversion
     (Skill.Tasks.Closure,
      Cx_Write);

   -- write a file to disk
   procedure Write
     (State  : Skill.Files.File;
      Output : Skill.Streams.Writer.Output_Stream)
   is
      String_Type : Skill.Field_Types.Builtin.String_Type_P.Field_Type :=
        State.String_Type;

      Job_Failed_Concurrently : Boolean := False;

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

      procedure String (S : Types.String_Access) is
      begin
         Output.V64 (Types.v64 (String_Type.String_IDs.Element (S)));
      end String;

      procedure Write_Type (T : Field_Types.Field_Type) is
      begin
         Output.V64 (Types.v64 (T.ID));

         case T.ID is
            when 0 =>
               declare
                  type X is
                    access all Skill.Field_Types.Builtin.Constant_I8
                      .Field_Type;
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     X);
               begin
                  Output.I8 (Cast (T).Value);
               end;
            when 1 =>
               declare
                  type X is
                    access all Skill.Field_Types.Builtin.Constant_I16
                      .Field_Type;
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     X);
               begin
                  Output.I16 (Cast (T).Value);
               end;
            when 2 =>
               declare
                  type X is
                    access all Skill.Field_Types.Builtin.Constant_I32
                      .Field_Type;
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     X);
               begin
                  Output.I32 (Cast (T).Value);
               end;
            when 3 =>
               declare
                  type X is
                    access all Skill.Field_Types.Builtin.Constant_I64
                      .Field_Type;
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     X);
               begin
                  Output.I64 (Cast (T).Value);
               end;
            when 4 =>
               declare
                  type X is
                    access all Skill.Field_Types.Builtin.Constant_V64
                      .Field_Type;
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     X);
               begin
                  Output.V64 (Cast (T).Value);
               end;

            when 15 =>
               declare
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     Skill.Field_Types.Builtin.Const_Arrays_P.Field_Type);
               begin
                  Output.V64 (Cast (T).Length);
                  Write_Type (Cast (T).Base);
               end;

            when 17 =>
               declare
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     Skill.Field_Types.Builtin.Var_Arrays_P.Field_Type);
               begin
                  Write_Type (Cast (T).Base);
               end;

            when 18 =>
               declare
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     Skill.Field_Types.Builtin.List_Type_P.Field_Type);
               begin
                  Write_Type (Cast (T).Base);
               end;

            when 19 =>
               declare
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     Skill.Field_Types.Builtin.Set_Type_P.Field_Type);
               begin
                  Write_Type (Cast (T).Base);
               end;

            when 20 =>
               declare
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     Skill.Field_Types.Builtin.Map_Type_P.Field_Type);
               begin
                  Write_Type (Cast (T).Key);
                  Write_Type (Cast (T).Value);
               end;

            when others =>
               null;
         end case;
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
      Lbpo_Map : Lbpo_Map_T (0 .. Natural (State.Types.Length) - 1);

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

            procedure Add_Field (F : Field_Declarations.Field_Declaration) is

               procedure Add_String (I : Types.Annotation) is
               begin
                  Strings.Add
                  (Field_Types.Builtin.String_Type_P.Unboxed
                     (I.Dynamic.Reflective_Get (F)));
               end Add_String;
            begin
               Strings.Add (F.Name);
               -- add string data
               if F.T.ID = 14 then
                  This.Do_In_Type_Order (Add_String'Access);
               end if;
            end Add_Field;
         begin
            Strings.Add (This.Skill_Name);
            This.Data_Fields.Foreach (Add_Field'Access);
         end Add;
      begin
         State.Types.Foreach (Add'Access);
      end;

      ------------------------------
      -- PHASE 2: Check & Reorder --
      ------------------------------

      --  check consistency of the state, now that we aggregated all instances
      -- TODO State.Check;

      -- make lbpo map, update data map to contain dynamic instances and create
      -- skill IDs for serialization
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

      --------------------
      -- PHASE 3: Write --
      --------------------

      --  write string block
      State.Strings.Prepare_And_Write
      (Output, State.String_Type.String_IDs'Access);

      -- Calculate Offsets
      -- @note this has top happen after string IDs have been updated
      declare

         procedure Make (F : Field_Declaration) is

            procedure Calculate (C : Tasks.Closure) is
            begin
               Cast (C).F.Offset;
               Write_Barrier.Complete;
            exception
               when E : others =>
                  Job_Failed_Concurrently := True;
                  Write_Barrier.Complete;
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Current_Error,
                     "A task crashed during offset calculation: " &
                     Cast (C).F.Name.all);
            end Calculate;
            T : Skill.Tasks.Run (Calculate'Access);
            C : Skill.Tasks.Closure := new Cl_Offset'(F => F);
         begin
            Write_Barrier.Start;
            T.Start (C);
         end Make;

         procedure Off (This : Types.Pools.Pool) is
         begin
            This.Data_Fields.Foreach (Make'Access);
         end Off;
      begin
         State.Types.Foreach (Off'Access);
      end;

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
               Output.V64 (Types.v64 (This.Super.Pool_Offset + 1));
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
         if Job_Failed_Concurrently then
            raise Program_Error
              with "internal error during offset calculation";
         end if;

         -- write fields
         declare
--          ArrayList<Task> data = new ArrayList<>();
            End_Offset, Offset : Types.v64 := 0;

            procedure Write_Field (F : Field_Declaration) is
               P : Types.Pools.Pool := F.Owner.To_Pool;
            begin
               -- write info
               Output.V64 (Types.v64 (F.Index));
               String (F.Name);
               Write_Type (F.T);
               Restrictions (F);
               End_Offset := Offset + F.Future_Offset;
               Output.V64 (End_Offset);

               -- update chunks and prepare write data
               F.Data_Chunks.Clear;
               F.Data_Chunks.Append
               (new Chunk_Entry_T'
                  (C =>
                     new Skill.Internal.Parts.Bulk_Chunk'
                       (Offset, 0, Types.v64 (P.Size), 1),
                   Input => Skill.Streams.Reader.Empty_Sub_Stream));

               Offset := End_Offset;
            end Write_Field;
         begin
            Field_Queue.Foreach (Write_Field'Access);

            -- map field data
            Output.Begin_Block_Map (Offset);
         end;

         -- write field data
         declare

            procedure Write_Field (F : Field_Declarations.Field_Declaration) is

               procedure Job (C : Skill.Tasks.Closure) is
               begin
                  Convert (C).F.Write (Convert (C).Map);
                  Write_Barrier.Complete;
                  pragma Assert (Convert (C).Map.Eof);
                  Convert (C).Map.Close;
               exception
                  when E : others =>
                     Job_Failed_Concurrently := True;
                     Write_Barrier.Complete;
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Current_Error,
                        "A task crashed during write data: " &
                        Convert (C).F.Name.all);
               end Job;

               T : Skill.Tasks.Run (Job'Access);
               C : Skill.Tasks.Closure :=
                 new Cl_Write'(F => F, Map => Output.Map (F.Future_Offset));
            begin
               Write_Barrier.Start;
               T.Start (C);
            end Write_Field;
         begin
            Field_Queue.Foreach (Write_Field'Access);

            -- await writing of actual field data
            Write_Barrier.Await;
            Output.End_Block_Map;

            if Job_Failed_Concurrently then
               raise Program_Error with "internal error during field write";
            end if;
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

--------------------------------------------------------------------------------
---------------------------------- APPEND --------------------------------------
--------------------------------------------------------------------------------

   -- append a file to an existing one
   procedure Append
     (State  : Skill.Files.File;
      Output : Skill.Streams.Writer.Output_Stream)
   is
      String_Type : Skill.Field_Types.Builtin.String_Type_P.Field_Type :=
        State.String_Type;

      Job_Failed_Concurrently : Boolean := False;

      function Make_LBPO_Map
        (P        :        Types.Pools.Pool;
         Lbpo_Map : in out Lbpo_Map_T;
         Next     :        Integer) return Integer
      is
         Result : Integer := Next + P.Dynamic.New_Objects_Size;

         procedure Children (sub : Types.Pools.Sub_Pool) is
         begin
            Result := Make_LBPO_Map (sub.To_Pool, Lbpo_Map, Result);
         end Children;
      begin
         Lbpo_Map (P.Pool_Offset) := Next;
         P.Sub_Pools.Foreach (Children'Access);
         return Result;
      end Make_LBPO_Map;

      procedure String (S : Types.String_Access) is
      begin
         Output.V64 (Types.v64 (String_Type.String_IDs.Element (S)));
      end String;

      procedure Write_Type (T : Field_Types.Field_Type) is
      begin
         Output.V64 (Types.v64 (T.ID));

         case T.ID is
            when 0 =>
               declare
                  type X is
                    access all Skill.Field_Types.Builtin.Constant_I8
                      .Field_Type;
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     X);
               begin
                  Output.I8 (Cast (T).Value);
               end;
            when 1 =>
               declare
                  type X is
                    access all Skill.Field_Types.Builtin.Constant_I16
                      .Field_Type;
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     X);
               begin
                  Output.I16 (Cast (T).Value);
               end;
            when 2 =>
               declare
                  type X is
                    access all Skill.Field_Types.Builtin.Constant_I32
                      .Field_Type;
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     X);
               begin
                  Output.I32 (Cast (T).Value);
               end;
            when 3 =>
               declare
                  type X is
                    access all Skill.Field_Types.Builtin.Constant_I64
                      .Field_Type;
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     X);
               begin
                  Output.I64 (Cast (T).Value);
               end;
            when 4 =>
               declare
                  type X is
                    access all Skill.Field_Types.Builtin.Constant_V64
                      .Field_Type;
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     X);
               begin
                  Output.V64 (Cast (T).Value);
               end;

            when 15 =>
               declare
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     Skill.Field_Types.Builtin.Const_Arrays_P.Field_Type);
               begin
                  Output.V64 (Cast (T).Length);
                  Write_Type (Cast (T).Base);
               end;

            when 17 =>
               declare
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     Skill.Field_Types.Builtin.Var_Arrays_P.Field_Type);
               begin
                  Write_Type (Cast (T).Base);
               end;

            when 18 =>
               declare
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     Skill.Field_Types.Builtin.List_Type_P.Field_Type);
               begin
                  Write_Type (Cast (T).Base);
               end;

            when 19 =>
               declare
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     Skill.Field_Types.Builtin.Set_Type_P.Field_Type);
               begin
                  Write_Type (Cast (T).Base);
               end;

            when 20 =>
               declare
                  function Cast is new Ada.Unchecked_Conversion
                    (Field_Types.Field_Type,
                     Skill.Field_Types.Builtin.Map_Type_P.Field_Type);
               begin
                  Write_Type (Cast (T).Key);
                  Write_Type (Cast (T).Value);
               end;

            when others =>
               null;
         end case;
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

      -- index of the first new pool
      New_Pool_Index : Natural := 0;

      -- index → bpo
      --  @note pools.par would not be possible if it were an actual
      Lbpo_Map : Lbpo_Map_T (0 .. Natural (State.Types.Length) - 1);

      Chunk_Map : Field_Declarations.Chunk_Map := new Chunk_Map_P.Map;

      -- relevant pools
      R_Pools : Skill.Types.Pools.Type_Vector :=
        Types.Pools.P_Type_Vector.Empty_Vector;

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

            procedure Add_Field (F : Field_Declarations.Field_Declaration) is

               procedure Add_String (I : Types.Annotation) is
               begin
                  Strings.Add
                  (Field_Types.Builtin.String_Type_P.Unboxed
                     (I.Dynamic.Reflective_Get (F)));
               end Add_String;
            begin
               Strings.Add (F.Name);
               -- add string data
               if F.T.ID = 14 then
                  This.Do_In_Type_Order (Add_String'Access);
               end if;
            end Add_Field;
         begin
            Strings.Add (This.Skill_Name);
            This.Data_Fields.Foreach (Add_Field'Access);
         end Add;
      begin
         State.Types.Foreach (Add'Access);
      end;

      ------------------------------
      -- PHASE 2: Check & Reorder --
      ------------------------------

      --  check consistency of the state, now that we aggregated all instances
      -- TODO State.Check;

      -- save the index of the first new pool
      declare
         T : Skill.Types.Pools.Type_Vector := State.Types;
      begin
         for I in 1 .. T.Length - 1 loop
            exit when T.Element (I - 1).Blocks.Is_Empty;
            New_Pool_Index := New_Pool_Index + 1;
         end loop;
         if not T.Last_Element.Blocks.Is_Empty then
            New_Pool_Index := Natural'Last;
         end if;
      end;

      -- make lbpo map, update data map to contain dynamic instances and create
      -- skill IDs for serialization
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
               Cast (This).Prepare_Append (Chunk_Map);
            end if;
         end Make;
      begin
         State.Types.Foreach (Make'Access);
      end;

      -- locate relevant pools
      R_Pools.Ensure_Index (State.Types.Length);
      declare

         procedure Find (P : Types.Pools.Pool) is
         begin
            -- new index?
            if P.Pool_Offset >= New_Pool_Index then
               R_Pools.Append (P);

            -- new instance or field?
            elsif P.Size > 0 then
               declare
                  Ex : Boolean := False;
                  procedure Exist (F : Field_Declaration) is
                  begin
                     if not Ex and then Chunk_Map.Contains (F) then
                        Ex := True;
                     end if;
                  end Exist;

               begin
                  P.Data_Fields.Foreach (Exist'Access);

                  if Ex then
                     R_Pools.Append (P);
                  end if;
               end;
            end if;

         end Find;
      begin
         State.Types.Foreach (Find'Access);
      end;

      --------------------
      -- PHASE 3: Write --
      --------------------

      --  write string block
      State.Strings.Prepare_And_Append
      (Output, State.String_Type.String_IDs'Access);

      -- Calculate Offsets
      -- @note this has top happen after string IDs have been updated
      declare

         procedure Make (F : Field_Declaration) is

            procedure Calculate (C : Tasks.Closure) is
            begin
               Cast (C).F.Offset;
               Write_Barrier.Complete;
            exception
               when E : others =>
                  Job_Failed_Concurrently := True;
                  Write_Barrier.Complete;
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Current_Error,
                     "A task crashed during offset calculation: " &
                     Cast (C).F.Name.all);
            end Calculate;
            T : Skill.Tasks.Run (Calculate'Access);
            C : Skill.Tasks.Closure := new Cl_Offset'(F => F);
         begin
            Write_Barrier.Start;
            T.Start (C);
         end Make;

         use Chunk_Map_P;
         procedure Off (P : Cursor) is
         begin
            Make (Key (P));
         end Off;
      begin
         Chunk_Map.Iterate (Off'Access);
      end;

      -- write count of the type block
      Output.V64 (Types.v64 (R_Pools.Length));

      -- write types
      declare
         use type Types.Pools.Pool;
         use type Interfaces.Integer_64;

         package A1 renames Field_Declarations.Field_Vector_P;
         Field_Queue : A1.Vector := A1.Empty_Vector;

         procedure Write_Type (This : Types.Pools.Pool) is
            New_Pool : constant Boolean := This.Pool_Offset >= New_Pool_Index;

            Fields : Field_Vector := Field_Vector_P.Empty_Vector;

            Lcount : Types.v64 := This.Blocks.Last_Element.Count;

            procedure Add (F : Field_Declaration) is
            begin
               if Chunk_Map.Contains (F) then
                  Fields.Append (F);
               end if;
            end Add;

         begin
            Fields.Ensure_Index (This.Data_Fields.Length);
            This.Data_Fields.Foreach (Add'Access);

            if New_Pool
              or else (0 /= This.Data_Fields.Length and then This.Size > 0)
            then
               String (This.Skill_Name);
               Output.V64 (Lcount);

               if New_Pool then
                  Restrictions (This);
                  if null = This.Super then
                     Output.I8 (0);
                  else
                     Output.V64 (Types.v64 (This.Super.Pool_Offset + 1));
                     if 0 /= Lcount then
                        Output.V64 (Types.v64 (Lbpo_Map (This.Pool_Offset)));
                     end if;
                  end if;
               elsif null /= This.Super and then 0 /= Lcount then
                  Output.V64 (Types.v64 (Lbpo_Map (This.Pool_Offset)));
               end if;

               if New_Pool and then 0 = Lcount then
                  Output.I8 (0);
               else
                  Output.V64 (Types.v64 (This.Data_Fields.Length));
                  Field_Queue.Append_All (This.Data_Fields);
               end if;
            end if;

         end Write_Type;

      begin
         R_Pools.Foreach (Write_Type'Access);

         -- await offsets before we can write fields
         Write_Barrier.Await;
         if Job_Failed_Concurrently then
            raise Program_Error
              with "internal error during offset calculation";
         end if;

         -- write fields
         declare
            --          ArrayList<Task> data = new ArrayList<>();
            End_Offset, Offset : Types.v64 := 0;

            procedure Write_Field (F : Field_Declaration) is
               P : Types.Pools.Pool := F.Owner.To_Pool;
            begin
               -- write info
               Output.V64 (Types.v64 (F.Index));

               if F.Data_Chunks.Last_Element.C.all in
                   Skill.Internal.Parts.Bulk_Chunk
               then
                  String (F.Name);
                  Write_Type (F.T);
                  Restrictions (F);
               end if;

               End_Offset := Offset + F.Future_Offset;
               Output.V64 (End_Offset);

               Offset := End_Offset;
            end Write_Field;
         begin
            Field_Queue.Foreach (Write_Field'Access);

            -- map field data
            Output.Begin_Block_Map (Offset);
         end;

         -- write field data
         declare

            procedure Write_Field (F : Field_Declarations.Field_Declaration) is

               procedure Job (C : Skill.Tasks.Closure) is
               begin
                  Convert (C).F.Write (Convert (C).Map);
                  Write_Barrier.Complete;
                  pragma Assert (Convert (C).Map.Eof);
                  Convert (C).Map.Close;
               exception
                  when E : others =>
                     Job_Failed_Concurrently := True;
                     Write_Barrier.Complete;
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Current_Error,
                        "A task crashed during write data: " &
                        Convert (C).F.Name.all);
               end Job;

               T : Skill.Tasks.Run (Job'Access);
               C : Skill.Tasks.Closure :=
                 new Cl_Write'(F => F, Map => Output.Map (F.Future_Offset));
            begin
               Write_Barrier.Start;
               T.Start (C);
            end Write_Field;
         begin
            Field_Queue.Foreach (Write_Field'Access);

            -- await writing of actual field data
            Write_Barrier.Await;
            Output.End_Block_Map;

            if Job_Failed_Concurrently then
               raise Program_Error with "internal error during field write";
            end if;
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
   end Append;

end Skill.Internal.File_Writers;
