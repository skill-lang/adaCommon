--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     string pool management                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

with Interfaces;

with Skill.Errors;
with Skill.Field_Types.Builtin.String_Type_P;

package body Skill.String_Pools is

   -- note: input is now owned by the pool
   function Create (Input : Skill.Streams.Reader.Input_Stream) return Pool is
      use type Skill.Streams.Reader.Input_Stream;
      This : Pool :=
        new Pool_T'
          (Input            => Input,
           New_Strings      => A1.Empty_Set,
           String_Positions => A2.Empty_Vector,
           Id_Map           => A3.Empty_Vector,
           Mutex            => <>);
      Null_Position : Position;

      use Interfaces;
   begin

      --null translation trick
      This.String_Positions.Append (Null_Position);
      This.Id_Map.Append (null);

      return This;

   exception
      when E : others =>
         raise Skill.Errors.Skill_Error;
   end Create;

   procedure Free (This : Skill.Types.String_Access) is
      procedure Delete is new Ada.Unchecked_Deallocation
        (String,
         Skill.Types.String_Access);
      D : Skill.Types.String_Access := This;
   begin
      if null /= D then
         Delete (D);
      end if;
   end Free;

   procedure Free (This : access Pool_T) is

      type P is access all Pool_T;
      procedure Delete is new Ada.Unchecked_Deallocation (Pool_T, P);
      D : P := P (This);

      use type Skill.Streams.Reader.Input_Stream;
   begin
      if null /= This.Input then
         This.Input.Close;
      end if;

      -- not true, because some strings are global constants
      --        This.Id_Map.Foreach (Free'Access);
      This.Id_Map.Free;
      This.String_Positions.Free;

      Delete (D);
   end Free;

   function Size (This : access Pool_T) return Integer is
   begin
      return Integer (This.String_Positions.Length) +
        Integer (This.New_Strings.Length);
   end Size;

   procedure AddPosition
     (This : access Pool_T;
      Pos  : Types.v64;
      Len  : Types.i32)
   is
      RPos : Position;
   begin
      RPos.AbsoluteOffset := Pos;
      RPos.Length         := Len;
      This.String_Positions.Append (RPos);
      This.Id_Map.Append (null);
   end AddPosition;

   -- should be synchronized now
   function Read_Result
     (This  : access Pool_T;
      Index : Types.v64) return Types.String_Access
   is
      Off   : Position := This.String_Positions.Element (Natural (Index));
      Input : Skill.Streams.Reader.Input_Stream := This.Input;
      Last  : Types.v64                         := Input.Position;

      use type Skill.Streams.Reader.Input_Stream;

      function Convert is new Ada.Unchecked_Conversion (Types.i8, Character);

      Result : Skill.Types.String_Access;
   begin
      Input.Jump (Off.AbsoluteOffset);
      -- range shifted by 1 to allow for empty ranges
      Result := new String (2 .. Positive (Off.Length + 1));

      for P in Result'Range loop
         Result (P) := Convert (Input.I8);
      end loop;

      -- unify with new strings
      declare
         Cursor   : A1.Cursor;
         Inserted : Boolean;
      begin
         This.New_Strings.Insert (Result, Cursor, Inserted);
         if not Inserted then
            Free (Result);
            Result := A1.Element (Cursor);
         end if;
      end;

      Input.Jump (Last);
      This.Id_Map.Replace_Element (Natural (Index), Result);

      return Result;
   end Read_Result;

   function Get
     (This  : access Pool_T;
      Index : Types.v64) return Skill.Types.String_Access
   is
      Result : Skill.Types.String_Access;
   begin

      if Index <= 0 then
         return null;
      end if;

      Result := This.Id_Map.Element (Natural (Index));
      if null /= Result then
         return Result;
      end if;

      -- we have to load the string from disk
      -- @note this block has to be synchronized in order to enable parallel
      -- decoding of field data
      -- @note this is correct, because string pool is the only one who can do
      -- parallel operations on input!
      This.Mutex.Lock;
      Result := Read_Result (This, Index);
      This.Mutex.Unlock;

      return Result;

   exception
      when E : others =>
         raise Skill.Errors.Skill_Error
           with InvalidPoolIndexException
             (Natural (Index),
              Integer (This.String_Positions.Length),
              "string",
              E);
   end Get;

   procedure Add (This : access Pool_T; S : Types.String_Access) is
   begin
      if null /= S then
         This.New_Strings.Include (S);
      end if;
   end Add;

   function Add
     (This : access Pool_T;
      S    : String) return Types.String_Access
   is
      Cursor   : A1.Cursor;
      Inserted : Boolean;
      Str      : Types.String_Access := new String'(S);
   begin
      This.New_Strings.Insert (Str, Cursor, Inserted);
      if Inserted then
         return Str;
      else
         Free (Str);
         return A1.Element (Cursor);
      end if;
   end Add;

   procedure Prepare_And_Write
     (This              : access Pool_T;
      Output            : Skill.Streams.Writer.Output_Stream;
      Serialization_IDs : Skill.Field_Types.Builtin.String_Type_P.ID_Map)
   is
      S : Types.String_Access;

      Count : Types.v64;

      use type Types.v64;
      use type Skill.Field_Types.Builtin.String_Type_P.ID_Map;
   begin

      -- ensure all strings are present
      for I in 1 .. Types.v64 (This.String_Positions.Length - 1) loop
         S := This.Get (I);
      end loop;

      -- create inverse map
      for I in 1 .. Types.Skill_ID_T (This.Id_Map.Length - 1) loop
         Serialization_IDs.Insert
         (Key => This.Id_Map.Element (I), New_Item => I);
      end loop;

   -- instert new strings to the map;
   -- this is the place where duplications with lazy strings will be detected
   -- and eliminated
      for S of This.New_Strings loop
         if not Serialization_IDs.Contains (S) then
            Serialization_IDs.Insert (S, This.Id_Map.Length);
            This.Id_Map.Append (S);
         end if;
      end loop;

      Count := Types.v64 (This.Id_Map.Length - 1);
      Output.V64 (Count);

      Output.Begin_Block_Map (4 * Count);
      declare
         Off : Types.i32 := 0;

         -- map offsets
         Offsets : Streams.Writer.Sub_Stream := Output.Map (4 * Count);

         procedure Put (S : Types.String_Access) is
         begin
            -- first ID is mapped to null
            if null = S then
               return;
            end if;

            Off := Off + S.all'Size / 8;
            Output.Put_Plain_String (S);
            Offsets.I32 (Off);
         end Put;
      begin
         This.Id_Map.Foreach (Put'Access);
         Output.End_Block_Map;
      end;
   end Prepare_And_Write;

   procedure Prepare_And_Append
     (This              : access Pool_T;
      Output            : Skill.Streams.Writer.Output_Stream;
      Serialization_IDs : Skill.Field_Types.Builtin.String_Type_P.ID_Map)
   is
      S : Types.String_Access;

      Count : Types.v64;

      use type Types.v64;
      use type Skill.Field_Types.Builtin.String_Type_P.ID_Map;

      Todo : A3.Vector := A3.Empty_Vector;
   begin

      -- ensure all strings are present
      for I in 1 .. Types.v64 (This.String_Positions.Length - 1) loop
         S := This.Get (I);
      end loop;

      -- create inverse map
      for I in 1 .. Types.Skill_ID_T (This.Id_Map.Length - 1) loop
         Serialization_IDs.Include
         (Key => This.Id_Map.Element (I), New_Item => I);
      end loop;

   -- instert new strings to the map;
   -- this is the place where duplications with lazy strings will be detected
   -- and eliminated;
   -- this is also the place, where new instances are appended to the output
   -- file
      for S of This.New_Strings loop
         if not Serialization_IDs.Contains (S) then
            Serialization_IDs.Include (S, This.Id_Map.Length);
            This.Id_Map.Append (S);
            Todo.Append (S);
         end if;
      end loop;

      Count := Types.v64 (Todo.Length);
      Output.V64 (Count);

      if 0 /= Count then
         Output.Begin_Block_Map (4 * Count);
         declare
            Off : Types.i32 := 0;

            -- map offsets
            Offsets : Streams.Writer.Sub_Stream := Output.Map (4 * Count);

            procedure Put (S : Types.String_Access) is
            begin
               -- first ID is mapped to null
               if null = S then
                  return;
               end if;

               Off := Off + S.all'Size / 8;
               Output.Put_Plain_String (S);
               Offsets.I32 (Off);
            end Put;
         begin
            Todo.Foreach (Put'Access);
            Output.End_Block_Map;
         end;
      end if;

      Todo.Free;
   end Prepare_And_Append;
end Skill.String_Pools;
