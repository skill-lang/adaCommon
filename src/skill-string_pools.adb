--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     string pool management                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Interfaces;

with Skill.Errors;
with Skill.Types;
with Skill.Streams.Reader;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with Skill.Synchronization;
with Ada.Unchecked_Deallocation;

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
         Skill.Errors.Print_Stacktrace (E);
         raise Skill.Errors.Skill_Error;
   end Create;

   procedure Free (This : access Pool_T) is
      procedure Delete is new Ada.Unchecked_Deallocation
        (String,
         Skill.Types.String_Access);
      procedure Free (This : Skill.Types.String_Access) is
         D : Skill.Types.String_Access := This;
      begin
         if null /= D then
            Delete (D);
         end if;
      end Free;

      type P is access all Pool_T;
      procedure Delete is new Ada.Unchecked_Deallocation (Pool_T, P);
      D : P := P (This);

      use type Skill.Streams.Reader.Input_Stream;
   begin
      if null /= This.Input then
         This.Input.Free;
      end if;

      This.Id_Map.Foreach (Free'Access);
      This.Id_Map.Free;

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

   function Get
     (This  : access Pool_T;
      Index : Types.v64) return Skill.Types.String_Access
   is
      Result : Skill.Types.String_Access;

      use type Skill.Streams.Reader.Input_Stream;

      -- should be synchronized now
      procedure Read_Result is
         Off   : Position := This.String_Positions.Element (Natural (Index));
         Input : Skill.Streams.Reader.Input_Stream := This.Input;
         Last  : Types.v64                         := Input.Position;

         function Convert is new Ada.Unchecked_Conversion
           (Types.i8,
            Character);
      begin
         Input.Jump (Off.AbsoluteOffset);
         -- range shifted by 1 to allow for empty ranges
         Result := new String (2 .. Positive (Off.Length + 1));

         for P in Result'Range loop
            Result (P) := Convert (Input.I8);
         end loop;

         Input.Jump (Last);
         This.Id_Map.Replace_Element (Natural (Index), Result);
      end Read_Result;

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
      Read_Result;
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

   procedure Prepare_And_Write
     (This   : access Pool_T;
      Output : Skill.Streams.Writer.Output_Stream)
   is
      S : Types.String_Access;

      Count : Types.V64;

      use type Types.v64;
   begin
--        Das Hier Typmäßig Benennen
--            HashMap<String, Integer> serializationIDs = ws.stringIDs;

      -- ensure all strings are present
      for I in 1 .. This.String_Positions.Length - 1 loop
         S := This.Get (Types.v64 (I));
      end loop;
--
--          // create inverse map
--          for (int i = 1; i < idMap.size(); i++) {
--              serializationIDs.put(idMap.get(i), i);
--          }
--
--          // instert new strings to the map;
--          // this is the place where duplications with lazy strings will be detected and eliminated
--          for (String s : newStrings) {
--              if (!serializationIDs.containsKey(s)) {
--                  serializationIDs.put(s, idMap.size());
--                  idMap.add(s);
--              }
--          }
--

      Count := Types.V64(This.Id_Map.Length - 1);
      Output.V64 (Count);
      declare
         Off : Types.I32 := 0;

         -- map offsets
         Offsets : Streams.Writer.Sub_Stream := Output.Map(4 * Count);

         procedure Put (S : Types.String_Access) is
         begin
            -- first ID is mapped to null
            if null = S then
               return;
            end if;

            Off := Off + S.all'Size;
            Output.Put_Plain_String(S);
            Offsets.I32 (Off);
         end;
      begin
         This.Id_Map.Foreach(Put'Access);
      end;
   end Prepare_And_Write;
end Skill.String_Pools;
