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

package body Skill.String_Pools is

   function Create (Input : Skill.Streams.Reader.Input_Stream) return Pool is
      use type Skill.Streams.Reader.Input_Stream;
      This          : Pool := new Pool_T'(Input            => Input,
                                          New_Strings      => A1.Empty_Set,
                                          String_Positions => A2.Empty_Vector,
                                          Id_Map           => A3.Empty_Vector);
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
      Index : Types.V64) return Skill.Types.String_Access
   is
      Result : Skill.Types.String_Access;

      use type Skill.Streams.Reader.Input_Stream;

      -- should be synchronized now
      procedure Read_Result is
         Off   : Position := This.String_Positions.Element (Natural(Index));
         Input : Skill.Streams.Reader.Input_Stream := This.Input;
         Last  : Types.v64                           := Input.Position;

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
         This.Id_Map.Replace_Element (Natural(Index), Result);
      end Read_Result;

      Mutex : Skill.Synchronization.Mutex;
   begin

      if Index <= 0 then
         return null;
      end if;

      Result := This.Id_Map.Element (Natural(Index));
      if null /= Result then
         return Result;
      end if;

      -- we have to load the string from disk
      -- @note this block has to be synchronized in order to enable parallel
      -- decoding of field data
      -- @note this is correct, because string pool is the only one who can do
      -- parallel operations on input!
      Mutex.Lock;
      Read_Result;
      Mutex.Unlock;

      return Result;

   exception
      when E : others =>
         raise Skill.Errors.Skill_Error
           with InvalidPoolIndexException
             (Natural(Index),
              Integer(This.String_Positions.Length),
              "string",
              E);
   end Get;

--      public String get(long index) {
--          if (0L == index)
--              return null;
--
--          String result;
--          try {
--              result = idMap.get((int) index);
--          } catch (IndexOutOfBoundsException e) {
--              throw new InvalidPoolIndexException(index, stringPositions.size(), "string", e);
--          }
--          if (null != result)
--              return result;
--
--          // we have to load the string from disk
--          // @note this block has to be synchronized in order to enable parallel
--          // decoding of field data
--          // @note this is correct, because string pool is the only one who can do
--          // parallel operations on input!
--          synchronized (this) {
--              Position off = stringPositions.get((int) index);
--              input.push(off.absoluteOffset);
--              byte[] chars = input.bytes(off.length);
--              input.pop();
--
--              try {
--                  result = new String(chars, "UTF-8");
--              } catch (UnsupportedEncodingException e) {
--                  // as if that would ever happen
--                  e.printStackTrace();
--              }
--              idMap.set((int) index, result);
--          }
--          return result;
--      }

--                 declare
--                    -- range shifted by 1 to allow for empty ranges
--                    S : Types.String_Access :=
--                      new String (2 .. Positive (Off - Last + 1));
--                    function Convert is new Ada.Unchecked_Conversion
--                      (Types.i8,
--                       Character);
--                 begin
--                    for P in S'Range loop
--                       S (P) := Convert (Input.I8);
--                    end loop;
--                    Last := Off;
--                    Ada.Text_IO.Put_Line (S.all);
--                 end;
--                 declare
--                    -- range shifted by 1 to allow for empty ranges
--                    S : Types.String_Access :=
--                      new String (2 .. Positive (Off - Last + 1));
--                    function Convert is new Ada.Unchecked_Conversion
--                      (Types.i8,
--                       Character);
--                 begin
--                    for P in S'Range loop
--                       S (P) := Convert (Input.I8);
--                    end loop;
--                    Last := Off;
--                    Ada.Text_IO.Put_Line (S.all);
--                 end;
end Skill.String_Pools;
