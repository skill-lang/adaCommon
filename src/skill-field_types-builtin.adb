--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     implementation of builtin field types               --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

with Skill.Types;
with Skill.Hashes; use Skill.Hashes;
with Skill.Types.Pools;
with Skill.Containers.Arrays;
with Skill.Containers.Sets;
with Skill.Containers.Maps;

package body Skill.Field_Types.Builtin is

   use type Skill.Types.v64;
   use type Skill.Types.Uv64;
   use type Skill.Containers.Boxed_Array;
   use type Skill.Types.Boxed_List;
   use type Skill.Containers.Boxed_Set;
   use type Skill.Types.Boxed_Map;

   function Offset_Single_V64 (Input : Types.v64) return Types.v64 is
      function Cast is new Ada.Unchecked_Conversion
        (Skill.Types.v64,
         Skill.Types.Uv64);

      V : constant Skill.Types.Uv64 := Cast (Input);

   begin
      if 0 = (V and 16#FFFFFFFFFFFFFF80#) then
         return 1;
      elsif 0 = (V and 16#FFFFFFFFFFFFC000#) then
         return 2;
      elsif 0 = (V and 16#FFFFFFFFFFE00000#) then
         return 3;
      elsif 0 = (V and 16#FFFFFFFFF0000000#) then
         return 4;
      elsif 0 = (V and 16#FFFFFFF800000000#) then
         return 5;
      elsif 0 = (V and 16#FFFFFC0000000000#) then
         return 6;
      elsif 0 = (V and 16#FFFE000000000000#) then
         return 7;
      elsif 0 = (V and 16#FF00000000000000#) then
         return 8;
      else
         return 9;
      end if;
   end Offset_Single_V64;

   procedure Insert
     (This : in out Skill.Containers.Boxed_Array;
      E    : in     Types.Box)
   is
   begin
      This.Append (E);
   end Insert;

   package body Annotation_Type_P is

      use type Types.Annotation;

      procedure Fix_Types (This : access Field_Type_T) is

         procedure Add (P : Types.Pools.Pool) is
         begin
            This.Types_By_Name.Include (P.Skill_Name, P);
         end Add;
      begin
         This.Types.Foreach (Add'Access);
      end Fix_Types;

      overriding function Read_Box
        (This  : access Field_Type_T;
         Input : Streams.Reader.Stream) return Types.Box
      is
         T   : Types.v64 := Input.V64;
         Idx : Types.v64 := Input.V64;
      begin
         if 0 = T then
            return Boxed (null);
         else
            declare
               Data : Types.Annotation_Array :=
                 This.Types.Element (Integer (T - 1)).Base.Data;
            begin
               return Boxed (Data (Integer (Idx)));
            end;
         end if;
      end Read_Box;

      overriding function Offset_Box
        (This   : access Field_Type_T;
         Target : Types.Box) return Types.v64
      is
         type T is access all String;

         function Cast is new Ada.Unchecked_Conversion
           (T,
            Types.String_Access);

         Ref : Types.Annotation := Unboxed (Target);
      begin
         if null = Ref then
            return 2;
         else
            return Offset_Single_V64
                (Types.v64
                   (1 +
                    This.Types_By_Name.Element
                    (Ref.Dynamic.Skill_Name).Pool_Offset)) +
              Offset_Single_V64 (Types.v64 (Ref.Skill_ID));
         end if;

      end Offset_Box;

      overriding procedure Write_Box
        (This   : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream;
         Target : Types.Box)
      is
         type T is access all String;

         function Cast is new Ada.Unchecked_Conversion
           (T,
            Types.String_Access);

         Ref : Types.Annotation := Unboxed (Target);
      begin
         if null = Ref then
            Output.I16 (0);
         else
            Output.V64
            (Types.v64
               (1 +
                This.Types_By_Name.Element
                (Ref.Dynamic.Skill_Name).Pool_Offset));

            Output.V64 (Types.v64 (Ref.Skill_ID));
         end if;
      end Write_Box;

   end Annotation_Type_P;

   package Arrays_P is new Skill.Containers.Arrays (Types.Box);

   package body Const_Arrays_P is
      pragma Warnings (Off);

      function Boxed
        (This : access Containers.Boxed_Array_T'Class) return Types.Box
      is
         type X is access all Containers.Boxed_Array_T'Class;
         function Cast is new Ada.Unchecked_Conversion (X, Types.Box);
      begin
         return Cast (X (This));
      end Boxed;

      function Read_Box
        (This  : access Field_Type_T;
         Input : Streams.Reader.Stream) return Types.Box
      is

         Count : Types.v64 := This.Length;

         Result : Containers.Boxed_Array :=
           Containers.Boxed_Array (Arrays_P.Make);
      begin
         for I in 1 .. Count loop
            Insert (Result, This.Base.Read_Box (Input));
         end loop;
         return Boxed (Result);
      end Read_Box;

      function Offset_Box
        (This   : access Field_Type_T;
         Target : Types.Box) return Types.v64
      is

         Result : Types.v64 := 0;
         Count  : Natural   := Natural (This.Length);
      begin
         for I in 1 .. Count loop
            Result :=
              Result + This.Base.Offset_Box (Unboxed (Target).Get (I - 1));
         end loop;
         return Result;
      end Offset_Box;

      procedure Write_Box
        (This   : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream;
         Target : Types.Box)
      is

         Length : Natural := Natural (This.Length);
      begin
         for I in 1 .. Length loop
            This.Base.Write_Box (Output, Unboxed (Target).Get (I - 1));
         end loop;
      end Write_Box;

   end Const_Arrays_P;

   package body Var_Arrays_P is

      function Read_Box
        (This  : access Field_Type_T;
         Input : Streams.Reader.Stream) return Types.Box
      is
         Count : Types.v64 := Input.V64;

         Result : Containers.Boxed_Array :=
           Containers.Boxed_Array (Arrays_P.Make);
      begin
         for I in 1 .. Count loop
            Insert (Result, This.Base.Read_Box (Input));
         end loop;
         return Boxed (Result);
      end Read_Box;

      function Offset_Box
        (This   : access Field_Type_T;
         Target : Types.Box) return Types.v64
      is
         Result : Types.v64;
         Arr    : Containers.Boxed_Array := Unboxed (Target);
         Count  : Natural                := Natural (Arr.Length);
      begin
         if null = Arr then
            return 1;
         end if;

         Result := Offset_Single_V64 (Types.v64 (Count));
         for I in 1 .. Count loop
            Result := Result + This.Base.Offset_Box (Arr.Get (I - 1));
         end loop;
         return Result;
      end Offset_Box;

      procedure Write_Box
        (This   : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream;
         Target : Types.Box)
      is
         Arr    : Containers.Boxed_Array := Unboxed (Target);
         Length : Natural                := Natural (Arr.Length);
      begin
         if null = Arr then
            Output.I8 (0);
            return;
         end if;

         Output.V64 (Types.v64 (Length));
         for I in 1 .. Length loop
            This.Base.Write_Box (Output, Arr.Get (I - 1));
         end loop;
      end Write_Box;

   end Var_Arrays_P;

   package body List_Type_P is

      function Read_Box
        (This  : access Field_Type_T;
         Input : Streams.Reader.Stream) return Types.Box
      is
         Count : Types.v64 := Input.V64;

         Result : Containers.Boxed_Array :=
           Containers.Boxed_Array (Arrays_P.Make);
      begin
         for I in 1 .. Count loop
            Insert (Result, This.Base.Read_Box (Input));
         end loop;
         return Boxed (Result);
      end Read_Box;

      function Offset_Box
        (This   : access Field_Type_T;
         Target : Types.Box) return Types.v64
      is
         Result : Types.v64;
         Arr    : Containers.Boxed_Array := Unboxed (Target);
         Count  : Natural                := Natural (Arr.Length);
      begin
         if null = Arr then
            return 1;
         end if;

         Result := Offset_Single_V64 (Types.v64 (Count));
         for I in 1 .. Count loop
            Result := Result + This.Base.Offset_Box (Arr.Get (I - 1));
         end loop;
         return Result;
      end Offset_Box;

      procedure Write_Box
        (This   : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream;
         Target : Types.Box)
      is
         Arr    : Containers.Boxed_Array := Unboxed (Target);
         Length : Natural                := Natural (Arr.Length);
      begin
         if null = Arr then
            Output.I8 (0);
            return;
         end if;

         Output.V64 (Types.v64 (Length));
         for I in 1 .. Length loop
            This.Base.Write_Box (Output, Arr.Get (I - 1));
         end loop;
      end Write_Box;

   end List_Type_P;

   package Sets_P is new Skill.Containers.Sets (Types.Box, Types.Hash, "=");

   package body Set_Type_P is

      function Read_Box
        (This  : access Field_Type_T;
         Input : Streams.Reader.Stream) return Types.Box
      is

         Count : Types.v64 := Input.V64;

         Result : Containers.Boxed_Set := Containers.Boxed_Set (Sets_P.Make);
      begin
         for I in 1 .. Count loop
            Result.Add (This.Base.Read_Box (Input));
         end loop;
         return Boxed (Result);
      end Read_Box;

      function Offset_Box
        (This   : access Field_Type_T;
         Target : Types.Box) return Types.v64
      is

         Result : Types.v64;
         Set    : Containers.Boxed_Set := Unboxed (Target);
         Count  : Natural              := Natural (Set.Length);
         Iter   : Containers.Set_Iterator;
      begin
         if null = Set then
            return 1;
         end if;

         Result := Offset_Single_V64 (Types.v64 (Count));
         Iter   := Set.Iterator;
         while Iter.Has_Next loop
            Result := Result + This.Base.Offset_Box (Iter.Next);
         end loop;
         Iter.Free;
         return Result;
      end Offset_Box;

      procedure Write_Box
        (This   : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream;
         Target : Types.Box)
      is

         Set    : Containers.Boxed_Set := Unboxed (Target);
         Length : Natural              := Natural (Set.Length);
         Iter   : Containers.Set_Iterator;
      begin
         if null = Set then
            Output.I8 (0);
            return;
         end if;

         Output.V64 (Types.v64 (Length));
         Iter := Set.Iterator;
         while Iter.Has_Next loop
            This.Base.Write_Box (Output, Iter.Next);
         end loop;
         Iter.Free;
      end Write_Box;

   end Set_Type_P;

   package Maps_P is new Skill.Containers.Maps(Types.Box, Types.Box, Types.Hash, "=", "=");

   package body Map_Type_P is

      function Read_Box
        (This  : access Field_Type_T;
         Input : Streams.Reader.Stream) return Types.Box
      is

         Count : Types.v64 := Input.V64;

         Result : Types.Boxed_Map := Types.Boxed_Map (Maps_P.Make);
         K, V   : Types.Box;
      begin
         for I in 1 .. Count loop
            K := This.Key.Read_Box (Input);
            V := This.Value.Read_Box (Input);
            Result.Update (K, V);
         end loop;
         return Boxed (Result);
      end Read_Box;

      function Offset_Box
        (This   : access Field_Type_T;
         Target : Types.Box) return Types.v64
      is

         Map : Containers.Boxed_Map := Unboxed (Target);
         Iter : Containers.Map_Iterator;

         Result : Types.v64;
         Count  : Natural := Natural (Map.Length);

      begin
         if null = Map or 0 = Count then
            return 1;
         end if;

         Result := Offset_Single_V64 (Types.v64 (Count));
         Iter := Map.Iterator;
         while Iter.Has_Next loop
            Result := Result
              + This.Key.Offset_Box (Iter.Key)
              + This.Value.Offset_Box (Iter.Value);
            Iter.Advance;
         end loop;
         Iter.Free;
         return Result;
      end Offset_Box;

      procedure Write_Box
        (This   : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream;
         Target : Types.Box)
      is

         Map    : Types.Boxed_Map := Unboxed (Target);
         Iter : Containers.Map_Iterator;
         Length : Natural         := Natural (Map.Length);
      begin
         if null = Map or 0 = Length then
            Output.I8 (0);
            return;
         end if;

         Output.V64 (Types.v64 (Length));
         Iter := Map.Iterator;
         while Iter.Has_Next loop
            This.Key.Write_Box (Output, Iter.Key);
            This.Value.Write_Box (Output, Iter.Value);
            Iter.Advance;
         end loop;
         Iter.Free;
      end Write_Box;

   end Map_Type_P;

end Skill.Field_Types.Builtin;
