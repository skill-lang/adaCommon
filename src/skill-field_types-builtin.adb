--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     implementation of builtin field types               --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;

with Skill.Types;
with Skill.Hashes; use Skill.Hashes;
with Skill.Types.Pools;
with Ada.Tags;
with Ada.Text_IO;

package body Skill.Field_Types.Builtin is

   use type Skill.Types.v64;
   use type Skill.Types.Uv64;

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

   procedure Insert (This : in out Types.Boxed_Array; E : in Types.Box) is
   begin
      This.Append (E);
   end Insert;
   procedure Insert (This : in out Types.Boxed_List; E : in Types.Box) is
   begin
      This.Append (E);
   end Insert;

   package body Annotation_Type_P is

      use type Types.Annotation;

      procedure Fix_Types (This : access Field_Type_T) is

         procedure Add (P : Types.Pools.Pool) is
         begin
            This.Types_By_Tag.Insert (P.Dynamic.Content_Tag, P);
         end Add;
      begin
         This.Types.Foreach (Add'Access);
      end Fix_Types;

      overriding function Read_Box
        (This  : access Field_Type_T;
         Input : Streams.Reader.Sub_Stream) return Types.Box
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

         Ref : Types.Annotation := Unboxed (Target);
      begin
         if null = Ref then
            return 2;
         else
            return Offset_Single_V64
                (Types.v64 (1 + This.Types_By_Tag.Element (Ref.Tag).Pool_Offset)) +
              Offset_Single_V64 (Types.v64 (Ref.Skill_ID));
         end if;
      exception
         when E : others =>
            Ada.Text_IO.Put_Line ("ref:");
            Ada.Text_IO.Put_Line (Ada.Tags.Expanded_Name (Ref.Tag));
            Ada.Text_IO.Put_Line (Integer'Image (Ref.Skill_ID));

            Ada.Text_IO.Put_Line ("map:");
            Ada.Text_Io.Put_Line (Integer'Image(Integer(This.Types_By_Tag.Length)));
--              for I of

            return 0;
      end Offset_Box;

      overriding procedure Write_Box
        (This   : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream;
         Target : Types.Box)
      is

         Ref : Types.Annotation := Unboxed (Target);
      begin
         if null = Ref then
            Output.I16 (0);
         else
            Output.V64 (Types.v64 (1 + This.Types_By_Tag.Element (Ref.Tag).Pool_Offset));

            Output.V64 (Types.v64 (Ref.Skill_ID));
         end if;
      end Write_Box;

   end Annotation_Type_P;

   package body Const_Arrays_P is

      function Read_Box
        (This  : access Field_Type_T;
         Input : Streams.Reader.Sub_Stream) return Types.Box
      is

         Count : Types.v64 := This.Length;

         Result : Types.Boxed_Array := Types.Arrays_P.Empty_Vector;
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
              Result + This.Base.Offset_Box (Unboxed (Target).Element (I));
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
            This.Base.Write_Box (Output, Unboxed (Target).Element (I));
         end loop;
      end Write_Box;

   end Const_Arrays_P;

   package body Var_Arrays_P is

      function Read_Box
        (This  : access Field_Type_T;
         Input : Streams.Reader.Sub_Stream) return Types.Box
      is

         Count : Types.v64 := Input.V64;

         Result : Types.Boxed_Array := Types.Arrays_P.Empty_Vector;
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
         Count  : Natural := Natural (Unboxed (Target).Length);
      begin
         Result := Offset_Single_V64 (Types.v64 (Count));
         for I in 1 .. Count loop
            Result :=
              Result + This.Base.Offset_Box (Unboxed (Target).Element (I));
         end loop;
         return Result;
      end Offset_Box;

      procedure Write_Box
        (This   : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream;
         Target : Types.Box)
      is

         Length : Natural := Natural (Unboxed (Target).Length);
      begin
         Output.V64 (Types.v64 (Length));
         for I in 1 .. Length loop
            This.Base.Write_Box (Output, Unboxed (Target).Element (I));
         end loop;
      end Write_Box;

   end Var_Arrays_P;

   package body List_Type_P is

      function Read_Box
        (This  : access Field_Type_T;
         Input : Streams.Reader.Sub_Stream) return Types.Box
      is

         Count : Types.v64 := Input.V64;

         Result : Types.Boxed_List := new Types.Lists_P.List;
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
         List   : Types.Boxed_List := Unboxed (Target);
         Count  : Natural          := Natural (List.Length);
      begin
         Result := Offset_Single_V64 (Types.v64 (Count));
         for I of List.all loop
            Result := Result + This.Base.Offset_Box (I);
         end loop;
         return Result;
      end Offset_Box;

      procedure Write_Box
        (This   : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream;
         Target : Types.Box)
      is

         List   : Types.Boxed_List := Unboxed (Target);
         Length : Natural          := Natural (List.Length);
      begin
         Output.V64 (Types.v64 (Length));
         for I of List.all loop
            This.Base.Write_Box (Output, I);
         end loop;
      end Write_Box;

   end List_Type_P;

   package body Set_Type_P is

      function Read_Box
        (This  : access Field_Type_T;
         Input : Streams.Reader.Sub_Stream) return Types.Box
      is

         Count : Types.v64 := Input.V64;

         Result : Types.Boxed_Set := new Types.Sets_P.Set;
      begin
         for I in 1 .. Count loop
            Result.Include (This.Base.Read_Box (Input));
         end loop;
         return Boxed (Result);
      end Read_Box;

      function Offset_Box
        (This   : access Field_Type_T;
         Target : Types.Box) return Types.v64
      is

         Result : Types.v64;
         Set    : Types.Boxed_Set := Unboxed (Target);
         Count  : Natural         := Natural (Set.Length);
      begin
         Result := Offset_Single_V64 (Types.v64 (Count));
         for I of Set.all loop
            Result := Result + This.Base.Offset_Box (I);
         end loop;
         return Result;
      end Offset_Box;

      procedure Write_Box
        (This   : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream;
         Target : Types.Box)
      is

         Set    : Types.Boxed_Set := Unboxed (Target);
         Length : Natural         := Natural (Set.Length);
      begin
         Output.V64 (Types.v64 (Length));
         for I of Set.all loop
            This.Base.Write_Box (Output, I);
         end loop;
      end Write_Box;

   end Set_Type_P;

   package body Map_Type_P is

      function Read_Box
        (This  : access Field_Type_T;
         Input : Streams.Reader.Sub_Stream) return Types.Box
      is

         Count : Types.v64 := Input.V64;

         Result : Types.Boxed_Map := new Types.Maps_P.Map;
         K, V   : Types.Box;
      begin
         for I in 1 .. Count loop
            K := This.Key.Read_Box (Input);
            V := This.Value.Read_Box (Input);
            Result.Include (K, V);
         end loop;
         return Boxed (Result);
      end Read_Box;

      function Offset_Box
        (This   : access Field_Type_T;
         Target : Types.Box) return Types.v64
      is

         Map : Types.Boxed_Map := Unboxed (Target);

         Result : Types.v64;
         Count  : Natural := Natural (Map.Length);

         procedure Offset (I : Types.Maps_P.Cursor) is
         begin
            Result := Result + This.Key.Offset_Box (Types.Maps_P.Key (I));
            Result :=
              Result + This.Value.Offset_Box (Types.Maps_P.Element (I));
         end Offset;

      begin
         Result := Offset_Single_V64 (Types.v64 (Count));
         Map.Iterate (Offset'Access);
         return Result;
      end Offset_Box;

      procedure Write_Box
        (This   : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream;
         Target : Types.Box)
      is

         Map    : Types.Boxed_Map := Unboxed (Target);
         Length : Natural         := Natural (Map.Length);

         procedure Offset (I : Types.Maps_P.Cursor) is
         begin
            This.Key.Write_Box (Output, Types.Maps_P.Key (I));
            This.Value.Write_Box (Output, Types.Maps_P.Element (I));
         end Offset;
      begin
         Output.V64 (Types.v64 (Length));
         Map.Iterate (Offset'Access);
      end Write_Box;

   end Map_Type_P;

end Skill.Field_Types.Builtin;
