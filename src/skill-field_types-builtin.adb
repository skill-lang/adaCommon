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

package body Skill.Field_Types.Builtin is

   use type Skill.Types.V64;
   use type Skill.Types.Uv64;

   function Offset_Single_V64
     (Input : Types.V64) return Types.V64 is
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

   procedure Insert(This : in out Types.Boxed_Array; E : in Types.Box) is
   begin
      This.Append(E);
   end Insert;
   procedure Insert(This : in out Types.Boxed_List; E : in Types.Box) is
   begin
      This.Append(E);
   end Insert;

   package body Annotation_Type_P is

      use type Types.Annotation;

      procedure Fix_Types
        (This : access Field_Type_T) is

         procedure Add(P : Types.Pools.Pool) is
         begin
            This.Types_By_Tag.Insert(P.Dynamic.Content_Tag, P);
         end;
      begin
         This.Types.Foreach(Add'Access);
      end Fix_Types;

      overriding
      function Read_Box
        (This : access Field_Type_T;
         Input : Streams.Reader.Sub_Stream) return Types.Box is
         T : Types.V64 := Input.V64;
         Idx : Types.V64 := Input.V64;
      begin
         if 0 = T then
            return Boxed(null);
         else
            declare
               Data : Types.Annotation_Array := This.Types.Element(Integer(T - 1)).Base.Data;
            begin
               return Boxed(Data(Integer(Idx)));
            end;
         end if;
      end Read_Box;

      overriding
      function Offset_Box
        (This : access Field_Type_T;
         Target : Types.Box) return Types.V64 is

         Ref : Types.Annotation := Unboxed(Target);
      begin
         if null = Ref then
            return 2;
         else
            return Offset_Single_V64(Types.V64
                                     (This.Types_By_Tag.Element(Ref.Tag).Id))
              +
              Offset_Single_V64(Types.V64(Ref.Skill_ID));
         end if;
      end Offset_Box;

      overriding
      procedure Write_Box
        (This : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream;
         Target : Types.Box) is

         Ref : Types.Annotation := Unboxed(Target);
      begin
         if null = Ref then
            Output.I16(0);
         else
            Output.V64 (Types.V64(
              (This.Types_By_Tag.Element(Ref.Tag).Id)));

            Output.V64(Types.V64(Ref.Skill_ID));
         end if;
      end Write_Box;

   end Annotation_Type_P;

   package body Var_Arrays_P is

      function Read_Box
        (This : access Field_Type_T;
         Input : Streams.Reader.Sub_Stream) return Types.Box is

         Count : Types.V64 := Input.V64;

         Result : Types.Boxed_Array := Types.Arrays_P.Empty_Vector;
      begin
         for I in 1 .. Count loop
            Insert(Result, This.Base.Read_Box(Input));
         end loop;
         return Boxed(Result);
      end Read_Box;

      function Offset_Box
        (This : access Field_Type_T;
         Target : Types.Box) return Types.V64 is

         Result : Types.V64;
         Count : Natural := Natural(Unboxed(Target).Length);
      begin
         Result := Offset_Single_V64(Types.V64(Count));
         for I in 1 .. Count loop
            Result := Result + This.Base.Offset_Box(Unboxed(Target).Element(I));
         end loop;
         return Result;
      end Offset_Box;

      procedure Write_Box
        (This : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream; Target : Types.Box) is

         Length : Natural := Natural(Unboxed(Target).Length);
      begin
         Output.V64 (Types.V64(Length));
         for I in 1 .. Length loop
            This.Base.Write_Box(Output, Unboxed(Target).Element(I));
         end loop;
      end Write_Box;

   end Var_Arrays_P;

end Skill.Field_Types.Builtin;
