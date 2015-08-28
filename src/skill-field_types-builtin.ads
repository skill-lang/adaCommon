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
with Ada.Tags;
with Ada.Unchecked_Conversion;

with Skill.Types;
with Skill.Hashes; use Skill.Hashes;
with Skill.Equals; use Skill.Equals;
with Skill.String_Pools;
with Skill.Streams;
with Skill.Streams.Writer;
with Skill.Types.Pools;

with Skill.Field_Types.Constant_Types;
with Skill.Field_Types.Plain_Types;

package Skill.Field_Types.Builtin is

   use type Types.Box;

   package T renames Skill.Types;

   package Constant_I8 is new Constant_Types (T.I8, 0, "constant i8");
   function Const_I8 (V : T.I8) return Field_Type is
     (new Constant_I8.Field_Type'(Value => V));

   package Constant_I16 is new Constant_Types (T.I16, 1, "constant i16");
   function Const_I16 (V : T.I16) return Field_Type is
     (new Constant_I16.Field_Type'(Value => V));

   package Constant_I32 is new Constant_Types (T.I32, 2, "constant i32");
   function Const_I32 (V : T.I32) return Field_Type is
     (new Constant_I32.Field_Type'(Value => V));

   package Constant_I64 is new Constant_Types (T.I64, 3, "constant i64");
   function Const_I64 (V : T.I64) return Field_Type is
     (new Constant_I64.Field_Type'(Value => V));

   package Constant_V64 is new Constant_Types (T.V64, 4, "constant v64");
   function Const_V64 (V : T.V64) return Field_Type is
     (new Constant_V64.Field_Type'(Value => V));


   package Annotation_Type_P is
      pragma Warnings (Off);

      package A1 is new Field_Types (Types.Annotation, 5);
      package IDs is new Ada.Containers.Hashed_Maps
        (Key_Type        => Types.String_Access,
         Element_Type    => Types.Skill_ID_T,
         Hash            => Hash,
         Equivalent_Keys => Skill.Equals.Equals,
         "="             => "=");

      function Hash_Tag is new Ada.Unchecked_Conversion(Ada.Tags.Tag, Ada.Containers.Hash_Type);
      use type Ada.Tags.Tag;
      use type Types.Pools.Pool;

      package Type_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Ada.Tags.Tag,
         Element_Type    => Types.Pools.Pool,
         Hash            => Hash_Tag,
         Equivalent_Keys => "=",
         "="             => "=");

      -- we need to pass a pointer to the map around
      type ID_Map is not null access IDs.Map;

      type Field_Type_T is new A1.Field_Type with record
         Types : Skill.Types.Pools.Type_Vector;
         Types_By_Tag : Type_Maps.Map;
      end record;

      type Field_Type is access all Field_Type_T;

      procedure Fix_Types (This : access Field_Type_T);

      function Boxed is new Ada.Unchecked_Conversion(Types.Annotation, Types.Box);
      function Unboxed is new Ada.Unchecked_Conversion(Types.Box, Types.Annotation);

      overriding
      function Read_Box(This : access Field_Type_T; Input : Streams.Reader.Sub_Stream) return Types.Box;

      overriding
      function Offset_Box(This : access Field_Type_T; Target : Types.Box) return Types.V64;

      overriding
      procedure Write_Box(This : access Field_Type_T; Output : Streams.Writer.Sub_Stream; Target : Types.Box);


      overriding
      function To_String(This : Field_Type_T) return String is
         ("annotation");

   end Annotation_Type_P;

   function Annotation (Types : Skill.Types.Pools.Type_Vector) return Annotation_Type_P.Field_Type is
     (new Annotation_Type_P.Field_Type_T'(Types         => types,
                                          Types_By_Tag => <>));

   function Offset_Single
     (Input : Boolean) return Types.V64 is
      (1);
   package Bool_Type_P is new Plain_Types (Boolean, 6, "bool", Streams.Reader.Bool, Streams.Writer.Bool);
   Bool : constant Field_Type := new Bool_Type_P.Field_Type;

   function Offset_Single
     (Input : Types.i8) return Types.V64 is
      (1);
   package I8_Type_P is new Plain_Types (T.I8, 7, "i8", Streams.Reader.I8, Streams.Writer.I8);
   I8 : constant Field_Type := new I8_Type_P.Field_Type;

   function Offset_Single
     (Input : Types.I16) return Types.V64 is
      (2);
   package I16_Type_P is new Plain_Types (T.I16, 8, "i16", Streams.Reader.I16, Streams.Writer.I16);
   I16 : constant Field_Type := new I16_Type_P.Field_Type;

   function Offset_Single
     (Input : Types.I32) return Types.V64 is
      (4);
   package I32_Type_P is new Plain_Types (T.I32, 9, "i32", Streams.Reader.I32, Streams.Writer.I32);
   I32 : constant Field_Type := new I32_Type_P.Field_Type;

   function Offset_Single
     (Input : Types.I64) return Types.V64 is
      (8);
   package I64_Type_P is new Plain_Types (T.I64, 10, "i64", Streams.Reader.I64, Streams.Writer.I64);
   I64 : constant Field_Type := new I64_Type_P.Field_Type;


   function Offset_Single_V64
     (Input : Types.V64) return Types.V64;
   package V64_Type_P is new Plain_Types (T.V64, 11, "v64", Streams.Reader.V64, Streams.Writer.V64, Offset_Single_V64);
   V64 : constant Field_Type := new V64_Type_P.Field_Type;


   function Offset_Single
     (Input : Types.F32) return Types.V64 is
      (4);
   package F32_Type_P is new Plain_Types (T.F32, 12, "f32", Streams.Reader.F32, Streams.Writer.F32);
   F32 : constant Field_Type := new F32_Type_P.Field_Type;

   function Offset_Single
     (Input : Types.F64) return Types.V64 is
      (8);
   package F64_Type_P is new Plain_Types (T.F64, 13, "f64", Streams.Reader.F64, Streams.Writer.F64);
   F64 : constant Field_Type := new F64_Type_P.Field_Type;


   package Const_Arrays_P is
      use type Types.Boxed_Array;
      package A1 is new Skill.Field_Types.Field_Types (Types.Boxed_Array, 15);


      type Field_Type_T is new A1.Field_Type with record
         Base : SKill.Field_Types.Field_Type;
         Length : T.V64;
      end record;
      type Field_Type is access all Field_Type_T;

      function Make (Base : Skill.Field_Types.Field_Type; Length : T.V64) return Field_Type is
        (new Field_Type_T'(Base => Base, Length => Length));


      function Boxed is new Ada.Unchecked_Conversion(Types.Boxed_Array, Types.Box);
      function Unboxed is new Ada.Unchecked_Conversion(Types.Box, Types.Boxed_Array);


      function Read_Box
        (This : access Field_Type_T;
         Input : Streams.Reader.Sub_Stream) return Types.Box;

      function Offset_Box
        (This : access Field_Type_T;
         Target : Types.Box) return Types.V64;

      procedure Write_Box
        (This : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream; Target : Types.Box);

      overriding
      function To_String (This : Field_Type_T) return String is
        (This.Base.To_String & "[" & Natural'Image(Natural(This.Length)) & "]");
   end Const_Arrays_P;

   function Const_Array
     (Length : Types.V64;
      Base : Skill.Field_Types.Field_Type)
      return Skill.Field_Types.Field_Type is
     (Skill.Field_Types.Field_Type(Const_Arrays_P.Make(Base => Base, Length => Length)));


   package Var_Arrays_P is
      use type Types.Boxed_Array;
      package A1 is new Skill.Field_Types.Field_Types (Types.Boxed_Array, 17);


      type Field_Type_T is new A1.Field_Type with record
         Base : SKill.Field_Types.Field_Type;
      end record;
      type Field_Type is access all Field_Type_T;

      function Make (Base : Skill.Field_Types.Field_Type) return Field_Type is
        (new Field_Type_T'(Base => Base));


      function Boxed is new Ada.Unchecked_Conversion(Types.Boxed_Array, Types.Box);
      function Unboxed is new Ada.Unchecked_Conversion(Types.Box, Types.Boxed_Array);


      function Read_Box
        (This : access Field_Type_T;
         Input : Streams.Reader.Sub_Stream) return Types.Box;

      function Offset_Box
        (This : access Field_Type_T;
         Target : Types.Box) return Types.V64;

      procedure Write_Box
        (This : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream; Target : Types.Box);

      overriding
      function To_String (This : Field_Type_T) return String is
        (This.Base.To_String & "[]");
   end Var_Arrays_P;

   function Var_Array
     (Base : Skill.Field_Types.Field_Type)
      return Skill.Field_Types.Field_Type is
     (Skill.Field_Types.Field_Type(Var_Arrays_P.Make(Base => Base)));


   package List_Type_P is
      use type Types.Boxed_Array;
      package A1 is new Skill.Field_Types.Field_Types (Types.Boxed_Array, 18);


      type Field_Type_T is new A1.Field_Type with record
         Base : SKill.Field_Types.Field_Type;
      end record;
      type Field_Type is access all Field_Type_T;

      function Make (Base : Skill.Field_Types.Field_Type) return Field_Type is
        (new Field_Type_T'(Base => Base));


      function Boxed is new Ada.Unchecked_Conversion(Types.Boxed_List, Types.Box);
      function Unboxed is new Ada.Unchecked_Conversion(Types.Box, Types.Boxed_List);


      function Read_Box
        (This : access Field_Type_T;
         Input : Streams.Reader.Sub_Stream) return Types.Box;

      function Offset_Box
        (This : access Field_Type_T;
         Target : Types.Box) return Types.V64;

      procedure Write_Box
        (This : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream; Target : Types.Box);

      overriding
      function To_String (This : Field_Type_T) return String is
        ("list<" & This.Base.To_String & ">");
   end List_Type_P;

   function List_Type
     (Base : Skill.Field_Types.Field_Type)
      return Skill.Field_Types.Field_Type is
     (Skill.Field_Types.Field_Type(List_Type_P.Make(Base => Base)));


   package Set_Type_P is
      use type Types.Boxed_Array;
      package A1 is new Skill.Field_Types.Field_Types (Types.Boxed_Array, 19);


      type Field_Type_T is new A1.Field_Type with record
         Base : SKill.Field_Types.Field_Type;
      end record;
      type Field_Type is access all Field_Type_T;

      function Make (Base : Skill.Field_Types.Field_Type) return Field_Type is
        (new Field_Type_T'(Base => Base));


      function Boxed is new Ada.Unchecked_Conversion(Types.Boxed_Set, Types.Box);
      function Unboxed is new Ada.Unchecked_Conversion(Types.Box, Types.Boxed_Set);


      function Read_Box
        (This : access Field_Type_T;
         Input : Streams.Reader.Sub_Stream) return Types.Box;

      function Offset_Box
        (This : access Field_Type_T;
         Target : Types.Box) return Types.V64;

      procedure Write_Box
        (This : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream; Target : Types.Box);

      overriding
      function To_String (This : Field_Type_T) return String is
        ("set<" & This.Base.To_String & ">");
   end Set_Type_P;

   function Set_Type
     (Base : Skill.Field_Types.Field_Type)
      return Skill.Field_Types.Field_Type is
     (Skill.Field_Types.Field_Type(Set_Type_P.Make(Base => Base)));


   package Map_Type_P is
      use type Types.Boxed_Array;
      package A1 is new Skill.Field_Types.Field_Types (Types.Boxed_Map, 20);


      type Field_Type_T is new A1.Field_Type with record
         Key : SKill.Field_Types.Field_Type;
         Value : SKill.Field_Types.Field_Type;
      end record;
      type Field_Type is access all Field_Type_T;

      function Make (K,V : Skill.Field_Types.Field_Type) return Field_Type is
        (new Field_Type_T'(Key => K, Value => V));


      function Boxed is new Ada.Unchecked_Conversion(Types.Boxed_Map, Types.Box);
      function Unboxed is new Ada.Unchecked_Conversion(Types.Box, Types.Boxed_Map);


      function Read_Box
        (This : access Field_Type_T;
         Input : Streams.Reader.Sub_Stream) return Types.Box;

      function Offset_Box
        (This : access Field_Type_T;
         Target : Types.Box) return Types.V64;

      procedure Write_Box
        (This : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream; Target : Types.Box);

      overriding
      function To_String (This : Field_Type_T) return String is
        ("map<" & This.Key.To_String & ", " & This.Value.To_String & ">");
   end Map_Type_P;

   function Map_Type
     (K,V : Skill.Field_Types.Field_Type)
      return Skill.Field_Types.Field_Type is
     (Skill.Field_Types.Field_Type(Map_Type_P.Make(K=>K,V=>V)));

end Skill.Field_Types.Builtin;
