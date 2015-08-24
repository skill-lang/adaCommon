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
with Skill.Equals; use Skill.Equals;
with Skill.String_Pools;
with Skill.Streams;
with Skill.Streams.Writer;
with Skill.Types.Pools;
with Ada.Unchecked_Conversion;

package Skill.Field_Types.Builtin is

   generic
      type T is private;
      Type_Id : Natural;
      Image : String;
   package Constant_Types is

      package A1 is new Field_Types (T, Type_Id);

      type Field_Type is new A1.Field_Type with record
         Value : T;
      end record;

      overriding function To_String (This : Field_Type) return String is
        (Image);

      overriding
      function Read_Box
        (This : access Field_Type;
         Input : Streams.Reader.Sub_Stream) return Types.Box is
         (raise Constraint_Error with "can not read a constant!");

      overriding
      function Offset_Box
        (This : access Field_Type;
         Target : Types.Box) return Types.V64 is
        (0);

      overriding
      procedure Write_Box
        (This : access Field_Type;
         Output : Streams.Writer.Sub_Stream;
         Target : Types.Box) is null;

   end Constant_Types;

   generic
      type T is private;
      Type_Id : Natural;
      Image : String;
      with function Read_Single
        (Input : access Streams.Reader.Abstract_Stream'Class) return T;
      with procedure Write_Single
        (This : access Streams.Writer.Sub_Stream_T; V : T);
      with function Offset_Single
        (Input : T) return Types.v64 is <>;
   package Plain_Types is
      package A1 is new Field_Types (T, Type_Id);

      type Field_Type is new A1.Field_Type with null record;

      overriding function To_String (This : Field_Type) return String is
        (Image);


      function Boxed is new Ada.Unchecked_Conversion(T, Types.Box);
      function Unboxed is new Ada.Unchecked_Conversion(Types.Box, T);

      overriding
      function Read_Box
        (This : access Field_Type;
         Input : Streams.Reader.Sub_Stream) return Types.Box is
        (Boxed(Read_Single(Input)));

      overriding
      function Offset_Box
        (This : access Field_Type;
         Target : Types.Box) return Types.V64 is
        (Offset_Single(Unboxed(Target)));

      overriding
      procedure Write_Box
        (This : access Field_Type;
         Output : Streams.Writer.Sub_Stream;
         Target : Types.Box);

   end Plain_Types;

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
      package A1 is new Field_Types (Types.Annotation, 5);
      package IDs is new Ada.Containers.Hashed_Maps (Key_Type        => Types.String_Access,
                                                     Element_Type    => Types.Skill_ID_T,
                                                     Hash            => Hash,
                                                     Equivalent_Keys => Skill.Equals.Equals,
                                                     "="             => "=");

      -- we need to pass a pointer to the map around
      type ID_Map is not null access IDs.Map;

      type Field_Type_T is new A1.Field_Type with record
         Types : Skill.Types.Pools.Type_Vector;
         Types_By_Name : Skill.Types.Pools.Type_Map;
      end record;

      type Field_Type is access Field_Type_T;

      procedure Fix_Types (This : access Field_Type_T; Tbn : Types.Pools.Type_Map);

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
                                          Types_By_Name => <>));

   function Offset_Single
     (Input : Boolean) return Types.V64 is
      (1);
   package A9 is new Plain_Types (Boolean, 6, "bool", Streams.Reader.Bool, Streams.Writer.Bool);
   Bool : constant Field_Type := new A9.Field_Type;

   function Offset_Single
     (Input : Types.i8) return Types.V64 is
      (1);
   package A11 is new Plain_Types (T.I8, 7, "i8", Streams.Reader.I8, Streams.Writer.I8);
   I8 : constant Field_Type := new A11.Field_Type;

   function Offset_Single
     (Input : Types.I16) return Types.V64 is
      (2);
   package A21 is new Plain_Types (T.I16, 8, "i16", Streams.Reader.I16, Streams.Writer.I16);
   I16 : constant Field_Type := new A21.Field_Type;

   function Offset_Single
     (Input : Types.I32) return Types.V64 is
      (4);
   package A31 is new Plain_Types (T.I32, 9, "i32", Streams.Reader.I32, Streams.Writer.I32);
   I32 : constant Field_Type := new A31.Field_Type;

   function Offset_Single
     (Input : Types.I64) return Types.V64 is
      (8);
   package A41 is new Plain_Types (T.I64, 10, "i64", Streams.Reader.I64, Streams.Writer.I64);
   I64 : constant Field_Type := new A41.Field_Type;


   function Offset_Single_V64
     (Input : Types.V64) return Types.V64;
   package A51 is new Plain_Types (T.V64, 11, "v64", Streams.Reader.V64, Streams.Writer.V64, Offset_Single_V64);
   V64 : constant Field_Type := new A51.Field_Type;


   function Offset_Single
     (Input : Types.F32) return Types.V64 is
      (4);
   package A32 is new Plain_Types (T.F32, 12, "f32", Streams.Reader.F32, Streams.Writer.F32);
   F32 : constant Field_Type := new A32.Field_Type;

   function Offset_Single
     (Input : Types.F64) return Types.V64 is
      (8);
   package A42 is new Plain_Types (T.F64, 13, "f64", Streams.Reader.F64, Streams.Writer.F64);
   F64 : constant Field_Type := new A42.Field_Type;


   package String_Type_T is
      package A1 is new Field_Types (Types.String_Access, 14);
      package IDs is new Ada.Containers.Hashed_Maps (Key_Type        => Types.String_Access,
                                                     Element_Type    => Types.Skill_ID_T,
                                                     Hash            => Hash,
                                                     Equivalent_Keys => Skill.Equals.Equals,
                                                     "="             => "=");

      -- we need to pass a pointer to the map around
      type ID_Map is not null access all IDs.Map;

      type Field_Type_T is new A1.Field_Type with record
         Strings : Skill.String_Pools.Pool;
         String_IDs : aliased IDs.Map;
      end record;

      type Field_Type is access all Field_Type_T;


      function Boxed is new Ada.Unchecked_Conversion(Types.String_Access, Types.Box);
      function Unboxed is new Ada.Unchecked_Conversion(Types.Box, Types.String_Access);


      function Read_Box
        (This : access Field_Type_T;
         Input : Streams.Reader.Sub_Stream) return Types.Box is
        (Boxed(This.Strings.Get (Input.V64)));

      function Offset_Box
        (This : access Field_Type_T;
         Target : Types.Box) return Types.V64 is
         (Offset_Single_V64(Types.V64(This.String_Ids.Element(Unboxed(Target)))));

      procedure Write_Box
        (This : access Field_Type_T;
         Output : Streams.Writer.Sub_Stream; Target : Types.Box);

      procedure Write_Single_Field (THis : access Field_Type_T; V : Types.String_Access; Output : Skill.Streams.Writer.Sub_Stream);

      function Get_Id_Map (THis : access Field_Type_T) return ID_Map;

      overriding function To_String (This : Field_Type_T) return String is
        ("string");
   end String_Type_T;

   function String_Type  (Strings : String_Pools.Pool) return String_Type_T.Field_Type is
     (new String_Type_T.Field_Type_T'(Strings, String_Type_T.Ids.Empty_Map));


   generic
      type Collection is private;
      Id : Natural;
      Collection_Name : String;
   package Single_Argument_Type_P is
      package A1 is new Skill.Field_Types.Field_Types (Collection, Id);


      type Field_Type_T is new A1.Field_Type with record
         Base : SKill.Field_Types.Field_Type;
      end record;
      type Field_Type is access all Field_Type_T;

      function Make (Base : Skill.Field_Types.Field_Type) return Field_Type is
         (new Field_Type_T'(Base => Base));


      function Boxed is new Ada.Unchecked_Conversion(Collection, Types.Box);
      function Unboxed is new Ada.Unchecked_Conversion(Types.Box, Collection);


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
        (Collection_Name & "(" & This.Base.To_String & ")");
   end Single_Argument_Type_P;

   package Var_Arrays_P is new Single_Argument_Type_P(Types.Boxed_Array, 17, "array");
   function Var_Array (Base : Skill.Field_Types.Field_Type) return Skill.Field_Types.Field_Type is
      (Skill.Field_Types.Field_Type(Var_Arrays_P.Make(Base => Base)));

--     function String_Type  (Strings : String_Pools.Pool) return String_Type_T.Field_Type is
--       (new String_Type_T.Field_Type_T'(Strings, String_Type_T.Ids.Empty_Map));
--     generic
--        type Base_T is private;
--        Base  : Field_Type;
--        Size : Natural;
--     package Array_Types is
--
--        type T is array(Natural range 0..Size-1) of Base_T;
--
--        package A1 is new Field_Types (T, 15);
--
--        type Field_Type is new A1.Field_Type with null record;
--
--        overriding function To_String (This : Field_Type) return String is
--          (Base.To_String & "["& Natural'Image(Size) &"]");
--     end Array_Types;
--
--
--     generic
--        type Base_T is private;
--        Base  : Field_Type;
--     package List_Types is
--
--        package A2 is new Ada.Containers.Doubly_Linked_Lists (Base_T, "=");
--
--        subtype T is A2.List;
--
--        package A1 is new Field_Types (T, 18);
--
--        type Field_Type is new A1.Field_Type with null record;
--
--        overriding function To_String (This : Field_Type) return String is
--          ("list<" & Base.To_String & ">");
--     end List_Types;
--
--     generic
--        type Base_T is private;
--        with function Hash (Element : Base_T) return Ada.Containers.Hash_Type;
--        Base  : Field_Type;
--     package Set_Types is
--
--        package A2 is new Ada.Containers.Hashed_Sets (Base_T, Hash, "=");
--
--        subtype T is A2.Set;
--
--        package A1 is new Field_Types (T, 19);
--
--        type Field_Type is new A1.Field_Type with null record;
--
--        overriding function To_String (This : Field_Type) return String is
--          ("set<" & Base.To_String & ">");
--     end Set_Types;
--
--     generic
--        type Key_T is private;
--        with function Hash (Element : Key_T) return Ada.Containers.Hash_Type;
--        type Value_T is private;
--        Key, Value  : Field_Type;
--     package Map_Types is
--
--        package A2 is new Ada.Containers.Hashed_Maps (Key_T, Value_T, Hash, "=");
--
--        subtype T is A2.Map;
--
--        package A1 is new Field_Types (T, 20);
--
--        type Field_Type is new A1.Field_Type with null record;
--
--        overriding function To_String (This : Field_Type) return String is
--          ("map<" & Key.To_String & ", "& Value.To_String & ">");
--     end Map_Types;

end Skill.Field_Types.Builtin;
