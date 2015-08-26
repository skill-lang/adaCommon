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

      type Field_Type is access Field_Type_T;

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

--     generic
--        type Collection is private;
--        Id : Natural;
--        Collection_Name : String;
--
--        with function New_Collection return Collection is <>;
--        with procedure Insert(This : in out Collection; E : in Types.Box) is <>;
--        with function Length (This : Collection) return Ada.Containers.Count_Type is <>;
--        with function Element (This : Collection; Idx : Ada.Containers.Count_Type) return Types.Box is <>;
--     package Single_Argument_Type_P is
--        package A1 is new Skill.Field_Types.Field_Types (Collection, Id);
--
--
--        type Field_Type_T is new A1.Field_Type with record
--           Base : SKill.Field_Types.Field_Type;
--        end record;
--        type Field_Type is access all Field_Type_T;
--
--        function Make (Base : Skill.Field_Types.Field_Type) return Field_Type is
--           (new Field_Type_T'(Base => Base));
--
--
--        function Boxed is new Ada.Unchecked_Conversion(Collection, Types.Box);
--        function Unboxed is new Ada.Unchecked_Conversion(Types.Box, Collection);
--
--
--        function Read_Box
--          (This : access Field_Type_T;
--           Input : Streams.Reader.Sub_Stream) return Types.Box;
--
--        function Offset_Box
--          (This : access Field_Type_T;
--           Target : Types.Box) return Types.V64;
--
--        procedure Write_Box
--          (This : access Field_Type_T;
--           Output : Streams.Writer.Sub_Stream; Target : Types.Box);
--
--        overriding
--        function To_String (This : Field_Type_T) return String is
--          (Collection_Name & "(" & This.Base.To_String & ")");
--     end Single_Argument_Type_P;


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


   -- single container function for lists
--     use type Types.Boxed_List;
--     function New_Collection return Types.Boxed_List is
--       (Types.Lists_P.Empty_List);
--     procedure Insert(This : in out Types.Boxed_List; E : in Types.Box);
--     function Length (This : Types.Boxed_List) return Ada.Containers.Count_Type is
--       (This.Length);
--     function Element (This : Types.Boxed_List; Idx : Ada.Containers.Count_Type) return Types.Box is
--       (This.
--        .Element(Integer(Idx)));
--
--     package List_Type_P is new Single_Argument_Type_P
--       (Collection => Types.Boxed_List,
--        Id => 18,
--        Collection_Name => "list"
--       );
--     function List_Type
--       (Base : Skill.Field_Types.Field_Type)
--        return Skill.Field_Types.Field_Type is
--       (Skill.Field_Types.Field_Type(List_Type_P.Make(Base => Base)));

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
