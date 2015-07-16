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

package Skill.Field_Types.Builtin is
   pragma Preelaborate;

   generic
      type T is private;
      Type_Id : Natural;
      Image : String;
   package Plain_Types is

      package A1 is new Field_Types (T, Type_Id);

      type Field_Type is new A1.Field_Type with null record;

      overriding function To_String (This : Field_Type) return String is
        (Image);
   end Plain_Types;

   package T renames Skill.Types;

   package A1 is new Plain_Types (T.I8, 0, "constant i8");
   Constant_I8 : constant Field_Type := new A1.Field_Type;

   package A2 is new Plain_Types (T.I16, 1, "constant i16");
   Constant_I16 : constant Field_Type := new A2.Field_Type;

   package A3 is new Plain_Types (T.I32, 2, "constant i32");
   Constant_I32 : constant Field_Type := new A3.Field_Type;

   package A4 is new Plain_Types (T.I64, 3, "constant i64");
   Constant_I64 : constant Field_Type := new A4.Field_Type;

   package A5 is new Plain_Types (T.V64, 4, "constant v64");
   Constant_V64 : constant Field_Type := new A5.Field_Type;


   package A8 is new Plain_Types (T.Annotation, 5, "annotation");
   Annotation : constant Field_Type := new A8.Field_Type;

   package A9 is new Plain_Types (Boolean, 6, "bool");
   Bool : constant Field_Type := new A9.Field_Type;


   package A11 is new Plain_Types (T.I8, 7, "i8");
   I8 : constant Field_Type := new A11.Field_Type;

   package A21 is new Plain_Types (T.I16, 8, "i16");
   I16 : constant Field_Type := new A21.Field_Type;

   package A31 is new Plain_Types (T.I32, 9, "i32");
   I32 : constant Field_Type := new A31.Field_Type;

   package A41 is new Plain_Types (T.I64, 10, "i64");
   I64 : constant Field_Type := new A41.Field_Type;

   package A51 is new Plain_Types (T.V64, 11, "v64");
   V64 : constant Field_Type := new A51.Field_Type;


   package A32 is new Plain_Types (T.I32, 12, "f32");
   F32 : constant Field_Type := new A32.Field_Type;

   package A42 is new Plain_Types (T.I64, 13, "f64");
   F64 : constant Field_Type := new A42.Field_Type;


   package String_Type_T is new Plain_Types (T.String_Access, 14, "string");
   String_Type : constant Field_Type := new String_Type_T.Field_Type;


   generic
      type Base_T is private;
      Base  : Field_Type;
      Size : Natural;
   package Array_Types is

      type T is array(Natural range 0..Size-1) of Base_T;

      package A1 is new Field_Types (T, 15);

      type Field_Type is new A1.Field_Type with null record;

      overriding function To_String (This : Field_Type) return String is
        (Base.To_String & "["& Natural'Image(Size) &"]");
   end Array_Types;

   generic
      type Base_T is private;
      Base  : Field_Type;
   package Var_Array_Types is

      package A2 is new Ada.Containers.Vectors (Natural, Base_T);

      subtype T is A2.Vector;

      package A1 is new Field_Types (T, 17);

      type Field_Type is new A1.Field_Type with null record;

      overriding function To_String (This : Field_Type) return String is
        (Base.To_String & "[]");
   end Var_Array_Types;

   generic
      type Base_T is private;
      Base  : Field_Type;
   package List_Types is

      package A2 is new Ada.Containers.Doubly_Linked_Lists (Base_T, "=");

      subtype T is A2.List;

      package A1 is new Field_Types (T, 18);

      type Field_Type is new A1.Field_Type with null record;

      overriding function To_String (This : Field_Type) return String is
        ("list<" & Base.To_String & ">");
   end List_Types;

   generic
      type Base_T is private;
      with function Hash (Element : Base_T) return Ada.Containers.Hash_Type;
      Base  : Field_Type;
   package Set_Types is

      package A2 is new Ada.Containers.Hashed_Sets (Base_T, Hash, "=");

      subtype T is A2.Set;

      package A1 is new Field_Types (T, 19);

      type Field_Type is new A1.Field_Type with null record;

      overriding function To_String (This : Field_Type) return String is
        ("set<" & Base.To_String & ">");
   end Set_Types;

   generic
      type Key_T is private;
      with function Hash (Element : Key_T) return Ada.Containers.Hash_Type;
      type Value_T is private;
      Key, Value  : Field_Type;
   package Map_Types is

      package A2 is new Ada.Containers.Hashed_Maps (Key_T, Value_T, Hash, "=");

      subtype T is A2.Map;

      package A1 is new Field_Types (T, 20);

      type Field_Type is new A1.Field_Type with null record;

      overriding function To_String (This : Field_Type) return String is
        ("map<" & Key.To_String & ", "& Value.To_String & ">");
   end Map_Types;

end Skill.Field_Types.Builtin;
