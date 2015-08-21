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

package Skill.Field_Types.Builtin is

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
   end Constant_Types;

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


--      @Override
--      public SkillObject readSingleField(InStream in) {
--          final int t = (int) in.v64();
--          final long f = in.v64();
--          if (0 == t)
--              return null;
--          return types.get(t - 1).getByID(f);
--      }
--
--      @Override
--      public long calculateOffset(Collection<SkillObject> xs) {
--          long result = 0L;
--          for (SkillObject ref : xs) {
--              if (null == ref)
--                  result += 2;
--              else {
--                  if (ref instanceof NamedType)
--                      result += V64.singleV64Offset(((NamedType) ref).τPool().typeID() - 31);
--                 else
--                      result += V64
--                              .singleV64Offset(typeByName.get(ref.getClass().getSimpleName().toLowerCase()).typeID() - 31);
--
--                  result += V64.singleV64Offset(ref.getSkillID());
--              }
--          }
--
--          return result;
--      }
--
--      /**
--       * used for simple offset calculation
--       */
--      public long singleOffset(SkillObject ref) {
--          if (null == ref)
--              return 2L;
--
--          final long name;
--          if (ref instanceof NamedType)
--              name = V64.singleV64Offset(((NamedType) ref).τPool().typeID() - 31);
--         else
--              name = V64.singleV64Offset(typeByName.get(ref.getClass().getSimpleName().toLowerCase()).typeID() - 31);
--
--          return name + V64.singleV64Offset(ref.getSkillID());
--      }
--
--      @Override
--      public void writeSingleField(SkillObject ref, OutStream out) throws IOException {
--          if (null == ref) {
--              // magic trick!
--              out.i16((short) 0);
--              return;
--          }
--
--          if (ref instanceof NamedType)
--              out.v64(((NamedType) ref).τPool().typeID() - 31);
--         else
--              out.v64(typeByName.get(ref.getClass().getSimpleName().toLowerCase()).typeID() - 31);
--          out.v64(ref.getSkillID());
--
--      }

      overriding
      function To_String(This : Field_Type_T) return String is
         ("annotation");

   end Annotation_Type_P;

   function Annotation (Types : Skill.Types.Pools.Type_Vector) return Annotation_Type_P.Field_Type is
     (new Annotation_Type_P.Field_Type_T'(Types         => types,
                                          Types_By_Name => <>));

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

--  	@Override
--  	public String readSingleField(InStream in) {
--  		return strings.get(in.v64());
--  	}

--  	@Override
--  	public long calculateOffset(Collection<String> xs) {
--  		// shortcut for small string pools
--  		if (stringIDs.size() < 128)
--  			return xs.size();
--
--  		long result = 0L;
--  		for (String s : xs) {
--  			result += V64.singleV64Offset(stringIDs.get(s));
--  		}
--
--  		return result;
--  	}

--      public long singleOffset(String name) {
--  		return V64.singleV64Offset(stringIDs.get(name));
--  	}

      procedure Write_Single_Field (THis : access Field_Type_T; V : Types.String_Access; Output : Skill.Streams.Writer.Sub_Stream);

      function Get_Id_Map (THis : access Field_Type_T) return ID_Map;

      overriding function To_String (This : Field_Type_T) return String is
        ("string");
   end String_Type_T;

   function String_Type  (Strings : String_Pools.Pool) return String_Type_T.Field_Type is
     (new String_Type_T.Field_Type_T'(Strings, String_Type_T.Ids.Empty_Map));


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
