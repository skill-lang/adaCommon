--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     unknown base pools                                  --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Files;
with Skill.Internal.File_Parsers;
with Skill.Types.Pools;
with Skill.Types;
with Skill.Containers.Vectors;
with Skill.Types.Pools.Sub;

-- instantiated pool packages
-- GNAT Bug workaround; should be "new Base(Annotation...)" instead
package Skill.Types.Pools.Unknown_Base is

   type Pool_T is new Base_Pool_T with private;
   type Pool is access Pool_T;

   -- API methods
   function Get (This : access Pool_T; ID : Skill_ID_T) return Annotation;

   ----------------------
   -- internal methods --
   ----------------------

   -- constructor invoked by new_pool
   function Make (Type_Id : Natural; Name : String_Access) return Pools.Pool;
   -- destructor invoked by close
   procedure Free (This : access Pool_T);

   overriding function Add_Field
     (This : access Pool_T;
      ID   : Natural;
      T    : Field_Types.Field_Type;
      Name : String_Access) return Skill.Field_Declarations.Field_Declaration;

   procedure Add_Known_Field
     (This            : access Pool_T;
      Name            : String_Access;
      String_Type     : Field_Types.Builtin.String_Type_T.Field_Type;
      Annotation_Type : Field_Types.Builtin.Annotation_Type_P
        .Field_Type) is null;

   overriding procedure Resize_Pool
     (This       : access Pool_T;
      Targets    : Type_Vector;
      Self_Index : Natural) is null;

   overriding function Static_Size (This : access Pool_T) return Natural;

   -- applies F for each element in this
   --        procedure Foreach
   --          (This : access Pool_T;
   --           F    : access procedure (I : Age));

   function Cast_Annotation (This : Annotation) return Annotation is (This);
   pragma Inline (Cast_Annotation);

   package Sub_Pools is new Pools.Sub
     (Skill_Object,
      Annotation,
      Cast_Annotation);

   function Make_Sub_Pool
     (This : access Pool_T;
      ID   : Natural;
      Name : String_Access) return Skill.Types.Pools.Pool is
     (Sub_Pools.Make (This.To_Pool, ID, Name));

   procedure Do_For_Static_Instances
     (This : access Pool_T;
      F    : access procedure (I : Annotation)) is null;

   procedure Update_After_Compress
     (This     : access Pool_T;
      Lbpo_Map : Skill.Internal.Lbpo_Map_T) is null;

private

   package A1 is new Containers.Vectors (Natural, Annotation);
   subtype Instance_Vector is A1.Vector;

   type Pool_T is new Base_Pool_T with record
      Static_Data : Instance_Vector;
      New_Objects : Instance_Vector;
   end record;

end Skill.Types.Pools.Unknown_Base;
