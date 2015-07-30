--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     generic subpools                                    --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Files;
with Skill.Internal.File_Parsers;
with Skill.Types.Pools;
with Skill.Types;
with Skill.Types.Vectors;

-- generic sub pool packages
generic
   type T is new Skill_Object with private;
   type P is access T;

   with function To_P (This : Annotation) return P;

package Skill.Types.Pools.Sub is
   type Pool_T is new Sub_Pool_T with private;
   type Pool is access Pool_T;

   -- API methods
   function Get (This : access Pool_T; ID : Skill_ID_T) return P;

   ----------------------
   -- internal methods --
   ----------------------

   -- constructor invoked by new_pool
   function Make (Super   : Skill.Types.Pools.Pool;
      Type_Id : Natural;
                  Name    : String_Access) return Pools.Pool;
   -- destructor invoked by close
   procedure Free (This : access Pool_T);

   overriding function Add_Field
     (This : access Pool_T;
      ID   : Natural;
      T    : Field_Types.Field_Type;
      Name : String_Access) return Skill.Field_Declarations.Field_Declaration;

   overriding function Insert_Instance
     (This : access Pool_T;
      ID   : Skill_ID_T) return Boolean;

   overriding function Static_Size (This : access Pool_T) return Natural;

   -- applies F for each element in this
   --        procedure Foreach
   --          (This : access Pool_T;
   --           F    : access procedure (I : Age));

   function Make_Sub_Pool
     (This : access Pool_T;
      ID   : Natural;
      Name : String_Access) return Skill.Types.Pools.Pool is
      (Make (This.To_Pool, Id, Name));

private

   package A1 is new Vectors (Natural, P);
   subtype Instance_Vector is A1.Vector;

   type Pool_T is new Sub_Pool_T with record
      Static_Data : Instance_Vector;
      New_Objects : Instance_Vector;
   end record;

end Skill.Types.Pools.Sub;
