--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     type handling in skill                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Ada.Unchecked_Conversion;

with Skill.Field_Types;
with Skill.Internal.Parts;

package body Skill.Types.Pools.Base is

   -- constructor invoked by new_pool
   function Make (Type_Id : Natural) return Pools.Pool is
      This : Pool :=
        new Pool_T'
          (Name        => Name,
           Type_Id     => Type_Id,
           Super       => null,
           Base        => null,
           Sub_Pools   => Sub_Pool_Vector.Empty_Vector,
           Data_Fields => Field_Types.Empty_Field_Array,
           Blocks      => new Skill.Internal.Parts.Blocks_P.Vector,
           Fixed       => False,
           Cached_Size => 0,
           Data        => Empty_Data,
           Owner       => null,
          Static_Data => new A1.Vector);
      function Convert is new Ada.Unchecked_Conversion
        (Source => Pool,
         Target => Base_Pool);
      function Convert is new Ada.Unchecked_Conversion
        (Source => Pool,
         Target => Pools.Pool);
   begin
      This.Base := Convert (This);
      return Convert (This);
   end Make;

   overriding function Insert_Instance
     (This : access Pool_T;
      ID   : Skill_ID_T)
     return Boolean
   is
      function Convert is new Ada.Unchecked_Conversion (Source => P,
                                                        Target => Annotation);

      I : Natural := Natural (Id) - 1;
      R : P;
   begin
        if null /= This.Data(I) then
         return false;
         end if;

      R := new T;
      R.Skill_ID := Id;
      This.Data (I) := Convert (R);
      This.Static_Data.Append(R);
      return true;
   end Insert_Instance;

end Skill.Types.Pools.Base;
