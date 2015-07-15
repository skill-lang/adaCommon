--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     type handling in skill                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

package Skill.Types is
   type String_Access is access String;
   type String_Access_Array is array(Integer range <>) of not null String_Access;

   generic
      type T is private;
      Type_Id : Natural;
   package Field_Types is
      type Field_Type is tagged record
         null;
      end record;
   end Field_Types;

   type Field_Declaration is tagged record
      null;
   end record;
   type Field_Array is array (Integer range <>) of Field_Declaration;
   type Field_Array_Access is not null access Field_Array;

   type Auto_Field is new Field_Declaration with record
      null;
   end record;
   type Auto_Field_Array is array (Integer range <>) of Auto_Field;


   -- any type that we will insert later on
   type Any is range 0 .. -1;
   -- any type that we will insert later on
   type Any_Ref is access Any;


end Skill.Types;
