--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     type handling in skill                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

package Skill.Field_Types is
   pragma Preelaborate;


   type Field_Type_Base is abstract tagged null record;
   type Field_Type is access Field_Type_Base'Class;

   function ID (This : Field_Type_Base) return Natural is abstract;
   function To_String (This : Field_Type_Base) return String is abstract;

   generic
      type T is private;
      Type_Id : Natural;
   package Field_Types is
      type Field_Type is abstract new Field_Type_Base with null record;

      overriding function Id (This : Field_Type) return Natural is
        (Type_ID);
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


end Skill.Field_Types;