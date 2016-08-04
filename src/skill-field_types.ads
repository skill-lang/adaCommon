--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     type handling in skill                              --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
pragma Ada_2012;

with Skill.Types;
with Skill.Streams.Reader;
with Skill.Streams.Writer;

package Skill.Field_Types is

   type Field_Type_Base is abstract tagged null record;
   type Field_Type is access all Field_Type_Base'Class;

   function ID (This : access Field_Type_Base) return Natural is abstract;
   function To_String (This : Field_Type_Base) return String is abstract;

   function Read_Box
     (This : access Field_Type_Base;
      Input : Streams.Reader.Sub_Stream) return Types.Box is abstract;

   function Offset_Box
     (This : access Field_Type_Base;
      Target : Types.Box) return Types.V64 is abstract;

   procedure Write_Box
     (This : access Field_Type_Base;
      Output : Streams.Writer.Sub_Stream; Target : Types.Box) is abstract;

   generic
      type T is private;
      Type_Id : Natural;
   package Field_Types is
      type Field_Type is abstract new Field_Type_Base with null record;

      overriding function Id (This : access Field_Type) return Natural is
        (Type_ID);
   end Field_Types;

end Skill.Field_Types;
