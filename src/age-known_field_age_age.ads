--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Your SKilL Scala Binding                            --
-- \__ \ ' <| | | |__     <<debug>>                                           --
-- |___/_|\_\_|_|____|    by: <<some developer>>                              --
--                                                                            --

with Skill.Files;
with Skill.Field_Declarations;
with Skill.Field_Types;

package Age.Known_Field_Age_Age is

   type Known_Field_Age_Age_T is
     new Skill.Field_Declarations.Field_Declaration_T with private;
   type Known_Field_Age_Age is access Known_Field_Age_Age_T'Class;

   function Make
     (ID    : Natural;
      T     : Skill.Field_Types.Field_Type;
      Owner : Skill.Field_Declarations.Owner_T)
      return Skill.Field_Declarations.Field_Declaration;

   overriding
   procedure Free (This : access Known_Field_Age_Age_T);

private

   type Known_Field_Age_Age_T is new Skill.Field_Declarations
     .Field_Declaration_T with
   record
      null;
   end record;

end Age.Known_Field_Age_Age;
