--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     !! remove after integration into generator !!       --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --

with Skill.Files;

-- parametrization of field declarations
package Age.Internal is
   type Known_Field_Age_Age_T is tagged private;
   type Known_Field_Age_Age is access Known_Field_Age_Age_T'Class;

   -- dummy damit es durch den compiler geht; in wahrheit werden wir das alles
   -- generisch erben
   function Read return Boolean;

private

   type Known_Field_Age_Age_T is tagged null record;

end Age.Internal;
