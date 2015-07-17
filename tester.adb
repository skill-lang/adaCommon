-- some dummy scratchpad tests

with Ada.Text_IO;

with Skill.Files;
with Skill.Errors;
with Skill.Types;

procedure Tester is

   use Skill;
   use type Skill.Types.String_Access;

   procedure Print is
      Sf : Skill.Files.File := Skill.Files.Open ("testFiles/aircraft.sf");

      S : Types.String_Access := Sf.Strings.Get(2);
   begin
      if null = S then
         Ada.Text_IO.Put_Line ("<<null>>");
      else
         Ada.Text_IO.Put_Line (S.all);
      end if;
   end Print;

begin
   Print;
exception when E : others =>
      Skill.Errors.Print_Stacktrace (E);
end Tester;

