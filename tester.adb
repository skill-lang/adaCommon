-- some dummy scratchpad tests

with Ada.Text_IO;

with Skill.Errors;
with Skill.Types;

with Age;
with Age.Api;

procedure Tester is

   use Skill;
   use type Skill.Types.String_Access;

   procedure Print is
      Sf : Age.Api.File := Age.Api.Open ("testFiles/ageUnrestricted.sf");

      S : Types.String_Access := Sf.Strings.Get(1);
   begin
         Ada.Text_IO.Put_Line (S.all);
   end Print;

begin
   Print;
exception when E : others =>
      Skill.Errors.Print_Stacktrace (E);
end Tester;

