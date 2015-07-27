-- some dummy scratchpad tests

with Ada.Text_IO;

with Skill.Errors;
with Skill.Types;

with Age;
with Age.Api;

procedure Tester is

   use Skill;
   use type Skill.Types.String_Access;

   procedure Print ( N : Natural) is
   begin
      Ada.Text_IO.Put_Line (Natural'Image (N));
      end Print;

   procedure Print is
      Sf : Age.Api.File := Age.Api.Open ("testFiles/ageUnrestricted.sf");
--        Sf : Age.Api.File := Age.Api.Open ("testFiles/age16.sf");

      S : Types.String_Access := Sf.Strings.Get(1);
   begin
      Ada.Text_IO.Put_Line (S.all);

      Print(Sf.Ages.Size);

      Sf.Close;
   end Print;

begin
   Print;
exception when E : others =>
      Skill.Errors.Print_Stacktrace (E);
end Tester;

