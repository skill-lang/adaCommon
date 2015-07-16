-- some dummy scratchpad tests

with Ada.Text_IO;

with Skill.Files;


procedure Tester is

   Sf : Skill.Files.File := Skill.Files.Open("testFiles/age.sf");

   procedure Print is
   begin
      null;
   end Print;

begin
   Print;
end Tester;

