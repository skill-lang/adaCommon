-- some dummy scratchpad tests

with Ada.Text_IO;

with Skill.Errors;
with Skill.Types;
with Skill.Tasks;
with Skill.Synchronization;

with Age;
with Age.Api;

procedure Tester is

   use Skill;
   use type Skill.Types.String_Access;

   procedure Print (V : Natural) is
   begin
      Ada.Text_IO.Put_Line (Natural'Image (V));
   end Print;

   procedure Print (V : Types.v64) is
   begin
      Ada.Text_IO.Put_Line (Long_Integer'Image (Long_Integer(V)));
   end Print;

   M : Synchronization.Mutex;

   procedure Print is
   --        Sf : Age.Api.File := Age.Api.Open ("testFiles/ageUnrestricted.sf");
--        Sf : Age.Api.File := Age.Api.Open ("testFiles/age.sf");
      Sf : Age.Api.File := Age.Api.Open ("testFiles/emptyBlocks.sf");

--        S : Types.String_Access := Sf.Strings.Get(1);
   begin
      M.Lock;
--        Ada.Text_IO.Put_Line (S.all);

      Print (Sf.Ages.Size);

--        Print (Sf.Ages.Get (1).Get_Age);

--        Print (Sf.Ages.Get (2).Get_Age);
      M.Unlock;

      Sf.Close;
   end Print;

--     T : array(1 .. 100) of Tasks.Run (Print'Access);
begin
   Print;
--     for I in T'Range loop
--        T(I).Start;
--     end loop;
exception when E : others =>
      Skill.Errors.Print_Stacktrace (E);
end Tester;

