-- some dummy scratchpad tests

with Ada.Text_IO;

with Skill.Types;
with Skill.Types.Builtin_Field_Types;

with Skill.Hashes; use Skill.Hashes;


procedure Test is

   package T renames Skill.Types.Builtin_Field_Types;

   Ft : Skill.Types.Field_Type;

   procedure Print is
   begin
      Ada.Text_IO.Put_Line
        (Natural'Image (Ft.ID) & " corresponds to " & Ft.To_String);
      null;
   end Print;

begin
   Ft := T.Constant_I8;
   Print;
   Ft := T.Constant_I16;
   Print;
   Ft := T.Constant_I32;
   Print;
   Ft := T.Constant_I64;
   Print;
   Ft := T.Constant_V64;
   Print;
   Ft := T.Annotation;
   Print;
   Ft := T.Bool;
   Print;
   Ft := T.I8;
   Print;
   Ft := T.I16;
   Print;
   Ft := T.I32;
   Print;
   Ft := T.I64;
   Print;
   Ft := T.V64;
   Print;
   Ft := T.F32;
   Print;
   Ft := T.F64;
   Print;
   Ft := T.String_Type;
   Print;
end Test;
