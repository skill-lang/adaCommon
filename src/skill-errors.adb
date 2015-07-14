--  ___ _  ___ _ _                                                            --
-- / __| |/ (_) | |       Common SKilL implementation                         --
-- \__ \ ' <| | | |__     error reporting                                     --
-- |___/_|\_\_|_|____|    by: Timm Felden                                     --
--                                                                            --
with Ada.Text_IO;
with Ada.Exceptions.Traceback;

with GNAT.Traceback;
with GNAT.Traceback.Symbolic;

package body Skill.Errors is

   procedure Print_Stacktrace is
      Trace  : GNAT.Traceback.Tracebacks_Array (1 .. 50);
      Length : Natural;
   begin
      GNAT.Traceback.Call_Chain (Trace, Length);
      Ada.Text_IO.Put_Line
        (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (2 .. Length)));
   end Print_Stacktrace;

   procedure Print_Stacktrace (E : in Ada.Exceptions.Exception_Occurrence) is
      Trace  : GNAT.Traceback.Tracebacks_Array := Ada.Exceptions.Traceback.Tracebacks(E);
   begin
      Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(E));
      Ada.Text_IO.Put_Line
        (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace));
   end Print_Stacktrace;

end Skill.Errors;
