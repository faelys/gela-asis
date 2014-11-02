with Asis;
with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;
with Asis.Ada_Environments;
with Asis.Compilation_Units;

with Ada.Exceptions;
with Ada.Wide_Text_IO;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Characters.Handling;  use Ada.Characters.Handling;

procedure Asis_Hello_World is

   My_Context            : Asis.Context;
   My_Context_Name       : constant Wide_String :=
     Asis.Ada_Environments.Default_Name;
   My_Context_Parameters : constant Wide_String :=
     To_Wide_String (Argument (1));
   Initialization_Parameters : constant Wide_String := "";
   Finalization_Parameters   : constant Wide_String := "";

begin
   Asis.Implementation.Initialize     (Initialization_Parameters);

   Asis.Ada_Environments.Associate
     (The_Context => My_Context,
      Name        => My_Context_Name,
      Parameters  => My_Context_Parameters);

   Asis.Ada_Environments.Open         (My_Context);

   declare
      use Asis.Compilation_Units;
      Units : Asis.Compilation_Unit_List := Compilation_Units (My_Context);
   begin
      for J in Units'Range loop
         Ada.Wide_Text_IO.Put_Line
           (Text_Name (Units (J)) & " => " & Unit_Full_Name (Units (J)));
      end loop;
   end;

   Asis.Ada_Environments.Close        (My_Context);
   Asis.Ada_Environments.Dissociate   (My_Context);
   Asis.Implementation.Finalize       (Finalization_Parameters);

   Set_Exit_Status (Success);

exception

   when E : Asis.Exceptions.ASIS_Inappropriate_Context          |
            Asis.Exceptions.ASIS_Inappropriate_Container        |
            Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
            Asis.Exceptions.ASIS_Inappropriate_Element          |
            Asis.Exceptions.ASIS_Inappropriate_Line             |
            Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
            Asis.Exceptions.ASIS_Failed                         =>

      Ada.Wide_Text_IO.Put_Line
        ("ASIS exception (" &
         To_Wide_String (Ada.Exceptions.Exception_Name (E)) &
         ") is raised");

      Ada.Wide_Text_IO.Put_Line
        ("ASIS Error Status is " &
         Asis.Errors.Error_Kinds'Wide_Image (Asis.Implementation.Status));

      Ada.Wide_Text_IO.Put_Line ("ASIS Diagnosis is ");
      Ada.Wide_Text_IO.Put_Line (Asis.Implementation.Diagnosis);

      Asis.Implementation.Set_Status;
end Asis_Hello_World;
