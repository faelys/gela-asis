------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

with Ada.Streams;

package Ada.Exceptions is

   pragma Preelaborate (Exceptions);

   type Exception_Id is private;
   pragma Preelaborable_Initialization (Exception_Id);

   Null_Id : constant Exception_Id;

   function Exception_Name (Id : in Exception_Id) return String;

   function Wide_Exception_Name (Id : in Exception_Id) return Wide_String;

   function Wide_Wide_Exception_Name (Id : in Exception_Id)
     return Wide_Wide_String;

   type Exception_Occurrence is limited private;
   pragma Preelaborable_Initialization (Exception_Occurrence);

   type Exception_Occurrence_Access is access all Exception_Occurrence;

   Null_Occurrence : constant Exception_Occurrence;

   procedure Raise_Exception (E       : in Exception_Id;
                              Message : in String := "");
   pragma No_Return (Raise_Exception);

   function Exception_Message (X : in Exception_Occurrence) return String;

   procedure Reraise_Occurrence (X : in Exception_Occurrence);

   function Exception_Identity (X : in Exception_Occurrence)
     return Exception_Id;

   function Exception_Name (X : in Exception_Occurrence) return String;
   --  Same as Exception_Name(Exception_Identity(X)).

   function Wide_Exception_Name (X : in Exception_Occurrence)
     return Wide_String;
   --  Same as Wide_Exception_Name(Exception_Identity(X)).

   function Wide_Wide_Exception_Name (X : in Exception_Occurrence)
     return Wide_Wide_String;
   --  Same as Wide_Wide_Exception_Name(Exception_Identity(X)).

   function Exception_Information (X : in Exception_Occurrence) return String;

   procedure Save_Occurrence (Target :    out Exception_Occurrence;
                              Source : in     Exception_Occurrence);

   function Save_Occurrence (Source : in Exception_Occurrence)
     return Exception_Occurrence_Access;

   procedure Read_Exception_Occurrence
    (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
     Item   : out Exception_Occurrence);

   procedure Write_Exception_Occurrence
    (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
     Item   : in Exception_Occurrence);

   for Exception_Occurrence'Read use Read_Exception_Occurrence;
   for Exception_Occurrence'Write use Write_Exception_Occurrence;

private

   pragma Import (Ada, Exception_Id);
   pragma Import (Ada, Exception_Occurrence);
   pragma Import (Ada, Null_Id);
   pragma Import (Ada, Null_Occurrence);

end Ada.Exceptions;
