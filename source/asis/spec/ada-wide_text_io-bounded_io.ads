------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

with Ada.Strings.Wide_Bounded;

generic
   with package Bounded is
     new Ada.Strings.Wide_Bounded.Generic_Bounded_Length (<>);
package Ada.Wide_Text_IO.Bounded_IO is

   procedure Put
     (File : in File_Type;
      Item : in Bounded.Wide_Bounded_String);

   procedure Put
     (Item : in Bounded.Wide_Bounded_String);

   procedure Put_Line
     (File : in File_Type;
      Item : in Bounded.Wide_Bounded_String);

   procedure Put_Line
     (Item : in Bounded.Wide_Bounded_String);

   function Get_Line
     (File : in File_Type)
     return Bounded.Wide_Bounded_String;

   function Get_Line
     return Bounded.Wide_Bounded_String;

   procedure Get_Line
     (File : in File_Type; Item : out Bounded.Wide_Bounded_String);

   procedure Get_Line
     (Item : out Bounded.Wide_Bounded_String);

end Ada.Wide_Text_IO.Bounded_IO;
