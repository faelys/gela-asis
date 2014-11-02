------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

with Ada.Task_Identification; use Ada.Task_Identification;

generic
   type Attribute is private;
   Initial_Value : in Attribute;
package Ada.Task_Attributes is

   type Attribute_Handle is access all Attribute;

   function Value (T : Task_Id := Current_Task) return Attribute;

   function Reference (T : Task_Id := Current_Task) return Attribute_Handle;

   procedure Set_Value (Val : in Attribute;
                        T   : in Task_Id := Current_Task);

   procedure Reinitialize (T : in Task_Id := Current_Task);

end Ada.Task_Attributes;

