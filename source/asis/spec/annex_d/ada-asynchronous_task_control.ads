------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

with Ada.Task_Identification;

package Ada.Asynchronous_Task_Control is

   pragma Preelaborate (Asynchronous_Task_Control);

   procedure Hold (T : in Ada.Task_Identification.Task_Id);

   procedure Continue (T : in Ada.Task_Identification.Task_Id);

   function Is_Held (T : in Ada.Task_Identification.Task_Id) return Boolean;

end Ada.Asynchronous_Task_Control;
