------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

with Ada.Task_Identification; -- See C.7.1
with System;

package Ada.Dynamic_Priorities is

   pragma Preelaborate (Dynamic_Priorities);

   procedure Set_Priority (Priority : in System.Any_Priority;
                           T        : in Ada.Task_Identification.Task_Id
                             := Ada.Task_Identification.Current_Task);

   function Get_Priority (T : in Ada.Task_Identification.Task_Id
                            := Ada.Task_Identification.Current_Task)
     return System.Any_Priority;

end Ada.Dynamic_Priorities;
