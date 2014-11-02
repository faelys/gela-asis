------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

with Ada.Real_Time;
with Ada.Task_Identification;

package Ada.Dispatching.EDF is

   subtype Deadline is Ada.Real_Time.Time;

   Default_Deadline : constant Deadline := Ada.Real_Time.Time_Last;

   procedure Set_Deadline (D : in Deadline;
                           T : in Ada.Task_Identification.Task_Id
                             := Ada.Task_Identification.Current_Task);

   procedure Delay_Until_And_Set_Deadline
    (Delay_Until_Time : in Ada.Real_Time.Time;
     Deadline_Offset  : in Ada.Real_Time.Time_Span);

   function Get_Deadline (T : Ada.Task_Identification.Task_Id
                            := Ada.Task_Identification.Current_Task)
     return Deadline;

end Ada.Dispatching.EDF;
