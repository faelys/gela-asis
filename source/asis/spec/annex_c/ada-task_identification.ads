------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

package Ada.Task_Identification is
   pragma Preelaborate(Task_Identification);

   type Task_Id is private;
   pragma Preelaborable_Initialization (Task_Id);

   Null_Task_Id : constant Task_Id;

   function  "=" (Left, Right : Task_Id) return Boolean;

   function  Image        (T : Task_Id) return String;

   function  Current_Task return Task_Id;
   procedure Abort_Task   (T : in out Task_Id);

   function  Is_Terminated (T : Task_Id) return Boolean;
   function  Is_Callable   (T : Task_Id) return Boolean;
private
   pragma Import (Ada, Task_Id);
   pragma Import (Ada, Null_Task_Id);
end Ada.Task_Identification;
