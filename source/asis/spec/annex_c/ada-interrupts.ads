------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

with System;

package Ada.Interrupts is

   type Interrupt_ID is (Implementation_Defined);

   type Parameterless_Handler is access protected procedure;

   function Is_Reserved (Interrupt : in Interrupt_ID) return Boolean;

   function Is_Attached (Interrupt : in Interrupt_ID) return Boolean;

   function Current_Handler (Interrupt : in Interrupt_ID)
     return Parameterless_Handler;

   procedure Attach_Handler (New_Handler : in Parameterless_Handler;
                             Interrupt   : in Interrupt_ID);

   procedure Exchange_Handler (Old_Handler :    out Parameterless_Handler;
                               New_Handler : in     Parameterless_Handler;
                               Interrupt   : in     Interrupt_ID);

   procedure Detach_Handler (Interrupt : in Interrupt_ID);

   function Reference (Interrupt : in Interrupt_ID) return System.Address;

private

end Ada.Interrupts;
