------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

package Ada.Numerics.Float_Random is

   -- Basic facilities

   type Generator is limited private;

   subtype Uniformly_Distributed is Float range 0.0 .. 1.0;
   function Random (Gen : Generator) return Uniformly_Distributed;

   procedure Reset (Gen       : in Generator;
                    Initiator : in Integer);
   procedure Reset (Gen       : in Generator);

   -- Advanced facilities

   type State is private;

   procedure Save  (Gen        : in  Generator;
                    To_State   : out State);
   procedure Reset (Gen        : in  Generator;
                    From_State : in  State);

   Max_Image_Width : constant := implementation-defined;

   function Image (Of_State    : State)  return String;
   function Value (Coded_State : String) return State;

private

   pragma Import (Ada, State);
   pragma Import (Ada, Generator);

end Ada.Numerics.Float_Random;




