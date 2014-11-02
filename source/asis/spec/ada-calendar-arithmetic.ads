------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

package Ada.Calendar.Arithmetic is

   --  Arithmetic on days:

   type Day_Count is
     range -366 * (1 + Year_Number'Last - Year_Number'First)
       ..  366 * (1 + Year_Number'Last - Year_Number'First);

   subtype Leap_Seconds_Count is Integer range -2047 .. 2047;

   procedure Difference (Left         : in     Time;
                         Right        : in     Time;
                         Days         :    out Day_Count;
                         Seconds      :    out Duration;
                         Leap_Seconds :    out Leap_Seconds_Count);

   function "+" (Left  : in Time;
                 Right : in Day_Count)
                return Time;

   function "+" (Left  : in Day_Count;
                 Right : in Time)
                return Time;

   function "-" (Left  : in Time;
                 Right : in Day_Count)
                return Time;

   function "-" (Left  : in Time;
                 Right : in Time)
                return Day_Count;

end Ada.Calendar.Arithmetic;
