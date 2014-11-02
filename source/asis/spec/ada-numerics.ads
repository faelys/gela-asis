------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

package Ada.Numerics is
   pragma Pure (Numerics);

   Argument_Error : exception;

   Pi : constant :=
     3.14159_26535_89793_23846_26433_83279_50288_41971_69399_37511;
   π  : constant := Pi;
   e  : constant :=
     2.71828_18284_59045_23536_02874_71352_66249_77572_47093_69996;
end Ada.Numerics;



