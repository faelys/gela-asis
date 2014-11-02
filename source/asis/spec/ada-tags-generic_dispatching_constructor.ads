------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

generic
    type T (<>) is abstract tagged limited private;
    type Parameters (<>) is limited private;

    with function Constructor (Params : not null access Parameters)
           return T is abstract;

function Ada.Tags.Generic_Dispatching_Constructor
 (The_Tag : in Tag;
  Params  : not null access Parameters)
    return T'Class;
pragma Preelaborate (Ada.Tags.Generic_Dispatching_Constructor);
pragma Convention (Intrinsic, Ada.Tags.Generic_Dispatching_Constructor);
