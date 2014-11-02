------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

with Ada.Containers;

generic
   with package Bounded is
     new Ada.Strings.Wide_Bounded.Generic_Bounded_Length (<>);

function Ada.Strings.Wide_Bounded.Wide_Hash
  (Key : in Bounded.Bounded_Wide_String)
  return Containers.Hash_Type;

pragma Preelaborate (Wide_Hash);
