------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

package Ada.Characters.Conversions is

   pragma Pure (Conversions);

   function Is_Character (Item : in Wide_Character) return Boolean;

   function Is_String (Item : in Wide_String) return Boolean;

   function Is_Character (Item : in Wide_Wide_Character) return Boolean;

   function Is_String (Item : in Wide_Wide_String) return Boolean;

   function Is_Wide_Character (Item : in Wide_Wide_Character) return Boolean;

   function Is_Wide_String (Item : in Wide_Wide_String) return Boolean;

   function To_Wide_Character (Item : in Character) return Wide_Character;

   function To_Wide_String (Item : in String) return Wide_String;

   function To_Wide_Wide_Character (Item : in Character)
                                   return Wide_Wide_Character;

   function To_Wide_Wide_String (Item : in String) return Wide_Wide_String;

   function To_Wide_Wide_Character (Item : in Wide_Character)
                                   return Wide_Wide_Character;

   function To_Wide_Wide_String (Item : in Wide_String)
                                return Wide_Wide_String;

   function To_Character (Item       : in Wide_Character;
                          Substitute : in Character := ' ')
                         return Character;

   function To_String (Item       : in Wide_String;
                       Substitute : in Character := ' ')
                      return String;

   function To_Character (Item       : in Wide_Wide_Character;
                          Substitute : in Character := ' ')
                         return Character;

   function To_String (Item       : in Wide_Wide_String;
                       Substitute : in Character := ' ')
                      return String;

   function To_Wide_Character (Item       : in Wide_Wide_Character;
                               Substitute : in Wide_Character := ' ')
                              return Wide_Character;

   function To_Wide_String (Item       : in Wide_Wide_String;
                            Substitute : in Wide_Character := ' ')
                           return Wide_String;

end Ada.Characters.Conversions;
