------------------------------------------------------------------------------
--              A d a   r u n - t i m e   s p e c i f i c a t i o n         --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--          Read copyright and license at the end of ada.ads file           --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

package Ada.Strings.Wide_Wide_Maps is
   pragma Preelaborate(Wide_Wide_Maps);

   --  Representation for a set of Wide_Wide_Character values:
   type Wide_Wide_Character_Set is private;
   pragma Preelaborable_Initialization(Wide_Wide_Character_Set);

   Null_Set : constant Wide_Wide_Character_Set;

   type Wide_Wide_Character_Range is
      record
         Low  : Wide_Wide_Character;
         High : Wide_Wide_Character;
      end record;
   --  Represents Wide_Wide_Character range Low..High

   type Wide_Wide_Character_Ranges is array (Positive range <>)
     of Wide_Wide_Character_Range;

   function To_Set (Ranges : in Wide_Wide_Character_Ranges)
                   return Wide_Wide_Character_Set;

   function To_Set (Span : in Wide_Wide_Character_Range)
                   return Wide_Wide_Character_Set;

   function To_Ranges (Set : in Wide_Wide_Character_Set)
                      return Wide_Wide_Character_Ranges;

   function "=" (Left, Right : in Wide_Wide_Character_Set) return Boolean;

   function "not" (Right : in Wide_Wide_Character_Set)
                  return Wide_Wide_Character_Set;
   function "and" (Left, Right : in Wide_Wide_Character_Set)
                  return Wide_Wide_Character_Set;
   function "or" (Left, Right : in Wide_Wide_Character_Set)
                 return Wide_Wide_Character_Set;
   function "xor" (Left, Right : in Wide_Wide_Character_Set)
                  return Wide_Wide_Character_Set;
   function "-" (Left, Right : in Wide_Wide_Character_Set)
                return Wide_Wide_Character_Set;

   function Is_In (Element : in Wide_Wide_Character;
                   Set     : in Wide_Wide_Character_Set)
                  return Boolean;

   function Is_Subset (Elements : in Wide_Wide_Character_Set;
                       Set      : in Wide_Wide_Character_Set)
                      return Boolean;

   function "<=" (Left  : in Wide_Wide_Character_Set;
                  Right : in Wide_Wide_Character_Set)
                 return Boolean renames Is_Subset;

   -- Alternative representation for a set of Wide_Wide_Character values:
   subtype Wide_Wide_Character_Sequence is Wide_Wide_String;

   function To_Set (Sequence : in Wide_Wide_Character_Sequence)
                   return Wide_Wide_Character_Set;

   function To_Set (Singleton : in Wide_Wide_Character)
                   return Wide_Wide_Character_Set;

   function To_Sequence (Set : in Wide_Wide_Character_Set)
                        return Wide_Wide_Character_Sequence;

   -- Representation for a Wide_Wide_Character to Wide_Wide_Character
   -- mapping:
   type Wide_Wide_Character_Mapping is private;
   pragma Preelaborable_Initialization(Wide_Wide_Character_Mapping);

   function Value (Map     : in Wide_Wide_Character_Mapping;
                   Element : in Wide_Wide_Character)
                  return Wide_Wide_Character;

   Identity : constant Wide_Wide_Character_Mapping;

   function To_Mapping (From, To : in Wide_Wide_Character_Sequence)
                       return Wide_Wide_Character_Mapping;

   function To_Domain (Map : in Wide_Wide_Character_Mapping)
                      return Wide_Wide_Character_Sequence;

   function To_Range (Map : in Wide_Wide_Character_Mapping)
                     return Wide_Wide_Character_Sequence;

   type Wide_Wide_Character_Mapping_Function is
     access function (From : in Wide_Wide_Character)
                     return Wide_Wide_Character;

private
   pragma Import (Ada, Wide_Wide_Character_Set);
   pragma Import (Ada, Null_Set);
   pragma Import (Ada, Wide_Wide_Character_Mapping);
   pragma Import (Ada, Identity);
end Ada.Strings.Wide_Wide_Maps;
