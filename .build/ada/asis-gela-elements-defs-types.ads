
------------------------------------------------------------------------------
--  Copyright (c) 2006-2013, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the Maxim Reznik, IE nor the names of its
--       contributors may be used to endorse or promote products derived from
--       this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------
  
package Asis.Gela.Elements.Defs.Types is

   -----------------------
   -- Derived_Type_Node --
   -----------------------

   type Derived_Type_Node is 
      new Type_Definition_Node with private;

   type Derived_Type_Ptr is
      access all Derived_Type_Node;
   for Derived_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Derived_Type_Node
     (The_Context : ASIS.Context)
      return Derived_Type_Ptr;

   function Corresponding_Parent_Subtype
     (Element : Derived_Type_Node) return Asis.Declaration;

   procedure Set_Corresponding_Parent_Subtype
     (Element : in out Derived_Type_Node;
      Value   : in     Asis.Declaration);

   function Corresponding_Root_Type
     (Element : Derived_Type_Node) return Asis.Declaration;

   procedure Set_Corresponding_Root_Type
     (Element : in out Derived_Type_Node;
      Value   : in     Asis.Declaration);

   function Implicit_Inherited_Declarations
     (Element : Derived_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Implicit_Inherited_Declarations
     (Element : in out Derived_Type_Node;
      Item    : in     Asis.Element);

   function Implicit_Inherited_Subprograms
     (Element : Derived_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Implicit_Inherited_Subprograms
     (Element : in out Derived_Type_Node;
      Item    : in     Asis.Element);

   function Corresponding_Type_Structure
     (Element : Derived_Type_Node) return Asis.Declaration;

   procedure Set_Corresponding_Type_Structure
     (Element : in out Derived_Type_Node;
      Value   : in     Asis.Declaration);

   function Trait_Kind
     (Element : Derived_Type_Node) return Asis.Trait_Kinds;

   procedure Set_Trait_Kind
     (Element : in out Derived_Type_Node;
      Value   : in     Asis.Trait_Kinds);

   function Parent_Subtype_Indication
     (Element : Derived_Type_Node) return Asis.Subtype_Indication;

   procedure Set_Parent_Subtype_Indication
     (Element : in out Derived_Type_Node;
      Value   : in     Asis.Subtype_Indication);

   function Has_Limited
     (Element : Derived_Type_Node) return Boolean;

   procedure Set_Has_Limited
     (Element : in out Derived_Type_Node;
      Value   : in     Boolean);

   function Has_Abstract
     (Element : Derived_Type_Node) return Boolean;

   procedure Set_Has_Abstract
     (Element : in out Derived_Type_Node;
      Value   : in     Boolean);

   function Type_Definition_Kind (Element : Derived_Type_Node)
      return Asis.Type_Kinds;

   function Children (Element : access Derived_Type_Node)
     return Traverse_List;

   function Clone
     (Element : Derived_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Derived_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------------
   -- Derived_Record_Extension_Node --
   -----------------------------------

   type Derived_Record_Extension_Node is 
      new Derived_Type_Node with private;

   type Derived_Record_Extension_Ptr is
      access all Derived_Record_Extension_Node;
   for Derived_Record_Extension_Ptr'Storage_Pool use Lists.Pool;

   function New_Derived_Record_Extension_Node
     (The_Context : ASIS.Context)
      return Derived_Record_Extension_Ptr;

   function Progenitor_List
     (Element : Derived_Record_Extension_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Progenitor_List
     (Element : in out Derived_Record_Extension_Node;
      Value   : in     Asis.Element);

   function Progenitor_List_List
     (Element : Derived_Record_Extension_Node) return Asis.Element;

   function Get_Record_Definition
     (Element : Derived_Record_Extension_Node) return Asis.Definition;

   procedure Set_Record_Definition
     (Element : in out Derived_Record_Extension_Node;
      Value   : in     Asis.Definition);

   function Type_Definition_Kind (Element : Derived_Record_Extension_Node)
      return Asis.Type_Kinds;

   function Children (Element : access Derived_Record_Extension_Node)
     return Traverse_List;

   function Clone
     (Element : Derived_Record_Extension_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Derived_Record_Extension_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------
   -- Enumeration_Type_Node --
   ---------------------------

   type Enumeration_Type_Node is 
      new Type_Definition_Node with private;

   type Enumeration_Type_Ptr is
      access all Enumeration_Type_Node;
   for Enumeration_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Enumeration_Type_Node
     (The_Context : ASIS.Context)
      return Enumeration_Type_Ptr;

   function Enumeration_Literal_Declarations
     (Element : Enumeration_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Enumeration_Literal_Declarations
     (Element : in out Enumeration_Type_Node;
      Value   : in     Asis.Element);

   function Enumeration_Literal_Declarations_List
     (Element : Enumeration_Type_Node) return Asis.Element;

   function Type_Definition_Kind (Element : Enumeration_Type_Node)
      return Asis.Type_Kinds;

   function Children (Element : access Enumeration_Type_Node)
     return Traverse_List;

   function Clone
     (Element : Enumeration_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Enumeration_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------
   -- Signed_Integer_Type_Node --
   ------------------------------

   type Signed_Integer_Type_Node is 
      new Type_Definition_Node with private;

   type Signed_Integer_Type_Ptr is
      access all Signed_Integer_Type_Node;
   for Signed_Integer_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Signed_Integer_Type_Node
     (The_Context : ASIS.Context)
      return Signed_Integer_Type_Ptr;

   function Integer_Constraint
     (Element : Signed_Integer_Type_Node) return Asis.Range_Constraint;

   procedure Set_Integer_Constraint
     (Element : in out Signed_Integer_Type_Node;
      Value   : in     Asis.Range_Constraint);

   function Type_Definition_Kind (Element : Signed_Integer_Type_Node)
      return Asis.Type_Kinds;

   function Children (Element : access Signed_Integer_Type_Node)
     return Traverse_List;

   function Clone
     (Element : Signed_Integer_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Signed_Integer_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------
   -- Modular_Type_Node --
   -----------------------

   type Modular_Type_Node is 
      new Type_Definition_Node with private;

   type Modular_Type_Ptr is
      access all Modular_Type_Node;
   for Modular_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Modular_Type_Node
     (The_Context : ASIS.Context)
      return Modular_Type_Ptr;

   function Mod_Static_Expression
     (Element : Modular_Type_Node) return Asis.Expression;

   procedure Set_Mod_Static_Expression
     (Element : in out Modular_Type_Node;
      Value   : in     Asis.Expression);

   function Type_Definition_Kind (Element : Modular_Type_Node)
      return Asis.Type_Kinds;

   function Children (Element : access Modular_Type_Node)
     return Traverse_List;

   function Clone
     (Element : Modular_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Modular_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------
   -- Root_Type_Node --
   --------------------

   type Root_Type_Node is 
      new Type_Definition_Node with private;

   type Root_Type_Ptr is
      access all Root_Type_Node;
   for Root_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Root_Type_Node
     (The_Context : ASIS.Context)
      return Root_Type_Ptr;

   function Root_Type_Kind
     (Element : Root_Type_Node) return Asis.Root_Type_Kinds;

   procedure Set_Root_Type_Kind
     (Element : in out Root_Type_Node;
      Value   : in     Asis.Root_Type_Kinds);

   function Type_Definition_Kind (Element : Root_Type_Node)
      return Asis.Type_Kinds;

   function Clone
     (Element : Root_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -------------------------
   -- Floating_Point_Node --
   -------------------------

   type Floating_Point_Node is 
      new Type_Definition_Node with private;

   type Floating_Point_Ptr is
      access all Floating_Point_Node;
   for Floating_Point_Ptr'Storage_Pool use Lists.Pool;

   function New_Floating_Point_Node
     (The_Context : ASIS.Context)
      return Floating_Point_Ptr;

   function Digits_Expression
     (Element : Floating_Point_Node) return Asis.Expression;

   procedure Set_Digits_Expression
     (Element : in out Floating_Point_Node;
      Value   : in     Asis.Expression);

   function Real_Range_Constraint
     (Element : Floating_Point_Node) return Asis.Range_Constraint;

   procedure Set_Real_Range_Constraint
     (Element : in out Floating_Point_Node;
      Value   : in     Asis.Range_Constraint);

   function Type_Definition_Kind (Element : Floating_Point_Node)
      return Asis.Type_Kinds;

   function Children (Element : access Floating_Point_Node)
     return Traverse_List;

   function Clone
     (Element : Floating_Point_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Floating_Point_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------
   -- Ordinary_Fixed_Point_Node --
   -------------------------------

   type Ordinary_Fixed_Point_Node is 
      new Type_Definition_Node with private;

   type Ordinary_Fixed_Point_Ptr is
      access all Ordinary_Fixed_Point_Node;
   for Ordinary_Fixed_Point_Ptr'Storage_Pool use Lists.Pool;

   function New_Ordinary_Fixed_Point_Node
     (The_Context : ASIS.Context)
      return Ordinary_Fixed_Point_Ptr;

   function Delta_Expression
     (Element : Ordinary_Fixed_Point_Node) return Asis.Expression;

   procedure Set_Delta_Expression
     (Element : in out Ordinary_Fixed_Point_Node;
      Value   : in     Asis.Expression);

   function Real_Range_Constraint
     (Element : Ordinary_Fixed_Point_Node) return Asis.Range_Constraint;

   procedure Set_Real_Range_Constraint
     (Element : in out Ordinary_Fixed_Point_Node;
      Value   : in     Asis.Range_Constraint);

   function Type_Definition_Kind (Element : Ordinary_Fixed_Point_Node)
      return Asis.Type_Kinds;

   function Children (Element : access Ordinary_Fixed_Point_Node)
     return Traverse_List;

   function Clone
     (Element : Ordinary_Fixed_Point_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Ordinary_Fixed_Point_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------
   -- Decimal_Fixed_Point_Node --
   ------------------------------

   type Decimal_Fixed_Point_Node is 
      new Ordinary_Fixed_Point_Node with private;

   type Decimal_Fixed_Point_Ptr is
      access all Decimal_Fixed_Point_Node;
   for Decimal_Fixed_Point_Ptr'Storage_Pool use Lists.Pool;

   function New_Decimal_Fixed_Point_Node
     (The_Context : ASIS.Context)
      return Decimal_Fixed_Point_Ptr;

   function Digits_Expression
     (Element : Decimal_Fixed_Point_Node) return Asis.Expression;

   procedure Set_Digits_Expression
     (Element : in out Decimal_Fixed_Point_Node;
      Value   : in     Asis.Expression);

   function Type_Definition_Kind (Element : Decimal_Fixed_Point_Node)
      return Asis.Type_Kinds;

   function Children (Element : access Decimal_Fixed_Point_Node)
     return Traverse_List;

   function Clone
     (Element : Decimal_Fixed_Point_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Decimal_Fixed_Point_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------
   -- Unconstrained_Array_Node --
   ------------------------------

   type Unconstrained_Array_Node is 
      new Type_Definition_Node with private;

   type Unconstrained_Array_Ptr is
      access all Unconstrained_Array_Node;
   for Unconstrained_Array_Ptr'Storage_Pool use Lists.Pool;

   function New_Unconstrained_Array_Node
     (The_Context : ASIS.Context)
      return Unconstrained_Array_Ptr;

   function Index_Subtype_Definitions
     (Element : Unconstrained_Array_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Index_Subtype_Definitions
     (Element : in out Unconstrained_Array_Node;
      Value   : in     Asis.Element);

   function Index_Subtype_Definitions_List
     (Element : Unconstrained_Array_Node) return Asis.Element;

   function Array_Component_Definition
     (Element : Unconstrained_Array_Node) return Asis.Component_Definition;

   procedure Set_Array_Component_Definition
     (Element : in out Unconstrained_Array_Node;
      Value   : in     Asis.Component_Definition);

   function Type_Definition_Kind (Element : Unconstrained_Array_Node)
      return Asis.Type_Kinds;

   function Children (Element : access Unconstrained_Array_Node)
     return Traverse_List;

   function Clone
     (Element : Unconstrained_Array_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Unconstrained_Array_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------
   -- Constrained_Array_Node --
   ----------------------------

   type Constrained_Array_Node is 
      new Type_Definition_Node with private;

   type Constrained_Array_Ptr is
      access all Constrained_Array_Node;
   for Constrained_Array_Ptr'Storage_Pool use Lists.Pool;

   function New_Constrained_Array_Node
     (The_Context : ASIS.Context)
      return Constrained_Array_Ptr;

   function Discrete_Subtype_Definitions
     (Element : Constrained_Array_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Discrete_Subtype_Definitions
     (Element : in out Constrained_Array_Node;
      Value   : in     Asis.Element);

   function Discrete_Subtype_Definitions_List
     (Element : Constrained_Array_Node) return Asis.Element;

   function Array_Component_Definition
     (Element : Constrained_Array_Node) return Asis.Component_Definition;

   procedure Set_Array_Component_Definition
     (Element : in out Constrained_Array_Node;
      Value   : in     Asis.Component_Definition);

   function Type_Definition_Kind (Element : Constrained_Array_Node)
      return Asis.Type_Kinds;

   function Children (Element : access Constrained_Array_Node)
     return Traverse_List;

   function Clone
     (Element : Constrained_Array_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Constrained_Array_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------
   -- Record_Type_Node --
   ----------------------

   type Record_Type_Node is 
      new Type_Definition_Node with private;

   type Record_Type_Ptr is
      access all Record_Type_Node;
   for Record_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Record_Type_Node
     (The_Context : ASIS.Context)
      return Record_Type_Ptr;

   function Trait_Kind
     (Element : Record_Type_Node) return Asis.Trait_Kinds;

   procedure Set_Trait_Kind
     (Element : in out Record_Type_Node;
      Value   : in     Asis.Trait_Kinds);

   function Get_Record_Definition
     (Element : Record_Type_Node) return Asis.Definition;

   procedure Set_Record_Definition
     (Element : in out Record_Type_Node;
      Value   : in     Asis.Definition);

   function Has_Limited
     (Element : Record_Type_Node) return Boolean;

   procedure Set_Has_Limited
     (Element : in out Record_Type_Node;
      Value   : in     Boolean);

   function Type_Definition_Kind (Element : Record_Type_Node)
      return Asis.Type_Kinds;

   function Children (Element : access Record_Type_Node)
     return Traverse_List;

   function Clone
     (Element : Record_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Record_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------
   -- Tagged_Record_Type_Node --
   -----------------------------

   type Tagged_Record_Type_Node is 
      new Record_Type_Node with private;

   type Tagged_Record_Type_Ptr is
      access all Tagged_Record_Type_Node;
   for Tagged_Record_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Tagged_Record_Type_Node
     (The_Context : ASIS.Context)
      return Tagged_Record_Type_Ptr;

   function Has_Abstract
     (Element : Tagged_Record_Type_Node) return Boolean;

   procedure Set_Has_Abstract
     (Element : in out Tagged_Record_Type_Node;
      Value   : in     Boolean);

   function Has_Tagged
     (Element : Tagged_Record_Type_Node) return Boolean;

   procedure Set_Has_Tagged
     (Element : in out Tagged_Record_Type_Node;
      Value   : in     Boolean);

   function Type_Definition_Kind (Element : Tagged_Record_Type_Node)
      return Asis.Type_Kinds;

   function Clone
     (Element : Tagged_Record_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Tagged_Record_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------
   -- Interface_Type_Node --
   -------------------------

   type Interface_Type_Node is 
      new Type_Definition_Node with private;

   type Interface_Type_Ptr is
      access all Interface_Type_Node;
   for Interface_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Interface_Type_Node
     (The_Context : ASIS.Context)
      return Interface_Type_Ptr;

   function Interface_Kind
     (Element : Interface_Type_Node) return Asis.Interface_Kinds;

   procedure Set_Interface_Kind
     (Element : in out Interface_Type_Node;
      Value   : in     Asis.Interface_Kinds);

   function Has_Limited
     (Element : Interface_Type_Node) return Boolean;

   procedure Set_Has_Limited
     (Element : in out Interface_Type_Node;
      Value   : in     Boolean);

   function Has_Synchronized
     (Element : Interface_Type_Node) return Boolean;

   procedure Set_Has_Synchronized
     (Element : in out Interface_Type_Node;
      Value   : in     Boolean);

   function Has_Protected
     (Element : Interface_Type_Node) return Boolean;

   procedure Set_Has_Protected
     (Element : in out Interface_Type_Node;
      Value   : in     Boolean);

   function Has_Task
     (Element : Interface_Type_Node) return Boolean;

   procedure Set_Has_Task
     (Element : in out Interface_Type_Node;
      Value   : in     Boolean);

   function Progenitor_List
     (Element : Interface_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Progenitor_List
     (Element : in out Interface_Type_Node;
      Value   : in     Asis.Element);

   function Progenitor_List_List
     (Element : Interface_Type_Node) return Asis.Element;

   function Implicit_Inherited_Subprograms
     (Element : Interface_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Implicit_Inherited_Subprograms
     (Element : in out Interface_Type_Node;
      Item    : in     Asis.Element);

   function Type_Definition_Kind (Element : Interface_Type_Node)
      return Asis.Type_Kinds;

   function Children (Element : access Interface_Type_Node)
     return Traverse_List;

   function Clone
     (Element : Interface_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Interface_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------
   -- Access_Type_Node --
   ----------------------

   type Access_Type_Node is 
      new Type_Definition_Node with private;

   type Access_Type_Ptr is
      access all Access_Type_Node;
   for Access_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Access_Type_Node
     (The_Context : ASIS.Context)
      return Access_Type_Ptr;

   function Access_Type_Kind
     (Element : Access_Type_Node) return Asis.Access_Type_Kinds;

   procedure Set_Access_Type_Kind
     (Element : in out Access_Type_Node;
      Value   : in     Asis.Access_Type_Kinds);

   function Get_Access_To_Object_Definition
     (Element : Access_Type_Node) return Asis.Subtype_Indication;

   procedure Set_Access_To_Object_Definition
     (Element : in out Access_Type_Node;
      Value   : in     Asis.Subtype_Indication);

   function Access_To_Subprogram_Parameter_Profile
     (Element : Access_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Access_To_Subprogram_Parameter_Profile
     (Element : in out Access_Type_Node;
      Value   : in     Asis.Element);

   function Access_To_Subprogram_Parameter_Profile_List
     (Element : Access_Type_Node) return Asis.Element;

   function Access_To_Function_Result_Subtype
     (Element : Access_Type_Node) return Asis.Definition;

   procedure Set_Access_To_Function_Result_Subtype
     (Element : in out Access_Type_Node;
      Value   : in     Asis.Definition);

   function Has_Null_Exclusion
     (Element : Access_Type_Node) return Boolean;

   procedure Set_Has_Null_Exclusion
     (Element : in out Access_Type_Node;
      Value   : in     Boolean);

   function Type_Definition_Kind (Element : Access_Type_Node)
      return Asis.Type_Kinds;

   function Children (Element : access Access_Type_Node)
     return Traverse_List;

   function Clone
     (Element : Access_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Access_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

private


   type Derived_Type_Node is 
      new Type_Definition_Node with
      record
         Corresponding_Parent_Subtype   : aliased Asis.Declaration;
         Corresponding_Root_Type        : aliased Asis.Declaration;
         Implicit_Inherited_Declarations : aliased Secondary_Declaration_Lists.List_Node;
         Implicit_Inherited_Subprograms : aliased Secondary_Declaration_Lists.List_Node;
         Corresponding_Type_Structure   : aliased Asis.Declaration;
         Trait_Kind                     : aliased Asis.Trait_Kinds := An_Ordinary_Trait;
         Parent_Subtype_Indication      : aliased Asis.Subtype_Indication;
         Has_Limited                    : aliased Boolean := False;
         Has_Abstract                   : aliased Boolean := False;
      end record;


   type Derived_Record_Extension_Node is 
      new Derived_Type_Node with
      record
         Progenitor_List                : aliased Primary_Expression_Lists.List;
         Record_Definition              : aliased Asis.Definition;
      end record;


   type Enumeration_Type_Node is 
      new Type_Definition_Node with
      record
         Enumeration_Literal_Declarations : aliased Primary_Declaration_Lists.List;
      end record;


   type Signed_Integer_Type_Node is 
      new Type_Definition_Node with
      record
         Integer_Constraint             : aliased Asis.Range_Constraint;
      end record;


   type Modular_Type_Node is 
      new Type_Definition_Node with
      record
         Mod_Static_Expression          : aliased Asis.Expression;
      end record;


   type Root_Type_Node is 
      new Type_Definition_Node with
      record
         Root_Type_Kind                 : aliased Asis.Root_Type_Kinds := Not_A_Root_Type_Definition;
      end record;


   type Floating_Point_Node is 
      new Type_Definition_Node with
      record
         Digits_Expression              : aliased Asis.Expression;
         Real_Range_Constraint          : aliased Asis.Range_Constraint;
      end record;


   type Ordinary_Fixed_Point_Node is 
      new Type_Definition_Node with
      record
         Delta_Expression               : aliased Asis.Expression;
         Real_Range_Constraint          : aliased Asis.Range_Constraint;
      end record;


   type Decimal_Fixed_Point_Node is 
      new Ordinary_Fixed_Point_Node with
      record
         Digits_Expression              : aliased Asis.Expression;
      end record;


   type Unconstrained_Array_Node is 
      new Type_Definition_Node with
      record
         Index_Subtype_Definitions      : aliased Primary_Identifier_Lists.List;
         Array_Component_Definition     : aliased Asis.Component_Definition;
      end record;


   type Constrained_Array_Node is 
      new Type_Definition_Node with
      record
         Discrete_Subtype_Definitions   : aliased Primary_Definition_Lists.List;
         Array_Component_Definition     : aliased Asis.Component_Definition;
      end record;


   type Record_Type_Node is 
      new Type_Definition_Node with
      record
         Trait_Kind                     : aliased Asis.Trait_Kinds := An_Ordinary_Trait;
         Record_Definition              : aliased Asis.Definition;
         Has_Limited                    : aliased Boolean := False;
      end record;


   type Tagged_Record_Type_Node is 
      new Record_Type_Node with
      record
         Has_Abstract                   : aliased Boolean := False;
         Has_Tagged                     : aliased Boolean := False;
      end record;


   type Interface_Type_Node is 
      new Type_Definition_Node with
      record
         Interface_Kind                 : aliased Asis.Interface_Kinds := Not_An_Interface;
         Has_Limited                    : aliased Boolean := False;
         Has_Synchronized               : aliased Boolean := False;
         Has_Protected                  : aliased Boolean := False;
         Has_Task                       : aliased Boolean := False;
         Progenitor_List                : aliased Primary_Expression_Lists.List;
         Implicit_Inherited_Subprograms : aliased Secondary_Declaration_Lists.List_Node;
      end record;


   type Access_Type_Node is 
      new Type_Definition_Node with
      record
         Access_Type_Kind               : aliased Asis.Access_Type_Kinds := A_Pool_Specific_Access_To_Variable;
         Access_To_Object_Definition    : aliased Asis.Subtype_Indication;
         Access_To_Subprogram_Parameter_Profile : aliased Primary_Parameter_Lists.List;
         Access_To_Function_Result_Subtype : aliased Asis.Definition;
         Has_Null_Exclusion             : aliased Boolean := False;
      end record;

end Asis.Gela.Elements.Defs.Types;
