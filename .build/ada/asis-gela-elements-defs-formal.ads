
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
  
package Asis.Gela.Elements.Defs.Formal is

   ------------------------------
   -- Formal_Private_Type_Node --
   ------------------------------

   type Formal_Private_Type_Node is 
      new Formal_Type_Definition_Node with private;

   type Formal_Private_Type_Ptr is
      access all Formal_Private_Type_Node;
   for Formal_Private_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Private_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Private_Type_Ptr;

   function Trait_Kind
     (Element : Formal_Private_Type_Node) return Asis.Trait_Kinds;

   procedure Set_Trait_Kind
     (Element : in out Formal_Private_Type_Node;
      Value   : in     Asis.Trait_Kinds);

   function Has_Limited
     (Element : Formal_Private_Type_Node) return Boolean;

   procedure Set_Has_Limited
     (Element : in out Formal_Private_Type_Node;
      Value   : in     Boolean);

   function Has_Private
     (Element : Formal_Private_Type_Node) return Boolean;

   procedure Set_Has_Private
     (Element : in out Formal_Private_Type_Node;
      Value   : in     Boolean);

   function Formal_Type_Definition_Kind (Element : Formal_Private_Type_Node)
      return Asis.Formal_Type_Kinds;

   function Clone
     (Element : Formal_Private_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -------------------------------------
   -- Formal_Tagged_Private_Type_Node --
   -------------------------------------

   type Formal_Tagged_Private_Type_Node is 
      new Formal_Private_Type_Node with private;

   type Formal_Tagged_Private_Type_Ptr is
      access all Formal_Tagged_Private_Type_Node;
   for Formal_Tagged_Private_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Tagged_Private_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Tagged_Private_Type_Ptr;

   function Has_Abstract
     (Element : Formal_Tagged_Private_Type_Node) return Boolean;

   procedure Set_Has_Abstract
     (Element : in out Formal_Tagged_Private_Type_Node;
      Value   : in     Boolean);

   function Has_Tagged
     (Element : Formal_Tagged_Private_Type_Node) return Boolean;

   procedure Set_Has_Tagged
     (Element : in out Formal_Tagged_Private_Type_Node;
      Value   : in     Boolean);

   function Formal_Type_Definition_Kind (Element : Formal_Tagged_Private_Type_Node)
      return Asis.Formal_Type_Kinds;

   function Clone
     (Element : Formal_Tagged_Private_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   ------------------------------
   -- Formal_Derived_Type_Node --
   ------------------------------

   type Formal_Derived_Type_Node is 
      new Formal_Type_Definition_Node with private;

   type Formal_Derived_Type_Ptr is
      access all Formal_Derived_Type_Node;
   for Formal_Derived_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Derived_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Derived_Type_Ptr;

   function Implicit_Inherited_Declarations
     (Element : Formal_Derived_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Implicit_Inherited_Declarations
     (Element : in out Formal_Derived_Type_Node;
      Item    : in     Asis.Element);

   function Implicit_Inherited_Subprograms
     (Element : Formal_Derived_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Implicit_Inherited_Subprograms
     (Element : in out Formal_Derived_Type_Node;
      Item    : in     Asis.Element);

   function Get_Subtype_Mark
     (Element : Formal_Derived_Type_Node) return Asis.Expression;

   procedure Set_Subtype_Mark
     (Element : in out Formal_Derived_Type_Node;
      Value   : in     Asis.Expression);

   function Trait_Kind
     (Element : Formal_Derived_Type_Node) return Asis.Trait_Kinds;

   procedure Set_Trait_Kind
     (Element : in out Formal_Derived_Type_Node;
      Value   : in     Asis.Trait_Kinds);

   function Progenitor_List
     (Element : Formal_Derived_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Progenitor_List
     (Element : in out Formal_Derived_Type_Node;
      Value   : in     Asis.Element);

   function Progenitor_List_List
     (Element : Formal_Derived_Type_Node) return Asis.Element;

   function Has_Abstract
     (Element : Formal_Derived_Type_Node) return Boolean;

   procedure Set_Has_Abstract
     (Element : in out Formal_Derived_Type_Node;
      Value   : in     Boolean);

   function Has_Private
     (Element : Formal_Derived_Type_Node) return Boolean;

   procedure Set_Has_Private
     (Element : in out Formal_Derived_Type_Node;
      Value   : in     Boolean);

   function Has_Limited
     (Element : Formal_Derived_Type_Node) return Boolean;

   procedure Set_Has_Limited
     (Element : in out Formal_Derived_Type_Node;
      Value   : in     Boolean);

   function Has_Synchronized
     (Element : Formal_Derived_Type_Node) return Boolean;

   procedure Set_Has_Synchronized
     (Element : in out Formal_Derived_Type_Node;
      Value   : in     Boolean);

   function Formal_Type_Definition_Kind (Element : Formal_Derived_Type_Node)
      return Asis.Formal_Type_Kinds;

   function Children (Element : access Formal_Derived_Type_Node)
     return Traverse_List;

   function Clone
     (Element : Formal_Derived_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Derived_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------
   -- Formal_Discrete_Type_Node --
   -------------------------------

   type Formal_Discrete_Type_Node is 
      new Formal_Type_Definition_Node with private;

   type Formal_Discrete_Type_Ptr is
      access all Formal_Discrete_Type_Node;
   for Formal_Discrete_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Discrete_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Discrete_Type_Ptr;

   function Formal_Type_Definition_Kind (Element : Formal_Discrete_Type_Node)
      return Asis.Formal_Type_Kinds;

   function Clone
     (Element : Formal_Discrete_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -------------------------------------
   -- Formal_Signed_Integer_Type_Node --
   -------------------------------------

   type Formal_Signed_Integer_Type_Node is 
      new Formal_Type_Definition_Node with private;

   type Formal_Signed_Integer_Type_Ptr is
      access all Formal_Signed_Integer_Type_Node;
   for Formal_Signed_Integer_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Signed_Integer_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Signed_Integer_Type_Ptr;

   function Formal_Type_Definition_Kind (Element : Formal_Signed_Integer_Type_Node)
      return Asis.Formal_Type_Kinds;

   function Clone
     (Element : Formal_Signed_Integer_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   ------------------------------
   -- Formal_Modular_Type_Node --
   ------------------------------

   type Formal_Modular_Type_Node is 
      new Formal_Type_Definition_Node with private;

   type Formal_Modular_Type_Ptr is
      access all Formal_Modular_Type_Node;
   for Formal_Modular_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Modular_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Modular_Type_Ptr;

   function Formal_Type_Definition_Kind (Element : Formal_Modular_Type_Node)
      return Asis.Formal_Type_Kinds;

   function Clone
     (Element : Formal_Modular_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   --------------------------------
   -- Formal_Floating_Point_Node --
   --------------------------------

   type Formal_Floating_Point_Node is 
      new Formal_Type_Definition_Node with private;

   type Formal_Floating_Point_Ptr is
      access all Formal_Floating_Point_Node;
   for Formal_Floating_Point_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Floating_Point_Node
     (The_Context : ASIS.Context)
      return Formal_Floating_Point_Ptr;

   function Formal_Type_Definition_Kind (Element : Formal_Floating_Point_Node)
      return Asis.Formal_Type_Kinds;

   function Clone
     (Element : Formal_Floating_Point_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   --------------------------------------
   -- Formal_Ordinary_Fixed_Point_Node --
   --------------------------------------

   type Formal_Ordinary_Fixed_Point_Node is 
      new Formal_Type_Definition_Node with private;

   type Formal_Ordinary_Fixed_Point_Ptr is
      access all Formal_Ordinary_Fixed_Point_Node;
   for Formal_Ordinary_Fixed_Point_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Ordinary_Fixed_Point_Node
     (The_Context : ASIS.Context)
      return Formal_Ordinary_Fixed_Point_Ptr;

   function Formal_Type_Definition_Kind (Element : Formal_Ordinary_Fixed_Point_Node)
      return Asis.Formal_Type_Kinds;

   function Clone
     (Element : Formal_Ordinary_Fixed_Point_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -------------------------------------
   -- Formal_Decimal_Fixed_Point_Node --
   -------------------------------------

   type Formal_Decimal_Fixed_Point_Node is 
      new Formal_Type_Definition_Node with private;

   type Formal_Decimal_Fixed_Point_Ptr is
      access all Formal_Decimal_Fixed_Point_Node;
   for Formal_Decimal_Fixed_Point_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Decimal_Fixed_Point_Node
     (The_Context : ASIS.Context)
      return Formal_Decimal_Fixed_Point_Ptr;

   function Formal_Type_Definition_Kind (Element : Formal_Decimal_Fixed_Point_Node)
      return Asis.Formal_Type_Kinds;

   function Clone
     (Element : Formal_Decimal_Fixed_Point_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -------------------------------------
   -- Formal_Unconstrained_Array_Node --
   -------------------------------------

   type Formal_Unconstrained_Array_Node is 
      new Formal_Type_Definition_Node with private;

   type Formal_Unconstrained_Array_Ptr is
      access all Formal_Unconstrained_Array_Node;
   for Formal_Unconstrained_Array_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Unconstrained_Array_Node
     (The_Context : ASIS.Context)
      return Formal_Unconstrained_Array_Ptr;

   function Index_Subtype_Definitions
     (Element : Formal_Unconstrained_Array_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Index_Subtype_Definitions
     (Element : in out Formal_Unconstrained_Array_Node;
      Value   : in     Asis.Element);

   function Index_Subtype_Definitions_List
     (Element : Formal_Unconstrained_Array_Node) return Asis.Element;

   function Array_Component_Definition
     (Element : Formal_Unconstrained_Array_Node) return Asis.Component_Definition;

   procedure Set_Array_Component_Definition
     (Element : in out Formal_Unconstrained_Array_Node;
      Value   : in     Asis.Component_Definition);

   function Array_Definition
     (Element : Formal_Unconstrained_Array_Node) return Asis.Element;

   procedure Set_Array_Definition
     (Element : in out Formal_Unconstrained_Array_Node;
      Value   : in     Asis.Element);

   function Formal_Type_Definition_Kind (Element : Formal_Unconstrained_Array_Node)
      return Asis.Formal_Type_Kinds;

   function Children (Element : access Formal_Unconstrained_Array_Node)
     return Traverse_List;

   function Clone
     (Element : Formal_Unconstrained_Array_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Unconstrained_Array_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------------
   -- Formal_Constrained_Array_Node --
   -----------------------------------

   type Formal_Constrained_Array_Node is 
      new Formal_Type_Definition_Node with private;

   type Formal_Constrained_Array_Ptr is
      access all Formal_Constrained_Array_Node;
   for Formal_Constrained_Array_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Constrained_Array_Node
     (The_Context : ASIS.Context)
      return Formal_Constrained_Array_Ptr;

   function Discrete_Subtype_Definitions
     (Element : Formal_Constrained_Array_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Discrete_Subtype_Definitions
     (Element : in out Formal_Constrained_Array_Node;
      Value   : in     Asis.Element);

   function Discrete_Subtype_Definitions_List
     (Element : Formal_Constrained_Array_Node) return Asis.Element;

   function Array_Component_Definition
     (Element : Formal_Constrained_Array_Node) return Asis.Component_Definition;

   procedure Set_Array_Component_Definition
     (Element : in out Formal_Constrained_Array_Node;
      Value   : in     Asis.Component_Definition);

   function Array_Definition
     (Element : Formal_Constrained_Array_Node) return Asis.Element;

   procedure Set_Array_Definition
     (Element : in out Formal_Constrained_Array_Node;
      Value   : in     Asis.Element);

   function Formal_Type_Definition_Kind (Element : Formal_Constrained_Array_Node)
      return Asis.Formal_Type_Kinds;

   function Children (Element : access Formal_Constrained_Array_Node)
     return Traverse_List;

   function Clone
     (Element : Formal_Constrained_Array_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Constrained_Array_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------
   -- Formal_Access_Type_Node --
   -----------------------------

   type Formal_Access_Type_Node is 
      new Formal_Type_Definition_Node with private;

   type Formal_Access_Type_Ptr is
      access all Formal_Access_Type_Node;
   for Formal_Access_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Access_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Access_Type_Ptr;

   function Access_Type_Kind
     (Element : Formal_Access_Type_Node) return Asis.Access_Type_Kinds;

   procedure Set_Access_Type_Kind
     (Element : in out Formal_Access_Type_Node;
      Value   : in     Asis.Access_Type_Kinds);

   function Get_Access_To_Object_Definition
     (Element : Formal_Access_Type_Node) return Asis.Subtype_Indication;

   procedure Set_Access_To_Object_Definition
     (Element : in out Formal_Access_Type_Node;
      Value   : in     Asis.Subtype_Indication);

   function Access_To_Subprogram_Parameter_Profile
     (Element : Formal_Access_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Access_To_Subprogram_Parameter_Profile
     (Element : in out Formal_Access_Type_Node;
      Value   : in     Asis.Element);

   function Access_To_Subprogram_Parameter_Profile_List
     (Element : Formal_Access_Type_Node) return Asis.Element;

   function Access_To_Function_Result_Subtype
     (Element : Formal_Access_Type_Node) return Asis.Definition;

   procedure Set_Access_To_Function_Result_Subtype
     (Element : in out Formal_Access_Type_Node;
      Value   : in     Asis.Definition);

   function Access_Definition
     (Element : Formal_Access_Type_Node) return Asis.Element;

   procedure Set_Access_Definition
     (Element : in out Formal_Access_Type_Node;
      Value   : in     Asis.Element);

   function Formal_Type_Definition_Kind (Element : Formal_Access_Type_Node)
      return Asis.Formal_Type_Kinds;

   function Children (Element : access Formal_Access_Type_Node)
     return Traverse_List;

   function Clone
     (Element : Formal_Access_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Access_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------
   -- Formal_Interface_Type_Node --
   --------------------------------

   type Formal_Interface_Type_Node is 
      new Formal_Type_Definition_Node with private;

   type Formal_Interface_Type_Ptr is
      access all Formal_Interface_Type_Node;
   for Formal_Interface_Type_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Interface_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Interface_Type_Ptr;

   function Interface_Kind
     (Element : Formal_Interface_Type_Node) return Asis.Interface_Kinds;

   procedure Set_Interface_Kind
     (Element : in out Formal_Interface_Type_Node;
      Value   : in     Asis.Interface_Kinds);

   function Progenitor_List
     (Element : Formal_Interface_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Progenitor_List
     (Element : in out Formal_Interface_Type_Node;
      Value   : in     Asis.Element);

   function Progenitor_List_List
     (Element : Formal_Interface_Type_Node) return Asis.Element;

   function Implicit_Inherited_Subprograms
     (Element : Formal_Interface_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Implicit_Inherited_Subprograms
     (Element : in out Formal_Interface_Type_Node;
      Item    : in     Asis.Element);

   function Formal_Type_Definition_Kind (Element : Formal_Interface_Type_Node)
      return Asis.Formal_Type_Kinds;

   function Children (Element : access Formal_Interface_Type_Node)
     return Traverse_List;

   function Clone
     (Element : Formal_Interface_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Interface_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

private


   type Formal_Private_Type_Node is 
      new Formal_Type_Definition_Node with
      record
         Trait_Kind                     : aliased Asis.Trait_Kinds := An_Ordinary_Trait;
         Has_Limited                    : aliased Boolean := False;
         Has_Private                    : aliased Boolean := False;
      end record;


   type Formal_Tagged_Private_Type_Node is 
      new Formal_Private_Type_Node with
      record
         Has_Abstract                   : aliased Boolean := False;
         Has_Tagged                     : aliased Boolean := False;
      end record;


   type Formal_Derived_Type_Node is 
      new Formal_Type_Definition_Node with
      record
         Implicit_Inherited_Declarations : aliased Secondary_Declaration_Lists.List_Node;
         Implicit_Inherited_Subprograms : aliased Secondary_Declaration_Lists.List_Node;
         Subtype_Mark                   : aliased Asis.Expression;
         Trait_Kind                     : aliased Asis.Trait_Kinds := An_Ordinary_Trait;
         Progenitor_List                : aliased Primary_Expression_Lists.List;
         Has_Abstract                   : aliased Boolean := False;
         Has_Private                    : aliased Boolean := False;
         Has_Limited                    : aliased Boolean := False;
         Has_Synchronized               : aliased Boolean := False;
      end record;


   type Formal_Discrete_Type_Node is 
      new Formal_Type_Definition_Node with
      record
         null;
      end record;


   type Formal_Signed_Integer_Type_Node is 
      new Formal_Type_Definition_Node with
      record
         null;
      end record;


   type Formal_Modular_Type_Node is 
      new Formal_Type_Definition_Node with
      record
         null;
      end record;


   type Formal_Floating_Point_Node is 
      new Formal_Type_Definition_Node with
      record
         null;
      end record;


   type Formal_Ordinary_Fixed_Point_Node is 
      new Formal_Type_Definition_Node with
      record
         null;
      end record;


   type Formal_Decimal_Fixed_Point_Node is 
      new Formal_Type_Definition_Node with
      record
         null;
      end record;


   type Formal_Unconstrained_Array_Node is 
      new Formal_Type_Definition_Node with
      record
         Index_Subtype_Definitions      : aliased Primary_Identifier_Lists.List;
         Array_Component_Definition     : aliased Asis.Component_Definition;
         Array_Definition               : aliased Asis.Element;
      end record;


   type Formal_Constrained_Array_Node is 
      new Formal_Type_Definition_Node with
      record
         Discrete_Subtype_Definitions   : aliased Primary_Definition_Lists.List;
         Array_Component_Definition     : aliased Asis.Component_Definition;
         Array_Definition               : aliased Asis.Element;
      end record;


   type Formal_Access_Type_Node is 
      new Formal_Type_Definition_Node with
      record
         Access_Type_Kind               : aliased Asis.Access_Type_Kinds := A_Pool_Specific_Access_To_Variable;
         Access_To_Object_Definition    : aliased Asis.Subtype_Indication;
         Access_To_Subprogram_Parameter_Profile : aliased Primary_Parameter_Lists.List;
         Access_To_Function_Result_Subtype : aliased Asis.Definition;
         Access_Definition              : aliased Asis.Element;
      end record;


   type Formal_Interface_Type_Node is 
      new Formal_Type_Definition_Node with
      record
         Interface_Kind                 : aliased Asis.Interface_Kinds := Not_An_Interface;
         Progenitor_List                : aliased Primary_Expression_Lists.List;
         Implicit_Inherited_Subprograms : aliased Secondary_Declaration_Lists.List_Node;
      end record;

end Asis.Gela.Elements.Defs.Formal;
