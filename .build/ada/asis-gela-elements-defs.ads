
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
  
package Asis.Gela.Elements.Defs is

   --------------------------
   -- Type_Definition_Node --
   --------------------------

   type Type_Definition_Node is abstract
      new Definition_Node with private;

   type Type_Definition_Ptr is
      access all Type_Definition_Node;
   for Type_Definition_Ptr'Storage_Pool use Lists.Pool;

   function Corresponding_Type_Operators
     (Element : Type_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Corresponding_Type_Operators
     (Element : in out Type_Definition_Node;
      Item    : in     Asis.Element);

   function Definition_Kind (Element : Type_Definition_Node)
      return Asis.Definition_Kinds;

   -----------------------------
   -- Subtype_Indication_Node --
   -----------------------------

   type Subtype_Indication_Node is 
      new Definition_Node with private;

   type Subtype_Indication_Ptr is
      access all Subtype_Indication_Node;
   for Subtype_Indication_Ptr'Storage_Pool use Lists.Pool;

   function New_Subtype_Indication_Node
     (The_Context : ASIS.Context)
      return Subtype_Indication_Ptr;

   function Get_Subtype_Mark
     (Element : Subtype_Indication_Node) return Asis.Expression;

   procedure Set_Subtype_Mark
     (Element : in out Subtype_Indication_Node;
      Value   : in     Asis.Expression);

   function Subtype_Constraint
     (Element : Subtype_Indication_Node) return Asis.Constraint;

   procedure Set_Subtype_Constraint
     (Element : in out Subtype_Indication_Node;
      Value   : in     Asis.Constraint);

   function Has_Null_Exclusion
     (Element : Subtype_Indication_Node) return Boolean;

   procedure Set_Has_Null_Exclusion
     (Element : in out Subtype_Indication_Node;
      Value   : in     Boolean);

   function Definition_Kind (Element : Subtype_Indication_Node)
      return Asis.Definition_Kinds;

   function Children (Element : access Subtype_Indication_Node)
     return Traverse_List;

   function Clone
     (Element : Subtype_Indication_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Subtype_Indication_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------
   -- Constraint_Node --
   ---------------------

   type Constraint_Node is abstract
      new Definition_Node with private;

   type Constraint_Ptr is
      access all Constraint_Node;
   for Constraint_Ptr'Storage_Pool use Lists.Pool;

   function Definition_Kind (Element : Constraint_Node)
      return Asis.Definition_Kinds;

   -------------------------------
   -- Component_Definition_Node --
   -------------------------------

   type Component_Definition_Node is 
      new Definition_Node with private;

   type Component_Definition_Ptr is
      access all Component_Definition_Node;
   for Component_Definition_Ptr'Storage_Pool use Lists.Pool;

   function New_Component_Definition_Node
     (The_Context : ASIS.Context)
      return Component_Definition_Ptr;

   function Component_Subtype_Indication
     (Element : Component_Definition_Node) return Asis.Subtype_Indication;

   procedure Set_Component_Subtype_Indication
     (Element : in out Component_Definition_Node;
      Value   : in     Asis.Subtype_Indication);

   function Trait_Kind
     (Element : Component_Definition_Node) return Asis.Trait_Kinds;

   procedure Set_Trait_Kind
     (Element : in out Component_Definition_Node;
      Value   : in     Asis.Trait_Kinds);

   function Definition_Kind (Element : Component_Definition_Node)
      return Asis.Definition_Kinds;

   function Children (Element : access Component_Definition_Node)
     return Traverse_List;

   function Clone
     (Element : Component_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Component_Definition_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------------
   -- Discrete_Subtype_Definition_Node --
   --------------------------------------

   type Discrete_Subtype_Definition_Node is abstract
      new Definition_Node with private;

   type Discrete_Subtype_Definition_Ptr is
      access all Discrete_Subtype_Definition_Node;
   for Discrete_Subtype_Definition_Ptr'Storage_Pool use Lists.Pool;

   function Definition_Kind (Element : Discrete_Subtype_Definition_Node)
      return Asis.Definition_Kinds;

   -------------------------
   -- Discrete_Range_Node --
   -------------------------

   type Discrete_Range_Node is abstract
      new Definition_Node with private;

   type Discrete_Range_Ptr is
      access all Discrete_Range_Node;
   for Discrete_Range_Ptr'Storage_Pool use Lists.Pool;

   function Definition_Kind (Element : Discrete_Range_Node)
      return Asis.Definition_Kinds;

   ------------------------------------
   -- Unknown_Discriminant_Part_Node --
   ------------------------------------

   type Unknown_Discriminant_Part_Node is 
      new Definition_Node with private;

   type Unknown_Discriminant_Part_Ptr is
      access all Unknown_Discriminant_Part_Node;
   for Unknown_Discriminant_Part_Ptr'Storage_Pool use Lists.Pool;

   function New_Unknown_Discriminant_Part_Node
     (The_Context : ASIS.Context)
      return Unknown_Discriminant_Part_Ptr;

   function Definition_Kind (Element : Unknown_Discriminant_Part_Node)
      return Asis.Definition_Kinds;

   function Clone
     (Element : Unknown_Discriminant_Part_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   ----------------------------------
   -- Known_Discriminant_Part_Node --
   ----------------------------------

   type Known_Discriminant_Part_Node is 
      new Definition_Node with private;

   type Known_Discriminant_Part_Ptr is
      access all Known_Discriminant_Part_Node;
   for Known_Discriminant_Part_Ptr'Storage_Pool use Lists.Pool;

   function New_Known_Discriminant_Part_Node
     (The_Context : ASIS.Context)
      return Known_Discriminant_Part_Ptr;

   function Discriminants
     (Element : Known_Discriminant_Part_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Discriminants
     (Element : in out Known_Discriminant_Part_Node;
      Value   : in     Asis.Element);

   function Discriminants_List
     (Element : Known_Discriminant_Part_Node) return Asis.Element;

   function Definition_Kind (Element : Known_Discriminant_Part_Node)
      return Asis.Definition_Kinds;

   function Children (Element : access Known_Discriminant_Part_Node)
     return Traverse_List;

   function Clone
     (Element : Known_Discriminant_Part_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Known_Discriminant_Part_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------
   -- Record_Definition_Node --
   ----------------------------

   type Record_Definition_Node is 
      new Definition_Node with private;

   type Record_Definition_Ptr is
      access all Record_Definition_Node;
   for Record_Definition_Ptr'Storage_Pool use Lists.Pool;

   function New_Record_Definition_Node
     (The_Context : ASIS.Context)
      return Record_Definition_Ptr;

   function Record_Components
     (Element : Record_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Record_Components
     (Element : in out Record_Definition_Node;
      Value   : in     Asis.Element);

   function Record_Components_List
     (Element : Record_Definition_Node) return Asis.Element;

   function Implicit_Components
     (Element : Record_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Implicit_Components
     (Element : in out Record_Definition_Node;
      Item    : in     Asis.Element);

   function Definition_Kind (Element : Record_Definition_Node)
      return Asis.Definition_Kinds;

   function Children (Element : access Record_Definition_Node)
     return Traverse_List;

   function Clone
     (Element : Record_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Record_Definition_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------------
   -- Null_Record_Definition_Node --
   ---------------------------------

   type Null_Record_Definition_Node is 
      new Definition_Node with private;

   type Null_Record_Definition_Ptr is
      access all Null_Record_Definition_Node;
   for Null_Record_Definition_Ptr'Storage_Pool use Lists.Pool;

   function New_Null_Record_Definition_Node
     (The_Context : ASIS.Context)
      return Null_Record_Definition_Ptr;

   function Definition_Kind (Element : Null_Record_Definition_Node)
      return Asis.Definition_Kinds;

   function Clone
     (Element : Null_Record_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -------------------------
   -- Null_Component_Node --
   -------------------------

   type Null_Component_Node is 
      new Definition_Node with private;

   type Null_Component_Ptr is
      access all Null_Component_Node;
   for Null_Component_Ptr'Storage_Pool use Lists.Pool;

   function New_Null_Component_Node
     (The_Context : ASIS.Context)
      return Null_Component_Ptr;

   function Pragmas
     (Element : Null_Component_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Pragmas
     (Element : in out Null_Component_Node;
      Value   : in     Asis.Element);

   function Pragmas_List
     (Element : Null_Component_Node) return Asis.Element;

   function Definition_Kind (Element : Null_Component_Node)
      return Asis.Definition_Kinds;

   function Clone
     (Element : Null_Component_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -----------------------
   -- Variant_Part_Node --
   -----------------------

   type Variant_Part_Node is 
      new Definition_Node with private;

   type Variant_Part_Ptr is
      access all Variant_Part_Node;
   for Variant_Part_Ptr'Storage_Pool use Lists.Pool;

   function New_Variant_Part_Node
     (The_Context : ASIS.Context)
      return Variant_Part_Ptr;

   function Discriminant_Direct_Name
     (Element : Variant_Part_Node) return Asis.Name;

   procedure Set_Discriminant_Direct_Name
     (Element : in out Variant_Part_Node;
      Value   : in     Asis.Name);

   function Variants
     (Element : Variant_Part_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Variants
     (Element : in out Variant_Part_Node;
      Value   : in     Asis.Element);

   function Variants_List
     (Element : Variant_Part_Node) return Asis.Element;

   function Pragmas
     (Element : Variant_Part_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Pragmas
     (Element : in out Variant_Part_Node;
      Value   : in     Asis.Element);

   function Pragmas_List
     (Element : Variant_Part_Node) return Asis.Element;

   function End_Pragmas
     (Element : Variant_Part_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_End_Pragmas
     (Element : in out Variant_Part_Node;
      Value   : in     Asis.Element);

   function End_Pragmas_List
     (Element : Variant_Part_Node) return Asis.Element;

   function Definition_Kind (Element : Variant_Part_Node)
      return Asis.Definition_Kinds;

   function Children (Element : access Variant_Part_Node)
     return Traverse_List;

   function Clone
     (Element : Variant_Part_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Variant_Part_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------
   -- Variant_Node --
   ------------------

   type Variant_Node is 
      new Definition_Node with private;

   type Variant_Ptr is
      access all Variant_Node;
   for Variant_Ptr'Storage_Pool use Lists.Pool;

   function New_Variant_Node
     (The_Context : ASIS.Context)
      return Variant_Ptr;

   function Record_Components
     (Element : Variant_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Record_Components
     (Element : in out Variant_Node;
      Value   : in     Asis.Element);

   function Record_Components_List
     (Element : Variant_Node) return Asis.Element;

   function Implicit_Components
     (Element : Variant_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Implicit_Components
     (Element : in out Variant_Node;
      Item    : in     Asis.Element);

   function Variant_Choices
     (Element : Variant_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Variant_Choices
     (Element : in out Variant_Node;
      Value   : in     Asis.Element);

   function Variant_Choices_List
     (Element : Variant_Node) return Asis.Element;

   function Definition_Kind (Element : Variant_Node)
      return Asis.Definition_Kinds;

   function Children (Element : access Variant_Node)
     return Traverse_List;

   function Clone
     (Element : Variant_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Variant_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------
   -- Others_Choice_Node --
   ------------------------

   type Others_Choice_Node is 
      new Definition_Node with private;

   type Others_Choice_Ptr is
      access all Others_Choice_Node;
   for Others_Choice_Ptr'Storage_Pool use Lists.Pool;

   function New_Others_Choice_Node
     (The_Context : ASIS.Context)
      return Others_Choice_Ptr;

   function Definition_Kind (Element : Others_Choice_Node)
      return Asis.Definition_Kinds;

   function Clone
     (Element : Others_Choice_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   ----------------------------
   -- Access_Definition_Node --
   ----------------------------

   type Access_Definition_Node is abstract
      new Definition_Node with private;

   type Access_Definition_Ptr is
      access all Access_Definition_Node;
   for Access_Definition_Ptr'Storage_Pool use Lists.Pool;

   function Has_Null_Exclusion
     (Element : Access_Definition_Node) return Boolean;

   procedure Set_Has_Null_Exclusion
     (Element : in out Access_Definition_Node;
      Value   : in     Boolean);

   function Definition_Kind (Element : Access_Definition_Node)
      return Asis.Definition_Kinds;

   -------------------------------------
   -- Incomplete_Type_Definition_Node --
   -------------------------------------

   type Incomplete_Type_Definition_Node is 
      new Definition_Node with private;

   type Incomplete_Type_Definition_Ptr is
      access all Incomplete_Type_Definition_Node;
   for Incomplete_Type_Definition_Ptr'Storage_Pool use Lists.Pool;

   function New_Incomplete_Type_Definition_Node
     (The_Context : ASIS.Context)
      return Incomplete_Type_Definition_Ptr;

   function Definition_Kind (Element : Incomplete_Type_Definition_Node)
      return Asis.Definition_Kinds;

   function Clone
     (Element : Incomplete_Type_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   --------------------------------------------
   -- Tagged_Incomplete_Type_Definition_Node --
   --------------------------------------------

   type Tagged_Incomplete_Type_Definition_Node is 
      new Incomplete_Type_Definition_Node with private;

   type Tagged_Incomplete_Type_Definition_Ptr is
      access all Tagged_Incomplete_Type_Definition_Node;
   for Tagged_Incomplete_Type_Definition_Ptr'Storage_Pool use Lists.Pool;

   function New_Tagged_Incomplete_Type_Definition_Node
     (The_Context : ASIS.Context)
      return Tagged_Incomplete_Type_Definition_Ptr;

   function Has_Tagged
     (Element : Tagged_Incomplete_Type_Definition_Node) return Boolean;

   procedure Set_Has_Tagged
     (Element : in out Tagged_Incomplete_Type_Definition_Node;
      Value   : in     Boolean);

   function Definition_Kind (Element : Tagged_Incomplete_Type_Definition_Node)
      return Asis.Definition_Kinds;

   function Clone
     (Element : Tagged_Incomplete_Type_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   ----------------------------------
   -- Private_Type_Definition_Node --
   ----------------------------------

   type Private_Type_Definition_Node is 
      new Definition_Node with private;

   type Private_Type_Definition_Ptr is
      access all Private_Type_Definition_Node;
   for Private_Type_Definition_Ptr'Storage_Pool use Lists.Pool;

   function New_Private_Type_Definition_Node
     (The_Context : ASIS.Context)
      return Private_Type_Definition_Ptr;

   function Trait_Kind
     (Element : Private_Type_Definition_Node) return Asis.Trait_Kinds;

   procedure Set_Trait_Kind
     (Element : in out Private_Type_Definition_Node;
      Value   : in     Asis.Trait_Kinds);

   function Corresponding_Type_Operators
     (Element : Private_Type_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Corresponding_Type_Operators
     (Element : in out Private_Type_Definition_Node;
      Item    : in     Asis.Element);

   function Has_Limited
     (Element : Private_Type_Definition_Node) return Boolean;

   procedure Set_Has_Limited
     (Element : in out Private_Type_Definition_Node;
      Value   : in     Boolean);

   function Has_Private
     (Element : Private_Type_Definition_Node) return Boolean;

   procedure Set_Has_Private
     (Element : in out Private_Type_Definition_Node;
      Value   : in     Boolean);

   function Definition_Kind (Element : Private_Type_Definition_Node)
      return Asis.Definition_Kinds;

   function Clone
     (Element : Private_Type_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -----------------------------------------
   -- Tagged_Private_Type_Definition_Node --
   -----------------------------------------

   type Tagged_Private_Type_Definition_Node is 
      new Private_Type_Definition_Node with private;

   type Tagged_Private_Type_Definition_Ptr is
      access all Tagged_Private_Type_Definition_Node;
   for Tagged_Private_Type_Definition_Ptr'Storage_Pool use Lists.Pool;

   function New_Tagged_Private_Type_Definition_Node
     (The_Context : ASIS.Context)
      return Tagged_Private_Type_Definition_Ptr;

   function Has_Abstract
     (Element : Tagged_Private_Type_Definition_Node) return Boolean;

   procedure Set_Has_Abstract
     (Element : in out Tagged_Private_Type_Definition_Node;
      Value   : in     Boolean);

   function Has_Tagged
     (Element : Tagged_Private_Type_Definition_Node) return Boolean;

   procedure Set_Has_Tagged
     (Element : in out Tagged_Private_Type_Definition_Node;
      Value   : in     Boolean);

   function Definition_Kind (Element : Tagged_Private_Type_Definition_Node)
      return Asis.Definition_Kinds;

   function Clone
     (Element : Tagged_Private_Type_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   ---------------------------------------
   -- Private_Extension_Definition_Node --
   ---------------------------------------

   type Private_Extension_Definition_Node is 
      new Private_Type_Definition_Node with private;

   type Private_Extension_Definition_Ptr is
      access all Private_Extension_Definition_Node;
   for Private_Extension_Definition_Ptr'Storage_Pool use Lists.Pool;

   function New_Private_Extension_Definition_Node
     (The_Context : ASIS.Context)
      return Private_Extension_Definition_Ptr;

   function Ancestor_Subtype_Indication
     (Element : Private_Extension_Definition_Node) return Asis.Subtype_Indication;

   procedure Set_Ancestor_Subtype_Indication
     (Element : in out Private_Extension_Definition_Node;
      Value   : in     Asis.Subtype_Indication);

   function Implicit_Inherited_Declarations
     (Element : Private_Extension_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Implicit_Inherited_Declarations
     (Element : in out Private_Extension_Definition_Node;
      Item    : in     Asis.Element);

   function Implicit_Inherited_Subprograms
     (Element : Private_Extension_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Implicit_Inherited_Subprograms
     (Element : in out Private_Extension_Definition_Node;
      Item    : in     Asis.Element);

   function Has_Synchronized
     (Element : Private_Extension_Definition_Node) return Boolean;

   procedure Set_Has_Synchronized
     (Element : in out Private_Extension_Definition_Node;
      Value   : in     Boolean);

   function Has_Abstract
     (Element : Private_Extension_Definition_Node) return Boolean;

   procedure Set_Has_Abstract
     (Element : in out Private_Extension_Definition_Node;
      Value   : in     Boolean);

   function Definition_Kind (Element : Private_Extension_Definition_Node)
      return Asis.Definition_Kinds;

   function Children (Element : access Private_Extension_Definition_Node)
     return Traverse_List;

   function Clone
     (Element : Private_Extension_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Private_Extension_Definition_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------
   -- Protected_Definition_Node --
   -------------------------------

   type Protected_Definition_Node is 
      new Definition_Node with private;

   type Protected_Definition_Ptr is
      access all Protected_Definition_Node;
   for Protected_Definition_Ptr'Storage_Pool use Lists.Pool;

   function New_Protected_Definition_Node
     (The_Context : ASIS.Context)
      return Protected_Definition_Ptr;

   function Is_Private_Present
     (Element : Protected_Definition_Node) return Boolean;

   procedure Set_Is_Private_Present
     (Element : in out Protected_Definition_Node;
      Value   : in     Boolean);

   function Visible_Part_Items
     (Element : Protected_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Visible_Part_Items
     (Element : in out Protected_Definition_Node;
      Value   : in     Asis.Element);

   function Visible_Part_Items_List
     (Element : Protected_Definition_Node) return Asis.Element;

   function Private_Part_Items
     (Element : Protected_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Private_Part_Items
     (Element : in out Protected_Definition_Node;
      Value   : in     Asis.Element);

   function Private_Part_Items_List
     (Element : Protected_Definition_Node) return Asis.Element;

   function Get_Identifier
     (Element : Protected_Definition_Node) return Asis.Element;

   procedure Set_Identifier
     (Element : in out Protected_Definition_Node;
      Value   : in     Asis.Element);

   function Corresponding_Type_Operators
     (Element : Protected_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Corresponding_Type_Operators
     (Element : in out Protected_Definition_Node;
      Item    : in     Asis.Element);

   function Definition_Kind (Element : Protected_Definition_Node)
      return Asis.Definition_Kinds;

   function Children (Element : access Protected_Definition_Node)
     return Traverse_List;

   function Clone
     (Element : Protected_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Protected_Definition_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------
   -- Task_Definition_Node --
   --------------------------

   type Task_Definition_Node is 
      new Protected_Definition_Node with private;

   type Task_Definition_Ptr is
      access all Task_Definition_Node;
   for Task_Definition_Ptr'Storage_Pool use Lists.Pool;

   function New_Task_Definition_Node
     (The_Context : ASIS.Context)
      return Task_Definition_Ptr;

   function Is_Task_Definition_Present
     (Element : Task_Definition_Node) return Boolean;

   procedure Set_Is_Task_Definition_Present
     (Element : in out Task_Definition_Node;
      Value   : in     Boolean);

   function Definition_Kind (Element : Task_Definition_Node)
      return Asis.Definition_Kinds;

   function Clone
     (Element : Task_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Task_Definition_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------------
   -- Formal_Type_Definition_Node --
   ---------------------------------

   type Formal_Type_Definition_Node is abstract
      new Definition_Node with private;

   type Formal_Type_Definition_Ptr is
      access all Formal_Type_Definition_Node;
   for Formal_Type_Definition_Ptr'Storage_Pool use Lists.Pool;

   function Corresponding_Type_Operators
     (Element : Formal_Type_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Corresponding_Type_Operators
     (Element : in out Formal_Type_Definition_Node;
      Item    : in     Asis.Element);

   function Definition_Kind (Element : Formal_Type_Definition_Node)
      return Asis.Definition_Kinds;

private


   type Type_Definition_Node is abstract
      new Definition_Node with
      record
         Corresponding_Type_Operators   : aliased Secondary_Declaration_Lists.List_Node;
      end record;


   type Subtype_Indication_Node is 
      new Definition_Node with
      record
         Subtype_Mark                   : aliased Asis.Expression;
         Subtype_Constraint             : aliased Asis.Constraint;
         Has_Null_Exclusion             : aliased Boolean := False;
      end record;


   type Constraint_Node is abstract
      new Definition_Node with
      record
         null;
      end record;


   type Component_Definition_Node is 
      new Definition_Node with
      record
         Component_Subtype_Indication   : aliased Asis.Subtype_Indication;
         Trait_Kind                     : aliased Asis.Trait_Kinds := An_Ordinary_Trait;
      end record;


   type Discrete_Subtype_Definition_Node is abstract
      new Definition_Node with
      record
         null;
      end record;


   type Discrete_Range_Node is abstract
      new Definition_Node with
      record
         null;
      end record;


   type Unknown_Discriminant_Part_Node is 
      new Definition_Node with
      record
         null;
      end record;


   type Known_Discriminant_Part_Node is 
      new Definition_Node with
      record
         Discriminants                  : aliased Primary_Declaration_Lists.List;
      end record;


   type Record_Definition_Node is 
      new Definition_Node with
      record
         Record_Components              : aliased Primary_Declaration_Lists.List;
         Implicit_Components            : aliased Secondary_Declaration_Lists.List_Node;
      end record;


   type Null_Record_Definition_Node is 
      new Definition_Node with
      record
         null;
      end record;


   type Null_Component_Node is 
      new Definition_Node with
      record
         Pragmas                        : aliased Primary_Pragma_Lists.List;
      end record;


   type Variant_Part_Node is 
      new Definition_Node with
      record
         Discriminant_Direct_Name       : aliased Asis.Name;
         Variants                       : aliased Primary_Variant_Lists.List;
         Pragmas                        : aliased Primary_Pragma_Lists.List;
         End_Pragmas                    : aliased Primary_Pragma_Lists.List;
      end record;


   type Variant_Node is 
      new Definition_Node with
      record
         Record_Components              : aliased Primary_Declaration_Lists.List;
         Implicit_Components            : aliased Secondary_Declaration_Lists.List_Node;
         Variant_Choices                : aliased Primary_Choise_Lists.List;
      end record;


   type Others_Choice_Node is 
      new Definition_Node with
      record
         null;
      end record;


   type Access_Definition_Node is abstract
      new Definition_Node with
      record
         Has_Null_Exclusion             : aliased Boolean := False;
      end record;


   type Incomplete_Type_Definition_Node is 
      new Definition_Node with
      record
         null;
      end record;


   type Tagged_Incomplete_Type_Definition_Node is 
      new Incomplete_Type_Definition_Node with
      record
         Has_Tagged                     : aliased Boolean := False;
      end record;


   type Private_Type_Definition_Node is 
      new Definition_Node with
      record
         Trait_Kind                     : aliased Asis.Trait_Kinds := An_Ordinary_Trait;
         Corresponding_Type_Operators   : aliased Secondary_Declaration_Lists.List_Node;
         Has_Limited                    : aliased Boolean := False;
         Has_Private                    : aliased Boolean := False;
      end record;


   type Tagged_Private_Type_Definition_Node is 
      new Private_Type_Definition_Node with
      record
         Has_Abstract                   : aliased Boolean := False;
         Has_Tagged                     : aliased Boolean := False;
      end record;


   type Private_Extension_Definition_Node is 
      new Private_Type_Definition_Node with
      record
         Ancestor_Subtype_Indication    : aliased Asis.Subtype_Indication;
         Implicit_Inherited_Declarations : aliased Secondary_Declaration_Lists.List_Node;
         Implicit_Inherited_Subprograms : aliased Secondary_Declaration_Lists.List_Node;
         Has_Synchronized               : aliased Boolean := False;
         Has_Abstract                   : aliased Boolean := False;
      end record;


   type Protected_Definition_Node is 
      new Definition_Node with
      record
         Is_Private_Present             : aliased Boolean := False;
         Visible_Part_Items             : aliased Primary_Declaration_Lists.List;
         Private_Part_Items             : aliased Primary_Declaration_Lists.List;
         Identifier                     : aliased Asis.Element;
         Corresponding_Type_Operators   : aliased Secondary_Declaration_Lists.List_Node;
      end record;


   type Task_Definition_Node is 
      new Protected_Definition_Node with
      record
         Is_Task_Definition_Present     : aliased Boolean := False;
      end record;


   type Formal_Type_Definition_Node is abstract
      new Definition_Node with
      record
         Corresponding_Type_Operators   : aliased Secondary_Declaration_Lists.List_Node;
      end record;

end Asis.Gela.Elements.Defs;
