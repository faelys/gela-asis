
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
  
package Asis.Gela.Elements.Decl is

   --------------------------------
   -- Base_Type_Declaration_Node --
   --------------------------------

   type Base_Type_Declaration_Node is abstract
      new Declaration_Node with private;

   type Base_Type_Declaration_Ptr is
      access all Base_Type_Declaration_Node;
   for Base_Type_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function Discriminant_Part
     (Element : Base_Type_Declaration_Node) return Asis.Definition;

   procedure Set_Discriminant_Part
     (Element : in out Base_Type_Declaration_Node;
      Value   : in     Asis.Definition);

   function Type_Declaration_View
     (Element : Base_Type_Declaration_Node) return Asis.Definition;

   procedure Set_Type_Declaration_View
     (Element : in out Base_Type_Declaration_Node;
      Value   : in     Asis.Definition);

   function Children (Element : access Base_Type_Declaration_Node)
     return Traverse_List;

   ------------------------------------
   -- Ordinary_Type_Declaration_Node --
   ------------------------------------

   type Ordinary_Type_Declaration_Node is 
      new Base_Type_Declaration_Node with private;

   type Ordinary_Type_Declaration_Ptr is
      access all Ordinary_Type_Declaration_Node;
   for Ordinary_Type_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Ordinary_Type_Declaration_Node
     (The_Context : ASIS.Context)
      return Ordinary_Type_Declaration_Ptr;

   function Corresponding_Type_Declaration
     (Element : Ordinary_Type_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Type_Declaration
     (Element : in out Ordinary_Type_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Declaration_Kind (Element : Ordinary_Type_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Ordinary_Type_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Ordinary_Type_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------------
   -- Protected_Type_Declaration_Node --
   -------------------------------------

   type Protected_Type_Declaration_Node is 
      new Ordinary_Type_Declaration_Node with private;

   type Protected_Type_Declaration_Ptr is
      access all Protected_Type_Declaration_Node;
   for Protected_Type_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Protected_Type_Declaration_Node
     (The_Context : ASIS.Context)
      return Protected_Type_Declaration_Ptr;

   function Is_Name_Repeated
     (Element : Protected_Type_Declaration_Node) return Boolean;

   procedure Set_Is_Name_Repeated
     (Element : in out Protected_Type_Declaration_Node;
      Value   : in     Boolean);

   function Corresponding_Body
     (Element : Protected_Type_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Body
     (Element : in out Protected_Type_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Progenitor_List
     (Element : Protected_Type_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Progenitor_List
     (Element : in out Protected_Type_Declaration_Node;
      Value   : in     Asis.Element);

   function Progenitor_List_List
     (Element : Protected_Type_Declaration_Node) return Asis.Element;

   function Declaration_Kind (Element : Protected_Type_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Protected_Type_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Protected_Type_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Protected_Type_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------
   -- Task_Type_Declaration_Node --
   --------------------------------

   type Task_Type_Declaration_Node is 
      new Protected_Type_Declaration_Node with private;

   type Task_Type_Declaration_Ptr is
      access all Task_Type_Declaration_Node;
   for Task_Type_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Task_Type_Declaration_Node
     (The_Context : ASIS.Context)
      return Task_Type_Declaration_Ptr;

   function Declaration_Kind (Element : Task_Type_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Task_Type_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Task_Type_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------------
   -- Private_Type_Declaration_Node --
   -----------------------------------

   type Private_Type_Declaration_Node is 
      new Ordinary_Type_Declaration_Node with private;

   type Private_Type_Declaration_Ptr is
      access all Private_Type_Declaration_Node;
   for Private_Type_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Private_Type_Declaration_Node
     (The_Context : ASIS.Context)
      return Private_Type_Declaration_Ptr;

   function Trait_Kind
     (Element : Private_Type_Declaration_Node) return Asis.Trait_Kinds;

   procedure Set_Trait_Kind
     (Element : in out Private_Type_Declaration_Node;
      Value   : in     Asis.Trait_Kinds);

   function Declaration_Kind (Element : Private_Type_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Private_Type_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Private_Type_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------------
   -- Private_Extension_Declaration_Node --
   ----------------------------------------

   type Private_Extension_Declaration_Node is 
      new Private_Type_Declaration_Node with private;

   type Private_Extension_Declaration_Ptr is
      access all Private_Extension_Declaration_Node;
   for Private_Extension_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Private_Extension_Declaration_Node
     (The_Context : ASIS.Context)
      return Private_Extension_Declaration_Ptr;

   function Progenitor_List
     (Element : Private_Extension_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Progenitor_List
     (Element : in out Private_Extension_Declaration_Node;
      Value   : in     Asis.Element);

   function Progenitor_List_List
     (Element : Private_Extension_Declaration_Node) return Asis.Element;

   function Declaration_Kind (Element : Private_Extension_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Private_Extension_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Private_Extension_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Private_Extension_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------
   -- Formal_Type_Declaration_Node --
   ----------------------------------

   type Formal_Type_Declaration_Node is 
      new Base_Type_Declaration_Node with private;

   type Formal_Type_Declaration_Ptr is
      access all Formal_Type_Declaration_Node;
   for Formal_Type_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Type_Declaration_Node
     (The_Context : ASIS.Context)
      return Formal_Type_Declaration_Ptr;

   function Generic_Actual
     (Element : Formal_Type_Declaration_Node) return Asis.Expression;

   procedure Set_Generic_Actual
     (Element : in out Formal_Type_Declaration_Node;
      Value   : in     Asis.Expression);

   function Declaration_Kind (Element : Formal_Type_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Formal_Type_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Type_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------------
   -- Base_Callable_Declaration_Node --
   ------------------------------------

   type Base_Callable_Declaration_Node is abstract
      new Declaration_Node with private;

   type Base_Callable_Declaration_Ptr is
      access all Base_Callable_Declaration_Node;
   for Base_Callable_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function Parameter_Profile
     (Element : Base_Callable_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Parameter_Profile
     (Element : in out Base_Callable_Declaration_Node;
      Value   : in     Asis.Element);

   function Parameter_Profile_List
     (Element : Base_Callable_Declaration_Node) return Asis.Element;

   function Corresponding_Body
     (Element : Base_Callable_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Body
     (Element : in out Base_Callable_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Specification
     (Element : Base_Callable_Declaration_Node) return Asis.Element;

   procedure Set_Specification
     (Element : in out Base_Callable_Declaration_Node;
      Value   : in     Asis.Element);

   function Children (Element : access Base_Callable_Declaration_Node)
     return Traverse_List;

   --------------------------------
   -- Procedure_Declaration_Node --
   --------------------------------

   type Procedure_Declaration_Node is 
      new Base_Callable_Declaration_Node with private;

   type Procedure_Declaration_Ptr is
      access all Procedure_Declaration_Node;
   for Procedure_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Procedure_Declaration_Node
     (The_Context : ASIS.Context)
      return Procedure_Declaration_Ptr;

   function Corresponding_Subprogram_Derivation
     (Element : Procedure_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Subprogram_Derivation
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Corresponding_Type
     (Element : Procedure_Declaration_Node) return Asis.Type_Definition;

   procedure Set_Corresponding_Type
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Asis.Type_Definition);

   function Is_Dispatching_Operation
     (Element : Procedure_Declaration_Node) return Boolean;

   procedure Set_Is_Dispatching_Operation
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Boolean);

   function Trait_Kind
     (Element : Procedure_Declaration_Node) return Asis.Trait_Kinds;

   procedure Set_Trait_Kind
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Asis.Trait_Kinds);

   function Overriding_Indicator_Kind
     (Element : Procedure_Declaration_Node) return Asis.Overriding_Indicator_Kinds;

   procedure Set_Overriding_Indicator_Kind
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Asis.Overriding_Indicator_Kinds);

   function Has_Abstract
     (Element : Procedure_Declaration_Node) return Boolean;

   procedure Set_Has_Abstract
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Boolean);

   function Is_Null_Procedure
     (Element : Procedure_Declaration_Node) return Boolean;

   procedure Set_Is_Null_Procedure
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Boolean);

   function Generic_Formal_Part
     (Element : Procedure_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Generic_Formal_Part
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Asis.Element);

   function Generic_Formal_Part_List
     (Element : Procedure_Declaration_Node) return Asis.Element;

   function Declaration_Kind (Element : Procedure_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Procedure_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Procedure_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Procedure_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------
   -- Function_Declaration_Node --
   -------------------------------

   type Function_Declaration_Node is 
      new Procedure_Declaration_Node with private;

   type Function_Declaration_Ptr is
      access all Function_Declaration_Node;
   for Function_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Function_Declaration_Node
     (The_Context : ASIS.Context)
      return Function_Declaration_Ptr;

   function Result_Subtype
     (Element : Function_Declaration_Node) return Asis.Definition;

   procedure Set_Result_Subtype
     (Element : in out Function_Declaration_Node;
      Value   : in     Asis.Definition);

   function Corresponding_Equality_Operator
     (Element : Function_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Equality_Operator
     (Element : in out Function_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Declaration_Kind (Element : Function_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Function_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Function_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Function_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------------------
   -- Procedure_Renaming_Declaration_Node --
   -----------------------------------------

   type Procedure_Renaming_Declaration_Node is 
      new Base_Callable_Declaration_Node with private;

   type Procedure_Renaming_Declaration_Ptr is
      access all Procedure_Renaming_Declaration_Node;
   for Procedure_Renaming_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Procedure_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Procedure_Renaming_Declaration_Ptr;

   function Is_Dispatching_Operation
     (Element : Procedure_Renaming_Declaration_Node) return Boolean;

   procedure Set_Is_Dispatching_Operation
     (Element : in out Procedure_Renaming_Declaration_Node;
      Value   : in     Boolean);

   function Renamed_Entity
     (Element : Procedure_Renaming_Declaration_Node) return Asis.Expression;

   procedure Set_Renamed_Entity
     (Element : in out Procedure_Renaming_Declaration_Node;
      Value   : in     Asis.Expression);

   function Corresponding_Base_Entity
     (Element : Procedure_Renaming_Declaration_Node) return Asis.Expression;

   procedure Set_Corresponding_Base_Entity
     (Element : in out Procedure_Renaming_Declaration_Node;
      Value   : in     Asis.Expression);

   function Corresponding_Declaration
     (Element : Procedure_Renaming_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Declaration
     (Element : in out Procedure_Renaming_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Overriding_Indicator_Kind
     (Element : Procedure_Renaming_Declaration_Node) return Asis.Overriding_Indicator_Kinds;

   procedure Set_Overriding_Indicator_Kind
     (Element : in out Procedure_Renaming_Declaration_Node;
      Value   : in     Asis.Overriding_Indicator_Kinds);

   function Declaration_Kind (Element : Procedure_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Procedure_Renaming_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Procedure_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Procedure_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------------
   -- Function_Renaming_Declaration_Node --
   ----------------------------------------

   type Function_Renaming_Declaration_Node is 
      new Procedure_Renaming_Declaration_Node with private;

   type Function_Renaming_Declaration_Ptr is
      access all Function_Renaming_Declaration_Node;
   for Function_Renaming_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Function_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Function_Renaming_Declaration_Ptr;

   function Result_Subtype
     (Element : Function_Renaming_Declaration_Node) return Asis.Definition;

   procedure Set_Result_Subtype
     (Element : in out Function_Renaming_Declaration_Node;
      Value   : in     Asis.Definition);

   function Corresponding_Equality_Operator
     (Element : Function_Renaming_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Equality_Operator
     (Element : in out Function_Renaming_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Declaration_Kind (Element : Function_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Function_Renaming_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Function_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Function_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------
   -- Entry_Declaration_Node --
   ----------------------------

   type Entry_Declaration_Node is 
      new Base_Callable_Declaration_Node with private;

   type Entry_Declaration_Ptr is
      access all Entry_Declaration_Node;
   for Entry_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Entry_Declaration_Node
     (The_Context : ASIS.Context)
      return Entry_Declaration_Ptr;

   function Entry_Family_Definition
     (Element : Entry_Declaration_Node) return Asis.Discrete_Subtype_Definition;

   procedure Set_Entry_Family_Definition
     (Element : in out Entry_Declaration_Node;
      Value   : in     Asis.Discrete_Subtype_Definition);

   function Overriding_Indicator_Kind
     (Element : Entry_Declaration_Node) return Asis.Overriding_Indicator_Kinds;

   procedure Set_Overriding_Indicator_Kind
     (Element : in out Entry_Declaration_Node;
      Value   : in     Asis.Overriding_Indicator_Kinds);

   function Declaration_Kind (Element : Entry_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Entry_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Entry_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Entry_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------
   -- Procedure_Body_Stub_Node --
   ------------------------------

   type Procedure_Body_Stub_Node is 
      new Base_Callable_Declaration_Node with private;

   type Procedure_Body_Stub_Ptr is
      access all Procedure_Body_Stub_Node;
   for Procedure_Body_Stub_Ptr'Storage_Pool use Lists.Pool;

   function New_Procedure_Body_Stub_Node
     (The_Context : ASIS.Context)
      return Procedure_Body_Stub_Ptr;

   function Corresponding_Subunit
     (Element : Procedure_Body_Stub_Node) return Asis.Declaration;

   procedure Set_Corresponding_Subunit
     (Element : in out Procedure_Body_Stub_Node;
      Value   : in     Asis.Declaration);

   function Corresponding_Declaration
     (Element : Procedure_Body_Stub_Node) return Asis.Declaration;

   procedure Set_Corresponding_Declaration
     (Element : in out Procedure_Body_Stub_Node;
      Value   : in     Asis.Declaration);

   function Overriding_Indicator_Kind
     (Element : Procedure_Body_Stub_Node) return Asis.Overriding_Indicator_Kinds;

   procedure Set_Overriding_Indicator_Kind
     (Element : in out Procedure_Body_Stub_Node;
      Value   : in     Asis.Overriding_Indicator_Kinds);

   function Declaration_Kind (Element : Procedure_Body_Stub_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Procedure_Body_Stub_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Procedure_Body_Stub_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------
   -- Function_Body_Stub_Node --
   -----------------------------

   type Function_Body_Stub_Node is 
      new Procedure_Body_Stub_Node with private;

   type Function_Body_Stub_Ptr is
      access all Function_Body_Stub_Node;
   for Function_Body_Stub_Ptr'Storage_Pool use Lists.Pool;

   function New_Function_Body_Stub_Node
     (The_Context : ASIS.Context)
      return Function_Body_Stub_Ptr;

   function Result_Subtype
     (Element : Function_Body_Stub_Node) return Asis.Definition;

   procedure Set_Result_Subtype
     (Element : in out Function_Body_Stub_Node;
      Value   : in     Asis.Definition);

   function Declaration_Kind (Element : Function_Body_Stub_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Function_Body_Stub_Node)
     return Traverse_List;

   function Clone
     (Element : Function_Body_Stub_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Function_Body_Stub_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------------
   -- Generic_Procedure_Declaration_Node --
   ----------------------------------------

   type Generic_Procedure_Declaration_Node is 
      new Base_Callable_Declaration_Node with private;

   type Generic_Procedure_Declaration_Ptr is
      access all Generic_Procedure_Declaration_Node;
   for Generic_Procedure_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Generic_Procedure_Declaration_Node
     (The_Context : ASIS.Context)
      return Generic_Procedure_Declaration_Ptr;

   function Generic_Formal_Part
     (Element : Generic_Procedure_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Generic_Formal_Part
     (Element : in out Generic_Procedure_Declaration_Node;
      Value   : in     Asis.Element);

   function Generic_Formal_Part_List
     (Element : Generic_Procedure_Declaration_Node) return Asis.Element;

   function Declaration_Kind (Element : Generic_Procedure_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Generic_Procedure_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Generic_Procedure_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Generic_Procedure_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------------------
   -- Generic_Function_Declaration_Node --
   ---------------------------------------

   type Generic_Function_Declaration_Node is 
      new Generic_Procedure_Declaration_Node with private;

   type Generic_Function_Declaration_Ptr is
      access all Generic_Function_Declaration_Node;
   for Generic_Function_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Generic_Function_Declaration_Node
     (The_Context : ASIS.Context)
      return Generic_Function_Declaration_Ptr;

   function Result_Subtype
     (Element : Generic_Function_Declaration_Node) return Asis.Definition;

   procedure Set_Result_Subtype
     (Element : in out Generic_Function_Declaration_Node;
      Value   : in     Asis.Definition);

   function Declaration_Kind (Element : Generic_Function_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Generic_Function_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Generic_Function_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Generic_Function_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------
   -- Base_Body_Declaration_Node --
   --------------------------------

   type Base_Body_Declaration_Node is abstract
      new Declaration_Node with private;

   type Base_Body_Declaration_Ptr is
      access all Base_Body_Declaration_Node;
   for Base_Body_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function Body_Declarative_Items
     (Element : Base_Body_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Body_Declarative_Items
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Asis.Element);

   function Body_Declarative_Items_List
     (Element : Base_Body_Declaration_Node) return Asis.Element;

   function Body_Statements
     (Element : Base_Body_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Body_Statements
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Asis.Element);

   function Body_Statements_List
     (Element : Base_Body_Declaration_Node) return Asis.Element;

   function Body_Exception_Handlers
     (Element : Base_Body_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Body_Exception_Handlers
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Asis.Element);

   function Body_Exception_Handlers_List
     (Element : Base_Body_Declaration_Node) return Asis.Element;

   function Corresponding_Declaration
     (Element : Base_Body_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Declaration
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Corresponding_Body_Stub
     (Element : Base_Body_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Body_Stub
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Is_Name_Repeated
     (Element : Base_Body_Declaration_Node) return Boolean;

   procedure Set_Is_Name_Repeated
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Boolean);

   function Handled_Statements
     (Element : Base_Body_Declaration_Node) return Asis.Element;

   procedure Set_Handled_Statements
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Asis.Element);

   function Compound_Name
     (Element : Base_Body_Declaration_Node) return Asis.Element;

   procedure Set_Compound_Name
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Asis.Element);

   function Children (Element : access Base_Body_Declaration_Node)
     return Traverse_List;

   -------------------------------------
   -- Procedure_Body_Declaration_Node --
   -------------------------------------

   type Procedure_Body_Declaration_Node is 
      new Base_Body_Declaration_Node with private;

   type Procedure_Body_Declaration_Ptr is
      access all Procedure_Body_Declaration_Node;
   for Procedure_Body_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Procedure_Body_Declaration_Node
     (The_Context : ASIS.Context)
      return Procedure_Body_Declaration_Ptr;

   function Parameter_Profile
     (Element : Procedure_Body_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Parameter_Profile
     (Element : in out Procedure_Body_Declaration_Node;
      Value   : in     Asis.Element);

   function Parameter_Profile_List
     (Element : Procedure_Body_Declaration_Node) return Asis.Element;

   function Specification
     (Element : Procedure_Body_Declaration_Node) return Asis.Element;

   procedure Set_Specification
     (Element : in out Procedure_Body_Declaration_Node;
      Value   : in     Asis.Element);

   function Overriding_Indicator_Kind
     (Element : Procedure_Body_Declaration_Node) return Asis.Overriding_Indicator_Kinds;

   procedure Set_Overriding_Indicator_Kind
     (Element : in out Procedure_Body_Declaration_Node;
      Value   : in     Asis.Overriding_Indicator_Kinds);

   function Declaration_Kind (Element : Procedure_Body_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Procedure_Body_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Procedure_Body_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Procedure_Body_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------------
   -- Function_Body_Declaration_Node --
   ------------------------------------

   type Function_Body_Declaration_Node is 
      new Procedure_Body_Declaration_Node with private;

   type Function_Body_Declaration_Ptr is
      access all Function_Body_Declaration_Node;
   for Function_Body_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Function_Body_Declaration_Node
     (The_Context : ASIS.Context)
      return Function_Body_Declaration_Ptr;

   function Result_Subtype
     (Element : Function_Body_Declaration_Node) return Asis.Definition;

   procedure Set_Result_Subtype
     (Element : in out Function_Body_Declaration_Node;
      Value   : in     Asis.Definition);

   function Declaration_Kind (Element : Function_Body_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Function_Body_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Function_Body_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Function_Body_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------------
   -- Package_Body_Declaration_Node --
   -----------------------------------

   type Package_Body_Declaration_Node is 
      new Base_Body_Declaration_Node with private;

   type Package_Body_Declaration_Ptr is
      access all Package_Body_Declaration_Node;
   for Package_Body_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Package_Body_Declaration_Node
     (The_Context : ASIS.Context)
      return Package_Body_Declaration_Ptr;

   function Declaration_Kind (Element : Package_Body_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Package_Body_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Package_Body_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------
   -- Task_Body_Declaration_Node --
   --------------------------------

   type Task_Body_Declaration_Node is 
      new Package_Body_Declaration_Node with private;

   type Task_Body_Declaration_Ptr is
      access all Task_Body_Declaration_Node;
   for Task_Body_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Task_Body_Declaration_Node
     (The_Context : ASIS.Context)
      return Task_Body_Declaration_Ptr;

   function Declaration_Kind (Element : Task_Body_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Task_Body_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Task_Body_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------------
   -- Entry_Body_Declaration_Node --
   ---------------------------------

   type Entry_Body_Declaration_Node is 
      new Base_Body_Declaration_Node with private;

   type Entry_Body_Declaration_Ptr is
      access all Entry_Body_Declaration_Node;
   for Entry_Body_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Entry_Body_Declaration_Node
     (The_Context : ASIS.Context)
      return Entry_Body_Declaration_Ptr;

   function Parameter_Profile
     (Element : Entry_Body_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Parameter_Profile
     (Element : in out Entry_Body_Declaration_Node;
      Value   : in     Asis.Element);

   function Parameter_Profile_List
     (Element : Entry_Body_Declaration_Node) return Asis.Element;

   function Entry_Index_Specification
     (Element : Entry_Body_Declaration_Node) return Asis.Declaration;

   procedure Set_Entry_Index_Specification
     (Element : in out Entry_Body_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Entry_Barrier
     (Element : Entry_Body_Declaration_Node) return Asis.Expression;

   procedure Set_Entry_Barrier
     (Element : in out Entry_Body_Declaration_Node;
      Value   : in     Asis.Expression);

   function Specification
     (Element : Entry_Body_Declaration_Node) return Asis.Element;

   procedure Set_Specification
     (Element : in out Entry_Body_Declaration_Node;
      Value   : in     Asis.Element);

   function Declaration_Kind (Element : Entry_Body_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Entry_Body_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Entry_Body_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Entry_Body_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------------
   -- Base_Renaming_Declaration_Node --
   ------------------------------------

   type Base_Renaming_Declaration_Node is abstract
      new Declaration_Node with private;

   type Base_Renaming_Declaration_Ptr is
      access all Base_Renaming_Declaration_Node;
   for Base_Renaming_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function Renamed_Entity
     (Element : Base_Renaming_Declaration_Node) return Asis.Expression;

   procedure Set_Renamed_Entity
     (Element : in out Base_Renaming_Declaration_Node;
      Value   : in     Asis.Expression);

   function Corresponding_Base_Entity
     (Element : Base_Renaming_Declaration_Node) return Asis.Expression;

   procedure Set_Corresponding_Base_Entity
     (Element : in out Base_Renaming_Declaration_Node;
      Value   : in     Asis.Expression);

   function Children (Element : access Base_Renaming_Declaration_Node)
     return Traverse_List;

   --------------------------------------
   -- Object_Renaming_Declaration_Node --
   --------------------------------------

   type Object_Renaming_Declaration_Node is 
      new Base_Renaming_Declaration_Node with private;

   type Object_Renaming_Declaration_Ptr is
      access all Object_Renaming_Declaration_Node;
   for Object_Renaming_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Object_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Object_Renaming_Declaration_Ptr;

   function Object_Declaration_Subtype
     (Element : Object_Renaming_Declaration_Node) return Asis.Definition;

   procedure Set_Object_Declaration_Subtype
     (Element : in out Object_Renaming_Declaration_Node;
      Value   : in     Asis.Definition);

   function Has_Null_Exclusion
     (Element : Object_Renaming_Declaration_Node) return Boolean;

   procedure Set_Has_Null_Exclusion
     (Element : in out Object_Renaming_Declaration_Node;
      Value   : in     Boolean);

   function Declaration_Kind (Element : Object_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Object_Renaming_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Object_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Object_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------------------
   -- Exception_Renaming_Declaration_Node --
   -----------------------------------------

   type Exception_Renaming_Declaration_Node is 
      new Base_Renaming_Declaration_Node with private;

   type Exception_Renaming_Declaration_Ptr is
      access all Exception_Renaming_Declaration_Node;
   for Exception_Renaming_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Exception_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Exception_Renaming_Declaration_Ptr;

   function Declaration_Kind (Element : Exception_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Exception_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Exception_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------------------
   -- Package_Renaming_Declaration_Node --
   ---------------------------------------

   type Package_Renaming_Declaration_Node is 
      new Base_Renaming_Declaration_Node with private;

   type Package_Renaming_Declaration_Ptr is
      access all Package_Renaming_Declaration_Node;
   for Package_Renaming_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Package_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Package_Renaming_Declaration_Ptr;

   function Declaration_Kind (Element : Package_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Package_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Package_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------------------------
   -- Generic_Package_Renaming_Declaration_Node --
   -----------------------------------------------

   type Generic_Package_Renaming_Declaration_Node is 
      new Package_Renaming_Declaration_Node with private;

   type Generic_Package_Renaming_Declaration_Ptr is
      access all Generic_Package_Renaming_Declaration_Node;
   for Generic_Package_Renaming_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Generic_Package_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Generic_Package_Renaming_Declaration_Ptr;

   function Empty_Generic_Part
     (Element : Generic_Package_Renaming_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Empty_Generic_Part
     (Element : in out Generic_Package_Renaming_Declaration_Node;
      Value   : in     Asis.Element);

   function Empty_Generic_Part_List
     (Element : Generic_Package_Renaming_Declaration_Node) return Asis.Element;

   function Declaration_Kind (Element : Generic_Package_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Generic_Package_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Generic_Package_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------------------------
   -- Generic_Procedure_Renaming_Declaration_Node --
   -------------------------------------------------

   type Generic_Procedure_Renaming_Declaration_Node is 
      new Generic_Package_Renaming_Declaration_Node with private;

   type Generic_Procedure_Renaming_Declaration_Ptr is
      access all Generic_Procedure_Renaming_Declaration_Node;
   for Generic_Procedure_Renaming_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Generic_Procedure_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Generic_Procedure_Renaming_Declaration_Ptr;

   function Specification
     (Element : Generic_Procedure_Renaming_Declaration_Node) return Asis.Element;

   procedure Set_Specification
     (Element : in out Generic_Procedure_Renaming_Declaration_Node;
      Value   : in     Asis.Element);

   function Declaration_Kind (Element : Generic_Procedure_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Generic_Procedure_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Generic_Procedure_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------------------------
   -- Generic_Function_Renaming_Declaration_Node --
   ------------------------------------------------

   type Generic_Function_Renaming_Declaration_Node is 
      new Generic_Procedure_Renaming_Declaration_Node with private;

   type Generic_Function_Renaming_Declaration_Ptr is
      access all Generic_Function_Renaming_Declaration_Node;
   for Generic_Function_Renaming_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Generic_Function_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Generic_Function_Renaming_Declaration_Ptr;

   function Declaration_Kind (Element : Generic_Function_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Generic_Function_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Generic_Function_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------------
   -- Incomplete_Type_Declaration_Node --
   --------------------------------------

   type Incomplete_Type_Declaration_Node is 
      new Declaration_Node with private;

   type Incomplete_Type_Declaration_Ptr is
      access all Incomplete_Type_Declaration_Node;
   for Incomplete_Type_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Incomplete_Type_Declaration_Node
     (The_Context : ASIS.Context)
      return Incomplete_Type_Declaration_Ptr;

   function Discriminant_Part
     (Element : Incomplete_Type_Declaration_Node) return Asis.Definition;

   procedure Set_Discriminant_Part
     (Element : in out Incomplete_Type_Declaration_Node;
      Value   : in     Asis.Definition);

   function Corresponding_Type_Declaration
     (Element : Incomplete_Type_Declaration_Node) return Asis.Definition;

   procedure Set_Corresponding_Type_Declaration
     (Element : in out Incomplete_Type_Declaration_Node;
      Value   : in     Asis.Definition);

   function Type_Declaration_View
     (Element : Incomplete_Type_Declaration_Node) return Asis.Definition;

   procedure Set_Type_Declaration_View
     (Element : in out Incomplete_Type_Declaration_Node;
      Value   : in     Asis.Definition);

   function Declaration_Kind (Element : Incomplete_Type_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Incomplete_Type_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Incomplete_Type_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Incomplete_Type_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------
   -- Subtype_Declaration_Node --
   ------------------------------

   type Subtype_Declaration_Node is 
      new Declaration_Node with private;

   type Subtype_Declaration_Ptr is
      access all Subtype_Declaration_Node;
   for Subtype_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Subtype_Declaration_Node
     (The_Context : ASIS.Context)
      return Subtype_Declaration_Ptr;

   function Type_Declaration_View
     (Element : Subtype_Declaration_Node) return Asis.Definition;

   procedure Set_Type_Declaration_View
     (Element : in out Subtype_Declaration_Node;
      Value   : in     Asis.Definition);

   function Corresponding_First_Subtype
     (Element : Subtype_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_First_Subtype
     (Element : in out Subtype_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Corresponding_Last_Constraint
     (Element : Subtype_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Last_Constraint
     (Element : in out Subtype_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Corresponding_Last_Subtype
     (Element : Subtype_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Last_Subtype
     (Element : in out Subtype_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Declaration_Kind (Element : Subtype_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Subtype_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Subtype_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Subtype_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------
   -- Component_Declaration_Node --
   --------------------------------

   type Component_Declaration_Node is 
      new Declaration_Node with private;

   type Component_Declaration_Ptr is
      access all Component_Declaration_Node;
   for Component_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Component_Declaration_Node
     (The_Context : ASIS.Context)
      return Component_Declaration_Ptr;

   function Object_Declaration_Subtype
     (Element : Component_Declaration_Node) return Asis.Definition;

   procedure Set_Object_Declaration_Subtype
     (Element : in out Component_Declaration_Node;
      Value   : in     Asis.Definition);

   function Initialization_Expression
     (Element : Component_Declaration_Node) return Asis.Expression;

   procedure Set_Initialization_Expression
     (Element : in out Component_Declaration_Node;
      Value   : in     Asis.Expression);

   function Declaration_Kind (Element : Component_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Component_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Component_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Component_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------
   -- Variable_Declaration_Node --
   -------------------------------

   type Variable_Declaration_Node is 
      new Component_Declaration_Node with private;

   type Variable_Declaration_Ptr is
      access all Variable_Declaration_Node;
   for Variable_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Variable_Declaration_Node
     (The_Context : ASIS.Context)
      return Variable_Declaration_Ptr;

   function Trait_Kind
     (Element : Variable_Declaration_Node) return Asis.Trait_Kinds;

   procedure Set_Trait_Kind
     (Element : in out Variable_Declaration_Node;
      Value   : in     Asis.Trait_Kinds);

   function Declaration_Kind (Element : Variable_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Variable_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Variable_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------
   -- Constant_Declaration_Node --
   -------------------------------

   type Constant_Declaration_Node is 
      new Variable_Declaration_Node with private;

   type Constant_Declaration_Ptr is
      access all Constant_Declaration_Node;
   for Constant_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Constant_Declaration_Node
     (The_Context : ASIS.Context)
      return Constant_Declaration_Ptr;

   function Declaration_Kind (Element : Constant_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Constant_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Constant_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------------
   -- Return_Object_Specification_Node --
   --------------------------------------

   type Return_Object_Specification_Node is 
      new Variable_Declaration_Node with private;

   type Return_Object_Specification_Ptr is
      access all Return_Object_Specification_Node;
   for Return_Object_Specification_Ptr'Storage_Pool use Lists.Pool;

   function New_Return_Object_Specification_Node
     (The_Context : ASIS.Context)
      return Return_Object_Specification_Ptr;

   function Declaration_Kind (Element : Return_Object_Specification_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Return_Object_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Return_Object_Specification_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------------
   -- Deferred_Constant_Declaration_Node --
   ----------------------------------------

   type Deferred_Constant_Declaration_Node is 
      new Declaration_Node with private;

   type Deferred_Constant_Declaration_Ptr is
      access all Deferred_Constant_Declaration_Node;
   for Deferred_Constant_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Deferred_Constant_Declaration_Node
     (The_Context : ASIS.Context)
      return Deferred_Constant_Declaration_Ptr;

   function Object_Declaration_Subtype
     (Element : Deferred_Constant_Declaration_Node) return Asis.Definition;

   procedure Set_Object_Declaration_Subtype
     (Element : in out Deferred_Constant_Declaration_Node;
      Value   : in     Asis.Definition);

   function Trait_Kind
     (Element : Deferred_Constant_Declaration_Node) return Asis.Trait_Kinds;

   procedure Set_Trait_Kind
     (Element : in out Deferred_Constant_Declaration_Node;
      Value   : in     Asis.Trait_Kinds);

   function Declaration_Kind (Element : Deferred_Constant_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Deferred_Constant_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Deferred_Constant_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Deferred_Constant_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------------------
   -- Single_Protected_Declaration_Node --
   ---------------------------------------

   type Single_Protected_Declaration_Node is 
      new Declaration_Node with private;

   type Single_Protected_Declaration_Ptr is
      access all Single_Protected_Declaration_Node;
   for Single_Protected_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Single_Protected_Declaration_Node
     (The_Context : ASIS.Context)
      return Single_Protected_Declaration_Ptr;

   function Progenitor_List
     (Element : Single_Protected_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Progenitor_List
     (Element : in out Single_Protected_Declaration_Node;
      Value   : in     Asis.Element);

   function Progenitor_List_List
     (Element : Single_Protected_Declaration_Node) return Asis.Element;

   function Object_Declaration_Subtype
     (Element : Single_Protected_Declaration_Node) return Asis.Definition;

   procedure Set_Object_Declaration_Subtype
     (Element : in out Single_Protected_Declaration_Node;
      Value   : in     Asis.Definition);

   function Is_Name_Repeated
     (Element : Single_Protected_Declaration_Node) return Boolean;

   procedure Set_Is_Name_Repeated
     (Element : in out Single_Protected_Declaration_Node;
      Value   : in     Boolean);

   function Corresponding_Body
     (Element : Single_Protected_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Body
     (Element : in out Single_Protected_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Declaration_Kind (Element : Single_Protected_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Single_Protected_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Single_Protected_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Single_Protected_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------
   -- Single_Task_Declaration_Node --
   ----------------------------------

   type Single_Task_Declaration_Node is 
      new Single_Protected_Declaration_Node with private;

   type Single_Task_Declaration_Ptr is
      access all Single_Task_Declaration_Node;
   for Single_Task_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Single_Task_Declaration_Node
     (The_Context : ASIS.Context)
      return Single_Task_Declaration_Ptr;

   function Declaration_Kind (Element : Single_Task_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Single_Task_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Single_Task_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------------
   -- Integer_Number_Declaration_Node --
   -------------------------------------

   type Integer_Number_Declaration_Node is 
      new Declaration_Node with private;

   type Integer_Number_Declaration_Ptr is
      access all Integer_Number_Declaration_Node;
   for Integer_Number_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Integer_Number_Declaration_Node
     (The_Context : ASIS.Context)
      return Integer_Number_Declaration_Ptr;

   function Initialization_Expression
     (Element : Integer_Number_Declaration_Node) return Asis.Expression;

   procedure Set_Initialization_Expression
     (Element : in out Integer_Number_Declaration_Node;
      Value   : in     Asis.Expression);

   function Declaration_Kind
     (Element : Integer_Number_Declaration_Node) return Asis.Declaration_Kinds;

   procedure Set_Declaration_Kind
     (Element : in out Integer_Number_Declaration_Node;
      Value   : in     Asis.Declaration_Kinds);

   function Children (Element : access Integer_Number_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Integer_Number_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Integer_Number_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------------------
   -- Enumeration_Literal_Specification_Node --
   --------------------------------------------

   type Enumeration_Literal_Specification_Node is 
      new Declaration_Node with private;

   type Enumeration_Literal_Specification_Ptr is
      access all Enumeration_Literal_Specification_Node;
   for Enumeration_Literal_Specification_Ptr'Storage_Pool use Lists.Pool;

   function New_Enumeration_Literal_Specification_Node
     (The_Context : ASIS.Context)
      return Enumeration_Literal_Specification_Ptr;

   function Declaration_Kind (Element : Enumeration_Literal_Specification_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Enumeration_Literal_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Enumeration_Literal_Specification_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------------
   -- Discriminant_Specification_Node --
   -------------------------------------

   type Discriminant_Specification_Node is 
      new Declaration_Node with private;

   type Discriminant_Specification_Ptr is
      access all Discriminant_Specification_Node;
   for Discriminant_Specification_Ptr'Storage_Pool use Lists.Pool;

   function New_Discriminant_Specification_Node
     (The_Context : ASIS.Context)
      return Discriminant_Specification_Ptr;

   function Object_Declaration_Subtype
     (Element : Discriminant_Specification_Node) return Asis.Definition;

   procedure Set_Object_Declaration_Subtype
     (Element : in out Discriminant_Specification_Node;
      Value   : in     Asis.Definition);

   function Initialization_Expression
     (Element : Discriminant_Specification_Node) return Asis.Expression;

   procedure Set_Initialization_Expression
     (Element : in out Discriminant_Specification_Node;
      Value   : in     Asis.Expression);

   function Trait_Kind
     (Element : Discriminant_Specification_Node) return Asis.Trait_Kinds;

   procedure Set_Trait_Kind
     (Element : in out Discriminant_Specification_Node;
      Value   : in     Asis.Trait_Kinds);

   function Has_Null_Exclusion
     (Element : Discriminant_Specification_Node) return Boolean;

   procedure Set_Has_Null_Exclusion
     (Element : in out Discriminant_Specification_Node;
      Value   : in     Boolean);

   function Declaration_Kind (Element : Discriminant_Specification_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Discriminant_Specification_Node)
     return Traverse_List;

   function Clone
     (Element : Discriminant_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Discriminant_Specification_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------------
   -- Entry_Index_Specification_Node --
   ------------------------------------

   type Entry_Index_Specification_Node is 
      new Declaration_Node with private;

   type Entry_Index_Specification_Ptr is
      access all Entry_Index_Specification_Node;
   for Entry_Index_Specification_Ptr'Storage_Pool use Lists.Pool;

   function New_Entry_Index_Specification_Node
     (The_Context : ASIS.Context)
      return Entry_Index_Specification_Ptr;

   function Specification_Subtype_Definition
     (Element : Entry_Index_Specification_Node) return Asis.Discrete_Subtype_Definition;

   procedure Set_Specification_Subtype_Definition
     (Element : in out Entry_Index_Specification_Node;
      Value   : in     Asis.Discrete_Subtype_Definition);

   function Declaration_Kind (Element : Entry_Index_Specification_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Entry_Index_Specification_Node)
     return Traverse_List;

   function Clone
     (Element : Entry_Index_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Entry_Index_Specification_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------------------
   -- Loop_Parameter_Specification_Node --
   ---------------------------------------

   type Loop_Parameter_Specification_Node is 
      new Entry_Index_Specification_Node with private;

   type Loop_Parameter_Specification_Ptr is
      access all Loop_Parameter_Specification_Node;
   for Loop_Parameter_Specification_Ptr'Storage_Pool use Lists.Pool;

   function New_Loop_Parameter_Specification_Node
     (The_Context : ASIS.Context)
      return Loop_Parameter_Specification_Ptr;

   function Trait_Kind
     (Element : Loop_Parameter_Specification_Node) return Asis.Trait_Kinds;

   procedure Set_Trait_Kind
     (Element : in out Loop_Parameter_Specification_Node;
      Value   : in     Asis.Trait_Kinds);

   function Declaration_Kind (Element : Loop_Parameter_Specification_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Loop_Parameter_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Loop_Parameter_Specification_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------------
   -- Formal_Object_Declaration_Node --
   ------------------------------------

   type Formal_Object_Declaration_Node is 
      new Declaration_Node with private;

   type Formal_Object_Declaration_Ptr is
      access all Formal_Object_Declaration_Node;
   for Formal_Object_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Object_Declaration_Node
     (The_Context : ASIS.Context)
      return Formal_Object_Declaration_Ptr;

   function Mode_Kind
     (Element : Formal_Object_Declaration_Node) return Asis.Mode_Kinds;

   procedure Set_Mode_Kind
     (Element : in out Formal_Object_Declaration_Node;
      Value   : in     Asis.Mode_Kinds);

   function Object_Declaration_Subtype
     (Element : Formal_Object_Declaration_Node) return Asis.Definition;

   procedure Set_Object_Declaration_Subtype
     (Element : in out Formal_Object_Declaration_Node;
      Value   : in     Asis.Definition);

   function Initialization_Expression
     (Element : Formal_Object_Declaration_Node) return Asis.Expression;

   procedure Set_Initialization_Expression
     (Element : in out Formal_Object_Declaration_Node;
      Value   : in     Asis.Expression);

   function Has_Null_Exclusion
     (Element : Formal_Object_Declaration_Node) return Boolean;

   procedure Set_Has_Null_Exclusion
     (Element : in out Formal_Object_Declaration_Node;
      Value   : in     Boolean);

   function Generic_Actual
     (Element : Formal_Object_Declaration_Node) return Asis.Expression;

   procedure Set_Generic_Actual
     (Element : in out Formal_Object_Declaration_Node;
      Value   : in     Asis.Expression);

   function Declaration_Kind (Element : Formal_Object_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Formal_Object_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Formal_Object_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Object_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------
   -- Parameter_Specification_Node --
   ----------------------------------

   type Parameter_Specification_Node is 
      new Formal_Object_Declaration_Node with private;

   type Parameter_Specification_Ptr is
      access all Parameter_Specification_Node;
   for Parameter_Specification_Ptr'Storage_Pool use Lists.Pool;

   function New_Parameter_Specification_Node
     (The_Context : ASIS.Context)
      return Parameter_Specification_Ptr;

   function Trait_Kind
     (Element : Parameter_Specification_Node) return Asis.Trait_Kinds;

   procedure Set_Trait_Kind
     (Element : in out Parameter_Specification_Node;
      Value   : in     Asis.Trait_Kinds);

   function Declaration_Kind (Element : Parameter_Specification_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Parameter_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Parameter_Specification_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------
   -- Package_Declaration_Node --
   ------------------------------

   type Package_Declaration_Node is 
      new Declaration_Node with private;

   type Package_Declaration_Ptr is
      access all Package_Declaration_Node;
   for Package_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Package_Declaration_Node
     (The_Context : ASIS.Context)
      return Package_Declaration_Ptr;

   function Is_Name_Repeated
     (Element : Package_Declaration_Node) return Boolean;

   procedure Set_Is_Name_Repeated
     (Element : in out Package_Declaration_Node;
      Value   : in     Boolean);

   function Corresponding_Declaration
     (Element : Package_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Declaration
     (Element : in out Package_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Corresponding_Body
     (Element : Package_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Body
     (Element : in out Package_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Is_Private_Present
     (Element : Package_Declaration_Node) return Boolean;

   procedure Set_Is_Private_Present
     (Element : in out Package_Declaration_Node;
      Value   : in     Boolean);

   function Generic_Formal_Part
     (Element : Package_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Generic_Formal_Part
     (Element : in out Package_Declaration_Node;
      Value   : in     Asis.Element);

   function Generic_Formal_Part_List
     (Element : Package_Declaration_Node) return Asis.Element;

   function Visible_Part_Declarative_Items
     (Element : Package_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Visible_Part_Declarative_Items
     (Element : in out Package_Declaration_Node;
      Value   : in     Asis.Element);

   function Visible_Part_Declarative_Items_List
     (Element : Package_Declaration_Node) return Asis.Element;

   function Private_Part_Declarative_Items
     (Element : Package_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Private_Part_Declarative_Items
     (Element : in out Package_Declaration_Node;
      Value   : in     Asis.Element);

   function Private_Part_Declarative_Items_List
     (Element : Package_Declaration_Node) return Asis.Element;

   function Package_Specification
     (Element : Package_Declaration_Node) return Asis.Element;

   procedure Set_Package_Specification
     (Element : in out Package_Declaration_Node;
      Value   : in     Asis.Element);

   function Declaration_Kind (Element : Package_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Package_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Package_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Package_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------------
   -- Protected_Body_Declaration_Node --
   -------------------------------------

   type Protected_Body_Declaration_Node is 
      new Declaration_Node with private;

   type Protected_Body_Declaration_Ptr is
      access all Protected_Body_Declaration_Node;
   for Protected_Body_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Protected_Body_Declaration_Node
     (The_Context : ASIS.Context)
      return Protected_Body_Declaration_Ptr;

   function Is_Name_Repeated
     (Element : Protected_Body_Declaration_Node) return Boolean;

   procedure Set_Is_Name_Repeated
     (Element : in out Protected_Body_Declaration_Node;
      Value   : in     Boolean);

   function Corresponding_Declaration
     (Element : Protected_Body_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Declaration
     (Element : in out Protected_Body_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Protected_Operation_Items
     (Element : Protected_Body_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Protected_Operation_Items
     (Element : in out Protected_Body_Declaration_Node;
      Value   : in     Asis.Element);

   function Protected_Operation_Items_List
     (Element : Protected_Body_Declaration_Node) return Asis.Element;

   function Corresponding_Body_Stub
     (Element : Protected_Body_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Body_Stub
     (Element : in out Protected_Body_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Get_Identifier
     (Element : Protected_Body_Declaration_Node) return Asis.Element;

   procedure Set_Identifier
     (Element : in out Protected_Body_Declaration_Node;
      Value   : in     Asis.Element);

   function Declaration_Kind (Element : Protected_Body_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Protected_Body_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Protected_Body_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Protected_Body_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------
   -- Package_Body_Stub_Node --
   ----------------------------

   type Package_Body_Stub_Node is 
      new Declaration_Node with private;

   type Package_Body_Stub_Ptr is
      access all Package_Body_Stub_Node;
   for Package_Body_Stub_Ptr'Storage_Pool use Lists.Pool;

   function New_Package_Body_Stub_Node
     (The_Context : ASIS.Context)
      return Package_Body_Stub_Ptr;

   function Corresponding_Subunit
     (Element : Package_Body_Stub_Node) return Asis.Declaration;

   procedure Set_Corresponding_Subunit
     (Element : in out Package_Body_Stub_Node;
      Value   : in     Asis.Declaration);

   function Corresponding_Declaration
     (Element : Package_Body_Stub_Node) return Asis.Declaration;

   procedure Set_Corresponding_Declaration
     (Element : in out Package_Body_Stub_Node;
      Value   : in     Asis.Declaration);

   function Declaration_Kind (Element : Package_Body_Stub_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Package_Body_Stub_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Package_Body_Stub_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------
   -- Task_Body_Stub_Node --
   -------------------------

   type Task_Body_Stub_Node is 
      new Package_Body_Stub_Node with private;

   type Task_Body_Stub_Ptr is
      access all Task_Body_Stub_Node;
   for Task_Body_Stub_Ptr'Storage_Pool use Lists.Pool;

   function New_Task_Body_Stub_Node
     (The_Context : ASIS.Context)
      return Task_Body_Stub_Ptr;

   function Declaration_Kind (Element : Task_Body_Stub_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Task_Body_Stub_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Task_Body_Stub_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------
   -- Protected_Body_Stub_Node --
   ------------------------------

   type Protected_Body_Stub_Node is 
      new Package_Body_Stub_Node with private;

   type Protected_Body_Stub_Ptr is
      access all Protected_Body_Stub_Node;
   for Protected_Body_Stub_Ptr'Storage_Pool use Lists.Pool;

   function New_Protected_Body_Stub_Node
     (The_Context : ASIS.Context)
      return Protected_Body_Stub_Ptr;

   function Declaration_Kind (Element : Protected_Body_Stub_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Protected_Body_Stub_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Protected_Body_Stub_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------
   -- Exception_Declaration_Node --
   --------------------------------

   type Exception_Declaration_Node is 
      new Declaration_Node with private;

   type Exception_Declaration_Ptr is
      access all Exception_Declaration_Node;
   for Exception_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Exception_Declaration_Node
     (The_Context : ASIS.Context)
      return Exception_Declaration_Ptr;

   function Declaration_Kind (Element : Exception_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Exception_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Exception_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------------------
   -- Choice_Parameter_Specification_Node --
   -----------------------------------------

   type Choice_Parameter_Specification_Node is 
      new Declaration_Node with private;

   type Choice_Parameter_Specification_Ptr is
      access all Choice_Parameter_Specification_Node;
   for Choice_Parameter_Specification_Ptr'Storage_Pool use Lists.Pool;

   function New_Choice_Parameter_Specification_Node
     (The_Context : ASIS.Context)
      return Choice_Parameter_Specification_Ptr;

   function Declaration_Kind (Element : Choice_Parameter_Specification_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Choice_Parameter_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Choice_Parameter_Specification_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------------
   -- Generic_Package_Declaration_Node --
   --------------------------------------

   type Generic_Package_Declaration_Node is 
      new Declaration_Node with private;

   type Generic_Package_Declaration_Ptr is
      access all Generic_Package_Declaration_Node;
   for Generic_Package_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Generic_Package_Declaration_Node
     (The_Context : ASIS.Context)
      return Generic_Package_Declaration_Ptr;

   function Is_Name_Repeated
     (Element : Generic_Package_Declaration_Node) return Boolean;

   procedure Set_Is_Name_Repeated
     (Element : in out Generic_Package_Declaration_Node;
      Value   : in     Boolean);

   function Corresponding_Body
     (Element : Generic_Package_Declaration_Node) return Asis.Declaration;

   procedure Set_Corresponding_Body
     (Element : in out Generic_Package_Declaration_Node;
      Value   : in     Asis.Declaration);

   function Is_Private_Present
     (Element : Generic_Package_Declaration_Node) return Boolean;

   procedure Set_Is_Private_Present
     (Element : in out Generic_Package_Declaration_Node;
      Value   : in     Boolean);

   function Visible_Part_Declarative_Items
     (Element : Generic_Package_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Visible_Part_Declarative_Items
     (Element : in out Generic_Package_Declaration_Node;
      Value   : in     Asis.Element);

   function Visible_Part_Declarative_Items_List
     (Element : Generic_Package_Declaration_Node) return Asis.Element;

   function Private_Part_Declarative_Items
     (Element : Generic_Package_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Private_Part_Declarative_Items
     (Element : in out Generic_Package_Declaration_Node;
      Value   : in     Asis.Element);

   function Private_Part_Declarative_Items_List
     (Element : Generic_Package_Declaration_Node) return Asis.Element;

   function Generic_Formal_Part
     (Element : Generic_Package_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Generic_Formal_Part
     (Element : in out Generic_Package_Declaration_Node;
      Value   : in     Asis.Element);

   function Generic_Formal_Part_List
     (Element : Generic_Package_Declaration_Node) return Asis.Element;

   function Specification
     (Element : Generic_Package_Declaration_Node) return Asis.Element;

   procedure Set_Specification
     (Element : in out Generic_Package_Declaration_Node;
      Value   : in     Asis.Element);

   function Declaration_Kind (Element : Generic_Package_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Generic_Package_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Generic_Package_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Generic_Package_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------------------
   -- Formal_Package_Declaration_With_Box_Node --
   ----------------------------------------------

   type Formal_Package_Declaration_With_Box_Node is 
      new Declaration_Node with private;

   type Formal_Package_Declaration_With_Box_Ptr is
      access all Formal_Package_Declaration_With_Box_Node;
   for Formal_Package_Declaration_With_Box_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Package_Declaration_With_Box_Node
     (The_Context : ASIS.Context)
      return Formal_Package_Declaration_With_Box_Ptr;

   function Corresponding_Declaration
     (Element : Formal_Package_Declaration_With_Box_Node) return Asis.Declaration;

   procedure Set_Corresponding_Declaration
     (Element : in out Formal_Package_Declaration_With_Box_Node;
      Value   : in     Asis.Declaration);

   function Corresponding_Body
     (Element : Formal_Package_Declaration_With_Box_Node) return Asis.Declaration;

   procedure Set_Corresponding_Body
     (Element : in out Formal_Package_Declaration_With_Box_Node;
      Value   : in     Asis.Declaration);

   function Generic_Unit_Name
     (Element : Formal_Package_Declaration_With_Box_Node) return Asis.Expression;

   procedure Set_Generic_Unit_Name
     (Element : in out Formal_Package_Declaration_With_Box_Node;
      Value   : in     Asis.Expression);

   function Normalized_Generic_Actual_Part
     (Element : Formal_Package_Declaration_With_Box_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Normalized_Generic_Actual_Part
     (Element : in out Formal_Package_Declaration_With_Box_Node;
      Item    : in     Asis.Element);

   function Generic_Actual
     (Element : Formal_Package_Declaration_With_Box_Node) return Asis.Expression;

   procedure Set_Generic_Actual
     (Element : in out Formal_Package_Declaration_With_Box_Node;
      Value   : in     Asis.Expression);

   function Declaration_Kind (Element : Formal_Package_Declaration_With_Box_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Formal_Package_Declaration_With_Box_Node)
     return Traverse_List;

   function Clone
     (Element : Formal_Package_Declaration_With_Box_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Package_Declaration_With_Box_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------
   -- Package_Instantiation_Node --
   --------------------------------

   type Package_Instantiation_Node is 
      new Formal_Package_Declaration_With_Box_Node with private;

   type Package_Instantiation_Ptr is
      access all Package_Instantiation_Node;
   for Package_Instantiation_Ptr'Storage_Pool use Lists.Pool;

   function New_Package_Instantiation_Node
     (The_Context : ASIS.Context)
      return Package_Instantiation_Ptr;

   function Generic_Actual_Part
     (Element : Package_Instantiation_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Generic_Actual_Part
     (Element : in out Package_Instantiation_Node;
      Value   : in     Asis.Element);

   function Generic_Actual_Part_List
     (Element : Package_Instantiation_Node) return Asis.Element;

   function Declaration_Kind (Element : Package_Instantiation_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Package_Instantiation_Node)
     return Traverse_List;

   function Clone
     (Element : Package_Instantiation_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Package_Instantiation_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------
   -- Procedure_Instantiation_Node --
   ----------------------------------

   type Procedure_Instantiation_Node is 
      new Package_Instantiation_Node with private;

   type Procedure_Instantiation_Ptr is
      access all Procedure_Instantiation_Node;
   for Procedure_Instantiation_Ptr'Storage_Pool use Lists.Pool;

   function New_Procedure_Instantiation_Node
     (The_Context : ASIS.Context)
      return Procedure_Instantiation_Ptr;

   function Specification
     (Element : Procedure_Instantiation_Node) return Asis.Element;

   procedure Set_Specification
     (Element : in out Procedure_Instantiation_Node;
      Value   : in     Asis.Element);

   function Overriding_Indicator_Kind
     (Element : Procedure_Instantiation_Node) return Asis.Overriding_Indicator_Kinds;

   procedure Set_Overriding_Indicator_Kind
     (Element : in out Procedure_Instantiation_Node;
      Value   : in     Asis.Overriding_Indicator_Kinds);

   function Declaration_Kind (Element : Procedure_Instantiation_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Procedure_Instantiation_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Procedure_Instantiation_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------------
   -- Function_Instantiation_Node --
   ---------------------------------

   type Function_Instantiation_Node is 
      new Procedure_Instantiation_Node with private;

   type Function_Instantiation_Ptr is
      access all Function_Instantiation_Node;
   for Function_Instantiation_Ptr'Storage_Pool use Lists.Pool;

   function New_Function_Instantiation_Node
     (The_Context : ASIS.Context)
      return Function_Instantiation_Ptr;

   function Declaration_Kind (Element : Function_Instantiation_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Function_Instantiation_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Function_Instantiation_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------------
   -- Formal_Package_Declaration_Node --
   -------------------------------------

   type Formal_Package_Declaration_Node is 
      new Package_Instantiation_Node with private;

   type Formal_Package_Declaration_Ptr is
      access all Formal_Package_Declaration_Node;
   for Formal_Package_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Package_Declaration_Node
     (The_Context : ASIS.Context)
      return Formal_Package_Declaration_Ptr;

   function Declaration_Kind (Element : Formal_Package_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Clone
     (Element : Formal_Package_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Package_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------------------
   -- Formal_Procedure_Declaration_Node --
   ---------------------------------------

   type Formal_Procedure_Declaration_Node is 
      new Declaration_Node with private;

   type Formal_Procedure_Declaration_Ptr is
      access all Formal_Procedure_Declaration_Node;
   for Formal_Procedure_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Procedure_Declaration_Node
     (The_Context : ASIS.Context)
      return Formal_Procedure_Declaration_Ptr;

   function Parameter_Profile
     (Element : Formal_Procedure_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Parameter_Profile
     (Element : in out Formal_Procedure_Declaration_Node;
      Value   : in     Asis.Element);

   function Parameter_Profile_List
     (Element : Formal_Procedure_Declaration_Node) return Asis.Element;

   function Default_Kind
     (Element : Formal_Procedure_Declaration_Node) return Asis.Subprogram_Default_Kinds;

   procedure Set_Default_Kind
     (Element : in out Formal_Procedure_Declaration_Node;
      Value   : in     Asis.Subprogram_Default_Kinds);

   function Formal_Subprogram_Default
     (Element : Formal_Procedure_Declaration_Node) return Asis.Expression;

   procedure Set_Formal_Subprogram_Default
     (Element : in out Formal_Procedure_Declaration_Node;
      Value   : in     Asis.Expression);

   function Specification
     (Element : Formal_Procedure_Declaration_Node) return Asis.Element;

   procedure Set_Specification
     (Element : in out Formal_Procedure_Declaration_Node;
      Value   : in     Asis.Element);

   function Has_Abstract
     (Element : Formal_Procedure_Declaration_Node) return Boolean;

   procedure Set_Has_Abstract
     (Element : in out Formal_Procedure_Declaration_Node;
      Value   : in     Boolean);

   function Generic_Actual
     (Element : Formal_Procedure_Declaration_Node) return Asis.Expression;

   procedure Set_Generic_Actual
     (Element : in out Formal_Procedure_Declaration_Node;
      Value   : in     Asis.Expression);

   function Declaration_Kind (Element : Formal_Procedure_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Formal_Procedure_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Formal_Procedure_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Procedure_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------------
   -- Formal_Function_Declaration_Node --
   --------------------------------------

   type Formal_Function_Declaration_Node is 
      new Formal_Procedure_Declaration_Node with private;

   type Formal_Function_Declaration_Ptr is
      access all Formal_Function_Declaration_Node;
   for Formal_Function_Declaration_Ptr'Storage_Pool use Lists.Pool;

   function New_Formal_Function_Declaration_Node
     (The_Context : ASIS.Context)
      return Formal_Function_Declaration_Ptr;

   function Result_Subtype
     (Element : Formal_Function_Declaration_Node) return Asis.Definition;

   procedure Set_Result_Subtype
     (Element : in out Formal_Function_Declaration_Node;
      Value   : in     Asis.Definition);

   function Declaration_Kind (Element : Formal_Function_Declaration_Node)
      return Asis.Declaration_Kinds;

   function Children (Element : access Formal_Function_Declaration_Node)
     return Traverse_List;

   function Clone
     (Element : Formal_Function_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Function_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

private


   type Base_Type_Declaration_Node is abstract
      new Declaration_Node with
      record
         Discriminant_Part              : aliased Asis.Definition;
         Type_Declaration_View          : aliased Asis.Definition;
      end record;


   type Ordinary_Type_Declaration_Node is 
      new Base_Type_Declaration_Node with
      record
         Corresponding_Type_Declaration : aliased Asis.Declaration;
      end record;


   type Protected_Type_Declaration_Node is 
      new Ordinary_Type_Declaration_Node with
      record
         Is_Name_Repeated               : aliased Boolean := False;
         Corresponding_Body             : aliased Asis.Declaration;
         Progenitor_List                : aliased Primary_Expression_Lists.List;
      end record;


   type Task_Type_Declaration_Node is 
      new Protected_Type_Declaration_Node with
      record
         null;
      end record;


   type Private_Type_Declaration_Node is 
      new Ordinary_Type_Declaration_Node with
      record
         Trait_Kind                     : aliased Asis.Trait_Kinds := An_Ordinary_Trait;
      end record;


   type Private_Extension_Declaration_Node is 
      new Private_Type_Declaration_Node with
      record
         Progenitor_List                : aliased Primary_Expression_Lists.List;
      end record;


   type Formal_Type_Declaration_Node is 
      new Base_Type_Declaration_Node with
      record
         Generic_Actual                 : aliased Asis.Expression;
      end record;


   type Base_Callable_Declaration_Node is abstract
      new Declaration_Node with
      record
         Parameter_Profile              : aliased Primary_Parameter_Lists.List;
         Corresponding_Body             : aliased Asis.Declaration;
         Specification                  : aliased Asis.Element;
      end record;


   type Procedure_Declaration_Node is 
      new Base_Callable_Declaration_Node with
      record
         Corresponding_Subprogram_Derivation : aliased Asis.Declaration;
         Corresponding_Type             : aliased Asis.Type_Definition;
         Is_Dispatching_Operation       : aliased Boolean := False;
         Trait_Kind                     : aliased Asis.Trait_Kinds := An_Ordinary_Trait;
         Overriding_Indicator_Kind      : aliased Asis.Overriding_Indicator_Kinds := No_Overriding_Indicator;
         Has_Abstract                   : aliased Boolean := False;
         Is_Null_Procedure              : aliased Boolean := False;
         Generic_Formal_Part            : aliased Primary_Declaration_Lists.List;
      end record;


   type Function_Declaration_Node is 
      new Procedure_Declaration_Node with
      record
         Result_Subtype                 : aliased Asis.Definition;
         Corresponding_Equality_Operator : aliased Asis.Declaration;
      end record;


   type Procedure_Renaming_Declaration_Node is 
      new Base_Callable_Declaration_Node with
      record
         Is_Dispatching_Operation       : aliased Boolean := False;
         Renamed_Entity                 : aliased Asis.Expression;
         Corresponding_Base_Entity      : aliased Asis.Expression;
         Corresponding_Declaration      : aliased Asis.Declaration;
         Overriding_Indicator_Kind      : aliased Asis.Overriding_Indicator_Kinds := No_Overriding_Indicator;
      end record;


   type Function_Renaming_Declaration_Node is 
      new Procedure_Renaming_Declaration_Node with
      record
         Result_Subtype                 : aliased Asis.Definition;
         Corresponding_Equality_Operator : aliased Asis.Declaration;
      end record;


   type Entry_Declaration_Node is 
      new Base_Callable_Declaration_Node with
      record
         Entry_Family_Definition        : aliased Asis.Discrete_Subtype_Definition;
         Overriding_Indicator_Kind      : aliased Asis.Overriding_Indicator_Kinds := No_Overriding_Indicator;
      end record;


   type Procedure_Body_Stub_Node is 
      new Base_Callable_Declaration_Node with
      record
         Corresponding_Subunit          : aliased Asis.Declaration;
         Corresponding_Declaration      : aliased Asis.Declaration;
         Overriding_Indicator_Kind      : aliased Asis.Overriding_Indicator_Kinds := No_Overriding_Indicator;
      end record;


   type Function_Body_Stub_Node is 
      new Procedure_Body_Stub_Node with
      record
         Result_Subtype                 : aliased Asis.Definition;
      end record;


   type Generic_Procedure_Declaration_Node is 
      new Base_Callable_Declaration_Node with
      record
         Generic_Formal_Part            : aliased Primary_Declaration_Lists.List;
      end record;


   type Generic_Function_Declaration_Node is 
      new Generic_Procedure_Declaration_Node with
      record
         Result_Subtype                 : aliased Asis.Definition;
      end record;


   type Base_Body_Declaration_Node is abstract
      new Declaration_Node with
      record
         Body_Declarative_Items         : aliased Primary_Declaration_Lists.List;
         Body_Statements                : aliased Primary_Statement_Lists.List;
         Body_Exception_Handlers        : aliased Primary_Handler_Lists.List;
         Corresponding_Declaration      : aliased Asis.Declaration;
         Corresponding_Body_Stub        : aliased Asis.Declaration;
         Is_Name_Repeated               : aliased Boolean := False;
         Handled_Statements             : aliased Asis.Element;
         Compound_Name                  : aliased Asis.Element;
      end record;


   type Procedure_Body_Declaration_Node is 
      new Base_Body_Declaration_Node with
      record
         Parameter_Profile              : aliased Primary_Parameter_Lists.List;
         Specification                  : aliased Asis.Element;
         Overriding_Indicator_Kind      : aliased Asis.Overriding_Indicator_Kinds := No_Overriding_Indicator;
      end record;


   type Function_Body_Declaration_Node is 
      new Procedure_Body_Declaration_Node with
      record
         Result_Subtype                 : aliased Asis.Definition;
      end record;


   type Package_Body_Declaration_Node is 
      new Base_Body_Declaration_Node with
      record
         null;
      end record;


   type Task_Body_Declaration_Node is 
      new Package_Body_Declaration_Node with
      record
         null;
      end record;


   type Entry_Body_Declaration_Node is 
      new Base_Body_Declaration_Node with
      record
         Parameter_Profile              : aliased Primary_Parameter_Lists.List;
         Entry_Index_Specification      : aliased Asis.Declaration;
         Entry_Barrier                  : aliased Asis.Expression;
         Specification                  : aliased Asis.Element;
      end record;


   type Base_Renaming_Declaration_Node is abstract
      new Declaration_Node with
      record
         Renamed_Entity                 : aliased Asis.Expression;
         Corresponding_Base_Entity      : aliased Asis.Expression;
      end record;


   type Object_Renaming_Declaration_Node is 
      new Base_Renaming_Declaration_Node with
      record
         Object_Declaration_Subtype     : aliased Asis.Definition;
         Has_Null_Exclusion             : aliased Boolean := False;
      end record;


   type Exception_Renaming_Declaration_Node is 
      new Base_Renaming_Declaration_Node with
      record
         null;
      end record;


   type Package_Renaming_Declaration_Node is 
      new Base_Renaming_Declaration_Node with
      record
         null;
      end record;


   type Generic_Package_Renaming_Declaration_Node is 
      new Package_Renaming_Declaration_Node with
      record
         Empty_Generic_Part             : aliased Primary_Declaration_Lists.List;
      end record;


   type Generic_Procedure_Renaming_Declaration_Node is 
      new Generic_Package_Renaming_Declaration_Node with
      record
         Specification                  : aliased Asis.Element;
      end record;


   type Generic_Function_Renaming_Declaration_Node is 
      new Generic_Procedure_Renaming_Declaration_Node with
      record
         null;
      end record;


   type Incomplete_Type_Declaration_Node is 
      new Declaration_Node with
      record
         Discriminant_Part              : aliased Asis.Definition;
         Corresponding_Type_Declaration : aliased Asis.Definition;
         Type_Declaration_View          : aliased Asis.Definition;
      end record;


   type Subtype_Declaration_Node is 
      new Declaration_Node with
      record
         Type_Declaration_View          : aliased Asis.Definition;
         Corresponding_First_Subtype    : aliased Asis.Declaration;
         Corresponding_Last_Constraint  : aliased Asis.Declaration;
         Corresponding_Last_Subtype     : aliased Asis.Declaration;
      end record;


   type Component_Declaration_Node is 
      new Declaration_Node with
      record
         Object_Declaration_Subtype     : aliased Asis.Definition;
         Initialization_Expression      : aliased Asis.Expression;
      end record;


   type Variable_Declaration_Node is 
      new Component_Declaration_Node with
      record
         Trait_Kind                     : aliased Asis.Trait_Kinds := An_Ordinary_Trait;
      end record;


   type Constant_Declaration_Node is 
      new Variable_Declaration_Node with
      record
         null;
      end record;


   type Return_Object_Specification_Node is 
      new Variable_Declaration_Node with
      record
         null;
      end record;


   type Deferred_Constant_Declaration_Node is 
      new Declaration_Node with
      record
         Object_Declaration_Subtype     : aliased Asis.Definition;
         Trait_Kind                     : aliased Asis.Trait_Kinds := An_Ordinary_Trait;
      end record;


   type Single_Protected_Declaration_Node is 
      new Declaration_Node with
      record
         Progenitor_List                : aliased Primary_Expression_Lists.List;
         Object_Declaration_Subtype     : aliased Asis.Definition;
         Is_Name_Repeated               : aliased Boolean := False;
         Corresponding_Body             : aliased Asis.Declaration;
      end record;


   type Single_Task_Declaration_Node is 
      new Single_Protected_Declaration_Node with
      record
         null;
      end record;


   type Integer_Number_Declaration_Node is 
      new Declaration_Node with
      record
         Initialization_Expression      : aliased Asis.Expression;
         Declaration_Kind               : aliased Asis.Declaration_Kinds
           := An_Integer_Number_Declaration;
      end record;


   type Enumeration_Literal_Specification_Node is 
      new Declaration_Node with
      record
         null;
      end record;


   type Discriminant_Specification_Node is 
      new Declaration_Node with
      record
         Object_Declaration_Subtype     : aliased Asis.Definition;
         Initialization_Expression      : aliased Asis.Expression;
         Trait_Kind                     : aliased Asis.Trait_Kinds := An_Ordinary_Trait;
         Has_Null_Exclusion             : aliased Boolean := False;
      end record;


   type Entry_Index_Specification_Node is 
      new Declaration_Node with
      record
         Specification_Subtype_Definition : aliased Asis.Discrete_Subtype_Definition;
      end record;


   type Loop_Parameter_Specification_Node is 
      new Entry_Index_Specification_Node with
      record
         Trait_Kind                     : aliased Asis.Trait_Kinds := An_Ordinary_Trait;
      end record;


   type Formal_Object_Declaration_Node is 
      new Declaration_Node with
      record
         Mode_Kind                      : aliased Asis.Mode_Kinds := A_Default_In_Mode;
         Object_Declaration_Subtype     : aliased Asis.Definition;
         Initialization_Expression      : aliased Asis.Expression;
         Has_Null_Exclusion             : aliased Boolean := False;
         Generic_Actual                 : aliased Asis.Expression;
      end record;


   type Parameter_Specification_Node is 
      new Formal_Object_Declaration_Node with
      record
         Trait_Kind                     : aliased Asis.Trait_Kinds := An_Ordinary_Trait;
      end record;


   type Package_Declaration_Node is 
      new Declaration_Node with
      record
         Is_Name_Repeated               : aliased Boolean := False;
         Corresponding_Declaration      : aliased Asis.Declaration;
         Corresponding_Body             : aliased Asis.Declaration;
         Is_Private_Present             : aliased Boolean := False;
         Generic_Formal_Part            : aliased Primary_Declaration_Lists.List;
         Visible_Part_Declarative_Items : aliased Primary_Declaration_Lists.List;
         Private_Part_Declarative_Items : aliased Primary_Declaration_Lists.List;
         Package_Specification          : aliased Asis.Element;
      end record;


   type Protected_Body_Declaration_Node is 
      new Declaration_Node with
      record
         Is_Name_Repeated               : aliased Boolean := False;
         Corresponding_Declaration      : aliased Asis.Declaration;
         Protected_Operation_Items      : aliased Primary_Declaration_Lists.List;
         Corresponding_Body_Stub        : aliased Asis.Declaration;
         Identifier                     : aliased Asis.Element;
      end record;


   type Package_Body_Stub_Node is 
      new Declaration_Node with
      record
         Corresponding_Subunit          : aliased Asis.Declaration;
         Corresponding_Declaration      : aliased Asis.Declaration;
      end record;


   type Task_Body_Stub_Node is 
      new Package_Body_Stub_Node with
      record
         null;
      end record;


   type Protected_Body_Stub_Node is 
      new Package_Body_Stub_Node with
      record
         null;
      end record;


   type Exception_Declaration_Node is 
      new Declaration_Node with
      record
         null;
      end record;


   type Choice_Parameter_Specification_Node is 
      new Declaration_Node with
      record
         null;
      end record;


   type Generic_Package_Declaration_Node is 
      new Declaration_Node with
      record
         Is_Name_Repeated               : aliased Boolean := False;
         Corresponding_Body             : aliased Asis.Declaration;
         Is_Private_Present             : aliased Boolean := False;
         Visible_Part_Declarative_Items : aliased Primary_Declaration_Lists.List;
         Private_Part_Declarative_Items : aliased Primary_Declaration_Lists.List;
         Generic_Formal_Part            : aliased Primary_Declaration_Lists.List;
         Specification                  : aliased Asis.Element;
      end record;


   type Formal_Package_Declaration_With_Box_Node is 
      new Declaration_Node with
      record
         Corresponding_Declaration      : aliased Asis.Declaration;
         Corresponding_Body             : aliased Asis.Declaration;
         Generic_Unit_Name              : aliased Asis.Expression;
         Normalized_Generic_Actual_Part : aliased Secondary_Association_Lists.List_Node;
         Generic_Actual                 : aliased Asis.Expression;
      end record;


   type Package_Instantiation_Node is 
      new Formal_Package_Declaration_With_Box_Node with
      record
         Generic_Actual_Part            : aliased Primary_Association_Lists.List;
      end record;


   type Procedure_Instantiation_Node is 
      new Package_Instantiation_Node with
      record
         Specification                  : aliased Asis.Element;
         Overriding_Indicator_Kind      : aliased Asis.Overriding_Indicator_Kinds := No_Overriding_Indicator;
      end record;


   type Function_Instantiation_Node is 
      new Procedure_Instantiation_Node with
      record
         null;
      end record;


   type Formal_Package_Declaration_Node is 
      new Package_Instantiation_Node with
      record
         null;
      end record;


   type Formal_Procedure_Declaration_Node is 
      new Declaration_Node with
      record
         Parameter_Profile              : aliased Primary_Parameter_Lists.List;
         Default_Kind                   : aliased Asis.Subprogram_Default_Kinds := Not_A_Default;
         Formal_Subprogram_Default      : aliased Asis.Expression;
         Specification                  : aliased Asis.Element;
         Has_Abstract                   : aliased Boolean := False;
         Generic_Actual                 : aliased Asis.Expression;
      end record;


   type Formal_Function_Declaration_Node is 
      new Formal_Procedure_Declaration_Node with
      record
         Result_Subtype                 : aliased Asis.Definition;
      end record;

end Asis.Gela.Elements.Decl;
