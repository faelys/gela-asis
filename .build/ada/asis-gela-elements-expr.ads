
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
  
package Asis.Gela.Elements.Expr is

   -------------------------
   -- Box_Expression_Node --
   -------------------------

   type Box_Expression_Node is 
      new Expression_Node with private;

   type Box_Expression_Ptr is
      access all Box_Expression_Node;
   for Box_Expression_Ptr'Storage_Pool use Lists.Pool;

   function New_Box_Expression_Node
     (The_Context : ASIS.Context)
      return Box_Expression_Ptr;

   function Expression_Kind (Element : Box_Expression_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Box_Expression_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -----------------------
   -- Base_Literal_Node --
   -----------------------

   type Base_Literal_Node is abstract
      new Expression_Node with private;

   type Base_Literal_Ptr is
      access all Base_Literal_Node;
   for Base_Literal_Ptr'Storage_Pool use Lists.Pool;

   function Value_Image
     (Element : Base_Literal_Node) return Wide_String;

   procedure Set_Value_Image
     (Element : in out Base_Literal_Node;
      Value   : in     Wide_String);

   --------------------------
   -- Integer_Literal_Node --
   --------------------------

   type Integer_Literal_Node is 
      new Base_Literal_Node with private;

   type Integer_Literal_Ptr is
      access all Integer_Literal_Node;
   for Integer_Literal_Ptr'Storage_Pool use Lists.Pool;

   function New_Integer_Literal_Node
     (The_Context : ASIS.Context)
      return Integer_Literal_Ptr;

   function Expression_Kind (Element : Integer_Literal_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Integer_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -----------------------
   -- Real_Literal_Node --
   -----------------------

   type Real_Literal_Node is 
      new Base_Literal_Node with private;

   type Real_Literal_Ptr is
      access all Real_Literal_Node;
   for Real_Literal_Ptr'Storage_Pool use Lists.Pool;

   function New_Real_Literal_Node
     (The_Context : ASIS.Context)
      return Real_Literal_Ptr;

   function Expression_Kind (Element : Real_Literal_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Real_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -------------------------
   -- String_Literal_Node --
   -------------------------

   type String_Literal_Node is 
      new Base_Literal_Node with private;

   type String_Literal_Ptr is
      access all String_Literal_Node;
   for String_Literal_Ptr'Storage_Pool use Lists.Pool;

   function New_String_Literal_Node
     (The_Context : ASIS.Context)
      return String_Literal_Ptr;

   function Expression_Kind (Element : String_Literal_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : String_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   --------------------------
   -- Base_Identifier_Node --
   --------------------------

   type Base_Identifier_Node is abstract
      new Expression_Node with private;

   type Base_Identifier_Ptr is
      access all Base_Identifier_Node;
   for Base_Identifier_Ptr'Storage_Pool use Lists.Pool;

   function Name_Image
     (Element : Base_Identifier_Node) return Wide_String;

   procedure Set_Name_Image
     (Element : in out Base_Identifier_Node;
      Value   : in     Wide_String);

   function Corresponding_Name_Declaration
     (Element : Base_Identifier_Node) return Asis.Declaration;

   procedure Set_Corresponding_Name_Declaration
     (Element : in out Base_Identifier_Node;
      Value   : in     Asis.Declaration);

   function Corresponding_Name_Definition_List
     (Element : Base_Identifier_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Corresponding_Name_Definition_List
     (Element : in out Base_Identifier_Node;
      Item    : in     Asis.Element);

   function Corresponding_Generic_Element
     (Element : Base_Identifier_Node) return Asis.Defining_Name;

   procedure Set_Corresponding_Generic_Element
     (Element : in out Base_Identifier_Node;
      Value   : in     Asis.Defining_Name);

   ---------------------
   -- Identifier_Node --
   ---------------------

   type Identifier_Node is 
      new Base_Identifier_Node with private;

   type Identifier_Ptr is
      access all Identifier_Node;
   for Identifier_Ptr'Storage_Pool use Lists.Pool;

   function New_Identifier_Node
     (The_Context : ASIS.Context)
      return Identifier_Ptr;

   function Expression_Kind (Element : Identifier_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Identifier_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   --------------------------
   -- Operator_Symbol_Node --
   --------------------------

   type Operator_Symbol_Node is 
      new Base_Identifier_Node with private;

   type Operator_Symbol_Ptr is
      access all Operator_Symbol_Node;
   for Operator_Symbol_Ptr'Storage_Pool use Lists.Pool;

   function New_Operator_Symbol_Node
     (The_Context : ASIS.Context)
      return Operator_Symbol_Ptr;

   function Operator_Kind
     (Element : Operator_Symbol_Node) return Asis.Operator_Kinds;

   procedure Set_Operator_Kind
     (Element : in out Operator_Symbol_Node;
      Value   : in     Asis.Operator_Kinds);

   function Expression_Kind (Element : Operator_Symbol_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Operator_Symbol_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   ----------------------------
   -- Character_Literal_Node --
   ----------------------------

   type Character_Literal_Node is 
      new Base_Identifier_Node with private;

   type Character_Literal_Ptr is
      access all Character_Literal_Node;
   for Character_Literal_Ptr'Storage_Pool use Lists.Pool;

   function New_Character_Literal_Node
     (The_Context : ASIS.Context)
      return Character_Literal_Ptr;

   function Expression_Kind (Element : Character_Literal_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Character_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   ------------------------------
   -- Enumeration_Literal_Node --
   ------------------------------

   type Enumeration_Literal_Node is 
      new Base_Identifier_Node with private;

   type Enumeration_Literal_Ptr is
      access all Enumeration_Literal_Node;
   for Enumeration_Literal_Ptr'Storage_Pool use Lists.Pool;

   function New_Enumeration_Literal_Node
     (The_Context : ASIS.Context)
      return Enumeration_Literal_Ptr;

   function Expression_Kind (Element : Enumeration_Literal_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Enumeration_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -------------------------------
   -- Explicit_Dereference_Node --
   -------------------------------

   type Explicit_Dereference_Node is 
      new Expression_Node with private;

   type Explicit_Dereference_Ptr is
      access all Explicit_Dereference_Node;
   for Explicit_Dereference_Ptr'Storage_Pool use Lists.Pool;

   function New_Explicit_Dereference_Node
     (The_Context : ASIS.Context)
      return Explicit_Dereference_Ptr;

   function Prefix
     (Element : Explicit_Dereference_Node) return Asis.Expression;

   procedure Set_Prefix
     (Element : in out Explicit_Dereference_Node;
      Value   : in     Asis.Expression);

   function Expression_Kind (Element : Explicit_Dereference_Node)
      return Asis.Expression_Kinds;

   function Children (Element : access Explicit_Dereference_Node)
     return Traverse_List;

   function Clone
     (Element : Explicit_Dereference_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Explicit_Dereference_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------
   -- Function_Call_Node --
   ------------------------

   type Function_Call_Node is 
      new Expression_Node with private;

   type Function_Call_Ptr is
      access all Function_Call_Node;
   for Function_Call_Ptr'Storage_Pool use Lists.Pool;

   function New_Function_Call_Node
     (The_Context : ASIS.Context)
      return Function_Call_Ptr;

   function Prefix
     (Element : Function_Call_Node) return Asis.Expression;

   procedure Set_Prefix
     (Element : in out Function_Call_Node;
      Value   : in     Asis.Expression);

   function Is_Prefix_Call
     (Element : Function_Call_Node) return Boolean;

   procedure Set_Is_Prefix_Call
     (Element : in out Function_Call_Node;
      Value   : in     Boolean);

   function Is_Dispatching_Call
     (Element : Function_Call_Node) return Boolean;

   procedure Set_Is_Dispatching_Call
     (Element : in out Function_Call_Node;
      Value   : in     Boolean);

   function Corresponding_Called_Function
     (Element : Function_Call_Node) return Asis.Declaration;

   procedure Set_Corresponding_Called_Function
     (Element : in out Function_Call_Node;
      Value   : in     Asis.Declaration);

   function Function_Call_Parameters
     (Element : Function_Call_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Function_Call_Parameters
     (Element : in out Function_Call_Node;
      Value   : in     Asis.Element);

   function Function_Call_Parameters_List
     (Element : Function_Call_Node) return Asis.Element;

   function Normalized_Function_Call_Parameters
     (Element : Function_Call_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Normalized_Function_Call_Parameters
     (Element : in out Function_Call_Node;
      Item    : in     Asis.Element);

   function Is_Call_On_Dispatching_Operation
     (Element : Function_Call_Node) return Boolean;

   procedure Set_Is_Call_On_Dispatching_Operation
     (Element : in out Function_Call_Node;
      Value   : in     Boolean);

   function Record_Aggregate
     (Element : Function_Call_Node) return Asis.Element;

   procedure Set_Record_Aggregate
     (Element : in out Function_Call_Node;
      Value   : in     Asis.Element);

   function Expression_Kind (Element : Function_Call_Node)
      return Asis.Expression_Kinds;

   function Children (Element : access Function_Call_Node)
     return Traverse_List;

   function Clone
     (Element : Function_Call_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Function_Call_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------
   -- Indexed_Component_Node --
   ----------------------------

   type Indexed_Component_Node is 
      new Expression_Node with private;

   type Indexed_Component_Ptr is
      access all Indexed_Component_Node;
   for Indexed_Component_Ptr'Storage_Pool use Lists.Pool;

   function New_Indexed_Component_Node
     (The_Context : ASIS.Context)
      return Indexed_Component_Ptr;

   function Prefix
     (Element : Indexed_Component_Node) return Asis.Expression;

   procedure Set_Prefix
     (Element : in out Indexed_Component_Node;
      Value   : in     Asis.Expression);

   function Index_Expressions
     (Element : Indexed_Component_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Index_Expressions
     (Element : in out Indexed_Component_Node;
      Value   : in     Asis.Element);

   function Index_Expressions_List
     (Element : Indexed_Component_Node) return Asis.Element;

   function Expression_Kind (Element : Indexed_Component_Node)
      return Asis.Expression_Kinds;

   function Children (Element : access Indexed_Component_Node)
     return Traverse_List;

   function Clone
     (Element : Indexed_Component_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Indexed_Component_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------
   -- Slice_Node --
   ----------------

   type Slice_Node is 
      new Expression_Node with private;

   type Slice_Ptr is
      access all Slice_Node;
   for Slice_Ptr'Storage_Pool use Lists.Pool;

   function New_Slice_Node
     (The_Context : ASIS.Context)
      return Slice_Ptr;

   function Prefix
     (Element : Slice_Node) return Asis.Expression;

   procedure Set_Prefix
     (Element : in out Slice_Node;
      Value   : in     Asis.Expression);

   function Slice_Range
     (Element : Slice_Node) return Asis.Discrete_Range;

   procedure Set_Slice_Range
     (Element : in out Slice_Node;
      Value   : in     Asis.Discrete_Range);

   function Expression_Kind (Element : Slice_Node)
      return Asis.Expression_Kinds;

   function Children (Element : access Slice_Node)
     return Traverse_List;

   function Clone
     (Element : Slice_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Slice_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------
   -- Selected_Component_Node --
   -----------------------------

   type Selected_Component_Node is 
      new Expression_Node with private;

   type Selected_Component_Ptr is
      access all Selected_Component_Node;
   for Selected_Component_Ptr'Storage_Pool use Lists.Pool;

   function New_Selected_Component_Node
     (The_Context : ASIS.Context)
      return Selected_Component_Ptr;

   function Prefix
     (Element : Selected_Component_Node) return Asis.Expression;

   procedure Set_Prefix
     (Element : in out Selected_Component_Node;
      Value   : in     Asis.Expression);

   function Selector
     (Element : Selected_Component_Node) return Asis.Expression;

   procedure Set_Selector
     (Element : in out Selected_Component_Node;
      Value   : in     Asis.Expression);

   function Expression_Kind (Element : Selected_Component_Node)
      return Asis.Expression_Kinds;

   function Children (Element : access Selected_Component_Node)
     return Traverse_List;

   function Clone
     (Element : Selected_Component_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Selected_Component_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------
   -- Attribute_Reference_Node --
   ------------------------------

   type Attribute_Reference_Node is 
      new Expression_Node with private;

   type Attribute_Reference_Ptr is
      access all Attribute_Reference_Node;
   for Attribute_Reference_Ptr'Storage_Pool use Lists.Pool;

   function New_Attribute_Reference_Node
     (The_Context : ASIS.Context)
      return Attribute_Reference_Ptr;

   function Prefix
     (Element : Attribute_Reference_Node) return Asis.Expression;

   procedure Set_Prefix
     (Element : in out Attribute_Reference_Node;
      Value   : in     Asis.Expression);

   function Attribute_Kind
     (Element : Attribute_Reference_Node) return Asis.Attribute_Kinds;

   procedure Set_Attribute_Kind
     (Element : in out Attribute_Reference_Node;
      Value   : in     Asis.Attribute_Kinds);

   function Attribute_Designator_Identifier
     (Element : Attribute_Reference_Node) return Asis.Expression;

   procedure Set_Attribute_Designator_Identifier
     (Element : in out Attribute_Reference_Node;
      Value   : in     Asis.Expression);

   function Attribute_Designator_Expressions
     (Element : Attribute_Reference_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Attribute_Designator_Expressions
     (Element : in out Attribute_Reference_Node;
      Value   : in     Asis.Element);

   function Attribute_Designator_Expressions_List
     (Element : Attribute_Reference_Node) return Asis.Element;

   function Expression_Kind (Element : Attribute_Reference_Node)
      return Asis.Expression_Kinds;

   function Children (Element : access Attribute_Reference_Node)
     return Traverse_List;

   function Clone
     (Element : Attribute_Reference_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Attribute_Reference_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------
   -- Base_Record_Aggregate_Node --
   --------------------------------

   type Base_Record_Aggregate_Node is abstract
      new Expression_Node with private;

   type Base_Record_Aggregate_Ptr is
      access all Base_Record_Aggregate_Node;
   for Base_Record_Aggregate_Ptr'Storage_Pool use Lists.Pool;

   function Record_Component_Associations
     (Element : Base_Record_Aggregate_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Record_Component_Associations
     (Element : in out Base_Record_Aggregate_Node;
      Value   : in     Asis.Element);

   function Record_Component_Associations_List
     (Element : Base_Record_Aggregate_Node) return Asis.Element;

   function Normalized_Record_Component_Associations
     (Element : Base_Record_Aggregate_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Normalized_Record_Component_Associations
     (Element : in out Base_Record_Aggregate_Node;
      Item    : in     Asis.Element);

   function Children (Element : access Base_Record_Aggregate_Node)
     return Traverse_List;

   ---------------------------
   -- Record_Aggregate_Node --
   ---------------------------

   type Record_Aggregate_Node is 
      new Base_Record_Aggregate_Node with private;

   type Record_Aggregate_Ptr is
      access all Record_Aggregate_Node;
   for Record_Aggregate_Ptr'Storage_Pool use Lists.Pool;

   function New_Record_Aggregate_Node
     (The_Context : ASIS.Context)
      return Record_Aggregate_Ptr;

   function Expression_Kind (Element : Record_Aggregate_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Record_Aggregate_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Record_Aggregate_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------
   -- Extension_Aggregate_Node --
   ------------------------------

   type Extension_Aggregate_Node is 
      new Base_Record_Aggregate_Node with private;

   type Extension_Aggregate_Ptr is
      access all Extension_Aggregate_Node;
   for Extension_Aggregate_Ptr'Storage_Pool use Lists.Pool;

   function New_Extension_Aggregate_Node
     (The_Context : ASIS.Context)
      return Extension_Aggregate_Ptr;

   function Extension_Aggregate_Expression
     (Element : Extension_Aggregate_Node) return Asis.Expression;

   procedure Set_Extension_Aggregate_Expression
     (Element : in out Extension_Aggregate_Node;
      Value   : in     Asis.Expression);

   function Expression_Kind (Element : Extension_Aggregate_Node)
      return Asis.Expression_Kinds;

   function Children (Element : access Extension_Aggregate_Node)
     return Traverse_List;

   function Clone
     (Element : Extension_Aggregate_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Extension_Aggregate_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------
   -- Base_Array_Aggregate_Node --
   -------------------------------

   type Base_Array_Aggregate_Node is abstract
      new Expression_Node with private;

   type Base_Array_Aggregate_Ptr is
      access all Base_Array_Aggregate_Node;
   for Base_Array_Aggregate_Ptr'Storage_Pool use Lists.Pool;

   function Array_Component_Associations
     (Element : Base_Array_Aggregate_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Array_Component_Associations
     (Element : in out Base_Array_Aggregate_Node;
      Value   : in     Asis.Element);

   function Array_Component_Associations_List
     (Element : Base_Array_Aggregate_Node) return Asis.Element;

   function Children (Element : access Base_Array_Aggregate_Node)
     return Traverse_List;

   -------------------------------------
   -- Positional_Array_Aggregate_Node --
   -------------------------------------

   type Positional_Array_Aggregate_Node is 
      new Base_Array_Aggregate_Node with private;

   type Positional_Array_Aggregate_Ptr is
      access all Positional_Array_Aggregate_Node;
   for Positional_Array_Aggregate_Ptr'Storage_Pool use Lists.Pool;

   function New_Positional_Array_Aggregate_Node
     (The_Context : ASIS.Context)
      return Positional_Array_Aggregate_Ptr;

   function Expression_Kind (Element : Positional_Array_Aggregate_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Positional_Array_Aggregate_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Positional_Array_Aggregate_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------
   -- Named_Array_Aggregate_Node --
   --------------------------------

   type Named_Array_Aggregate_Node is 
      new Base_Array_Aggregate_Node with private;

   type Named_Array_Aggregate_Ptr is
      access all Named_Array_Aggregate_Node;
   for Named_Array_Aggregate_Ptr'Storage_Pool use Lists.Pool;

   function New_Named_Array_Aggregate_Node
     (The_Context : ASIS.Context)
      return Named_Array_Aggregate_Ptr;

   function Expression_Kind (Element : Named_Array_Aggregate_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Named_Array_Aggregate_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Named_Array_Aggregate_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------
   -- Base_Short_Circuit_Node --
   -----------------------------

   type Base_Short_Circuit_Node is abstract
      new Expression_Node with private;

   type Base_Short_Circuit_Ptr is
      access all Base_Short_Circuit_Node;
   for Base_Short_Circuit_Ptr'Storage_Pool use Lists.Pool;

   function Short_Circuit_Operation_Left_Expression
     (Element : Base_Short_Circuit_Node) return Asis.Expression;

   procedure Set_Short_Circuit_Operation_Left_Expression
     (Element : in out Base_Short_Circuit_Node;
      Value   : in     Asis.Expression);

   function Short_Circuit_Operation_Right_Expression
     (Element : Base_Short_Circuit_Node) return Asis.Expression;

   procedure Set_Short_Circuit_Operation_Right_Expression
     (Element : in out Base_Short_Circuit_Node;
      Value   : in     Asis.Expression);

   function Children (Element : access Base_Short_Circuit_Node)
     return Traverse_List;

   ---------------------------------
   -- And_Then_Short_Circuit_Node --
   ---------------------------------

   type And_Then_Short_Circuit_Node is 
      new Base_Short_Circuit_Node with private;

   type And_Then_Short_Circuit_Ptr is
      access all And_Then_Short_Circuit_Node;
   for And_Then_Short_Circuit_Ptr'Storage_Pool use Lists.Pool;

   function New_And_Then_Short_Circuit_Node
     (The_Context : ASIS.Context)
      return And_Then_Short_Circuit_Ptr;

   function Expression_Kind (Element : And_Then_Short_Circuit_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : And_Then_Short_Circuit_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access And_Then_Short_Circuit_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------
   -- Or_Else_Short_Circuit_Node --
   --------------------------------

   type Or_Else_Short_Circuit_Node is 
      new Base_Short_Circuit_Node with private;

   type Or_Else_Short_Circuit_Ptr is
      access all Or_Else_Short_Circuit_Node;
   for Or_Else_Short_Circuit_Ptr'Storage_Pool use Lists.Pool;

   function New_Or_Else_Short_Circuit_Node
     (The_Context : ASIS.Context)
      return Or_Else_Short_Circuit_Ptr;

   function Expression_Kind (Element : Or_Else_Short_Circuit_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Or_Else_Short_Circuit_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Or_Else_Short_Circuit_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------------
   -- In_Range_Membership_Test_Node --
   -----------------------------------

   type In_Range_Membership_Test_Node is 
      new Expression_Node with private;

   type In_Range_Membership_Test_Ptr is
      access all In_Range_Membership_Test_Node;
   for In_Range_Membership_Test_Ptr'Storage_Pool use Lists.Pool;

   function New_In_Range_Membership_Test_Node
     (The_Context : ASIS.Context)
      return In_Range_Membership_Test_Ptr;

   function Membership_Test_Expression
     (Element : In_Range_Membership_Test_Node) return Asis.Expression;

   procedure Set_Membership_Test_Expression
     (Element : in out In_Range_Membership_Test_Node;
      Value   : in     Asis.Expression);

   function Membership_Test_Range
     (Element : In_Range_Membership_Test_Node) return Asis.Range_Constraint;

   procedure Set_Membership_Test_Range
     (Element : in out In_Range_Membership_Test_Node;
      Value   : in     Asis.Range_Constraint);

   function Expression_Kind (Element : In_Range_Membership_Test_Node)
      return Asis.Expression_Kinds;

   function Children (Element : access In_Range_Membership_Test_Node)
     return Traverse_List;

   function Clone
     (Element : In_Range_Membership_Test_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access In_Range_Membership_Test_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------------------
   -- Not_In_Range_Membership_Test_Node --
   ---------------------------------------

   type Not_In_Range_Membership_Test_Node is 
      new In_Range_Membership_Test_Node with private;

   type Not_In_Range_Membership_Test_Ptr is
      access all Not_In_Range_Membership_Test_Node;
   for Not_In_Range_Membership_Test_Ptr'Storage_Pool use Lists.Pool;

   function New_Not_In_Range_Membership_Test_Node
     (The_Context : ASIS.Context)
      return Not_In_Range_Membership_Test_Ptr;

   function Expression_Kind (Element : Not_In_Range_Membership_Test_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Not_In_Range_Membership_Test_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Not_In_Range_Membership_Test_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------
   -- In_Type_Membership_Test_Node --
   ----------------------------------

   type In_Type_Membership_Test_Node is 
      new Expression_Node with private;

   type In_Type_Membership_Test_Ptr is
      access all In_Type_Membership_Test_Node;
   for In_Type_Membership_Test_Ptr'Storage_Pool use Lists.Pool;

   function New_In_Type_Membership_Test_Node
     (The_Context : ASIS.Context)
      return In_Type_Membership_Test_Ptr;

   function Membership_Test_Expression
     (Element : In_Type_Membership_Test_Node) return Asis.Expression;

   procedure Set_Membership_Test_Expression
     (Element : in out In_Type_Membership_Test_Node;
      Value   : in     Asis.Expression);

   function Membership_Test_Subtype_Mark
     (Element : In_Type_Membership_Test_Node) return Asis.Expression;

   procedure Set_Membership_Test_Subtype_Mark
     (Element : in out In_Type_Membership_Test_Node;
      Value   : in     Asis.Expression);

   function Expression_Kind (Element : In_Type_Membership_Test_Node)
      return Asis.Expression_Kinds;

   function Children (Element : access In_Type_Membership_Test_Node)
     return Traverse_List;

   function Clone
     (Element : In_Type_Membership_Test_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access In_Type_Membership_Test_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------------
   -- Not_In_Type_Membership_Test_Node --
   --------------------------------------

   type Not_In_Type_Membership_Test_Node is 
      new In_Type_Membership_Test_Node with private;

   type Not_In_Type_Membership_Test_Ptr is
      access all Not_In_Type_Membership_Test_Node;
   for Not_In_Type_Membership_Test_Ptr'Storage_Pool use Lists.Pool;

   function New_Not_In_Type_Membership_Test_Node
     (The_Context : ASIS.Context)
      return Not_In_Type_Membership_Test_Ptr;

   function Expression_Kind (Element : Not_In_Type_Membership_Test_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Not_In_Type_Membership_Test_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Not_In_Type_Membership_Test_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------
   -- Null_Literal_Node --
   -----------------------

   type Null_Literal_Node is 
      new Expression_Node with private;

   type Null_Literal_Ptr is
      access all Null_Literal_Node;
   for Null_Literal_Ptr'Storage_Pool use Lists.Pool;

   function New_Null_Literal_Node
     (The_Context : ASIS.Context)
      return Null_Literal_Ptr;

   function Expression_Kind (Element : Null_Literal_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Null_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -----------------------------------
   -- Parenthesized_Expression_Node --
   -----------------------------------

   type Parenthesized_Expression_Node is 
      new Expression_Node with private;

   type Parenthesized_Expression_Ptr is
      access all Parenthesized_Expression_Node;
   for Parenthesized_Expression_Ptr'Storage_Pool use Lists.Pool;

   function New_Parenthesized_Expression_Node
     (The_Context : ASIS.Context)
      return Parenthesized_Expression_Ptr;

   function Expression_Parenthesized
     (Element : Parenthesized_Expression_Node) return Asis.Expression;

   procedure Set_Expression_Parenthesized
     (Element : in out Parenthesized_Expression_Node;
      Value   : in     Asis.Expression);

   function Expression_Kind (Element : Parenthesized_Expression_Node)
      return Asis.Expression_Kinds;

   function Children (Element : access Parenthesized_Expression_Node)
     return Traverse_List;

   function Clone
     (Element : Parenthesized_Expression_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Parenthesized_Expression_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------
   -- Base_Conversion_Node --
   --------------------------

   type Base_Conversion_Node is abstract
      new Expression_Node with private;

   type Base_Conversion_Ptr is
      access all Base_Conversion_Node;
   for Base_Conversion_Ptr'Storage_Pool use Lists.Pool;

   function Converted_Or_Qualified_Subtype_Mark
     (Element : Base_Conversion_Node) return Asis.Expression;

   procedure Set_Converted_Or_Qualified_Subtype_Mark
     (Element : in out Base_Conversion_Node;
      Value   : in     Asis.Expression);

   function Converted_Or_Qualified_Expression
     (Element : Base_Conversion_Node) return Asis.Expression;

   procedure Set_Converted_Or_Qualified_Expression
     (Element : in out Base_Conversion_Node;
      Value   : in     Asis.Expression);

   function Children (Element : access Base_Conversion_Node)
     return Traverse_List;

   --------------------------
   -- Type_Conversion_Node --
   --------------------------

   type Type_Conversion_Node is 
      new Base_Conversion_Node with private;

   type Type_Conversion_Ptr is
      access all Type_Conversion_Node;
   for Type_Conversion_Ptr'Storage_Pool use Lists.Pool;

   function New_Type_Conversion_Node
     (The_Context : ASIS.Context)
      return Type_Conversion_Ptr;

   function Expression_Kind (Element : Type_Conversion_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Type_Conversion_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Type_Conversion_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------
   -- Qualified_Expression_Node --
   -------------------------------

   type Qualified_Expression_Node is 
      new Base_Conversion_Node with private;

   type Qualified_Expression_Ptr is
      access all Qualified_Expression_Node;
   for Qualified_Expression_Ptr'Storage_Pool use Lists.Pool;

   function New_Qualified_Expression_Node
     (The_Context : ASIS.Context)
      return Qualified_Expression_Ptr;

   function Expression_Kind (Element : Qualified_Expression_Node)
      return Asis.Expression_Kinds;

   function Clone
     (Element : Qualified_Expression_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Qualified_Expression_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------
   -- Allocation_From_Subtype_Node --
   ----------------------------------

   type Allocation_From_Subtype_Node is 
      new Expression_Node with private;

   type Allocation_From_Subtype_Ptr is
      access all Allocation_From_Subtype_Node;
   for Allocation_From_Subtype_Ptr'Storage_Pool use Lists.Pool;

   function New_Allocation_From_Subtype_Node
     (The_Context : ASIS.Context)
      return Allocation_From_Subtype_Ptr;

   function Allocator_Subtype_Indication
     (Element : Allocation_From_Subtype_Node) return Asis.Subtype_Indication;

   procedure Set_Allocator_Subtype_Indication
     (Element : in out Allocation_From_Subtype_Node;
      Value   : in     Asis.Subtype_Indication);

   function Expression_Kind (Element : Allocation_From_Subtype_Node)
      return Asis.Expression_Kinds;

   function Children (Element : access Allocation_From_Subtype_Node)
     return Traverse_List;

   function Clone
     (Element : Allocation_From_Subtype_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Allocation_From_Subtype_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------------------------
   -- Allocation_From_Qualified_Expression_Node --
   -----------------------------------------------

   type Allocation_From_Qualified_Expression_Node is 
      new Expression_Node with private;

   type Allocation_From_Qualified_Expression_Ptr is
      access all Allocation_From_Qualified_Expression_Node;
   for Allocation_From_Qualified_Expression_Ptr'Storage_Pool use Lists.Pool;

   function New_Allocation_From_Qualified_Expression_Node
     (The_Context : ASIS.Context)
      return Allocation_From_Qualified_Expression_Ptr;

   function Allocator_Qualified_Expression
     (Element : Allocation_From_Qualified_Expression_Node) return Asis.Expression;

   procedure Set_Allocator_Qualified_Expression
     (Element : in out Allocation_From_Qualified_Expression_Node;
      Value   : in     Asis.Expression);

   function Expression_Kind (Element : Allocation_From_Qualified_Expression_Node)
      return Asis.Expression_Kinds;

   function Children (Element : access Allocation_From_Qualified_Expression_Node)
     return Traverse_List;

   function Clone
     (Element : Allocation_From_Qualified_Expression_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Allocation_From_Qualified_Expression_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

private


   type Box_Expression_Node is 
      new Expression_Node with
      record
         null;
      end record;


   type Base_Literal_Node is abstract
      new Expression_Node with
      record
         Value_Image                    : aliased Unbounded_Wide_String;
      end record;


   type Integer_Literal_Node is 
      new Base_Literal_Node with
      record
         null;
      end record;


   type Real_Literal_Node is 
      new Base_Literal_Node with
      record
         null;
      end record;


   type String_Literal_Node is 
      new Base_Literal_Node with
      record
         null;
      end record;


   type Base_Identifier_Node is abstract
      new Expression_Node with
      record
         Name_Image                     : aliased Unbounded_Wide_String;
         Corresponding_Name_Declaration : aliased Asis.Declaration;
         Corresponding_Name_Definition_List : aliased Secondary_Definition_Lists.List_Node;
         Corresponding_Generic_Element  : aliased Asis.Defining_Name;
      end record;


   type Identifier_Node is 
      new Base_Identifier_Node with
      record
         null;
      end record;


   type Operator_Symbol_Node is 
      new Base_Identifier_Node with
      record
         Operator_Kind                  : aliased Asis.Operator_Kinds := Not_An_Operator;
      end record;


   type Character_Literal_Node is 
      new Base_Identifier_Node with
      record
         null;
      end record;


   type Enumeration_Literal_Node is 
      new Base_Identifier_Node with
      record
         null;
      end record;


   type Explicit_Dereference_Node is 
      new Expression_Node with
      record
         Prefix                         : aliased Asis.Expression;
      end record;


   type Function_Call_Node is 
      new Expression_Node with
      record
         Prefix                         : aliased Asis.Expression;
         Is_Prefix_Call                 : aliased Boolean
           := True;
         Is_Dispatching_Call            : aliased Boolean := False;
         Corresponding_Called_Function  : aliased Asis.Declaration;
         Function_Call_Parameters       : aliased Primary_Association_Lists.List;
         Normalized_Function_Call_Parameters : aliased Secondary_Association_Lists.List_Node;
         Is_Call_On_Dispatching_Operation : aliased Boolean := False;
         Record_Aggregate               : aliased Asis.Element;
      end record;


   type Indexed_Component_Node is 
      new Expression_Node with
      record
         Prefix                         : aliased Asis.Expression;
         Index_Expressions              : aliased Primary_Expression_Lists.List;
      end record;


   type Slice_Node is 
      new Expression_Node with
      record
         Prefix                         : aliased Asis.Expression;
         Slice_Range                    : aliased Asis.Discrete_Range;
      end record;


   type Selected_Component_Node is 
      new Expression_Node with
      record
         Prefix                         : aliased Asis.Expression;
         Selector                       : aliased Asis.Expression;
      end record;


   type Attribute_Reference_Node is 
      new Expression_Node with
      record
         Prefix                         : aliased Asis.Expression;
         Attribute_Kind                 : aliased Asis.Attribute_Kinds := Not_An_Attribute;
         Attribute_Designator_Identifier : aliased Asis.Expression;
         Attribute_Designator_Expressions : aliased Primary_Expression_Lists.List;
      end record;


   type Base_Record_Aggregate_Node is abstract
      new Expression_Node with
      record
         Record_Component_Associations  : aliased Primary_Association_Lists.List;
         Normalized_Record_Component_Associations : aliased Secondary_Association_Lists.List_Node;
      end record;


   type Record_Aggregate_Node is 
      new Base_Record_Aggregate_Node with
      record
         null;
      end record;


   type Extension_Aggregate_Node is 
      new Base_Record_Aggregate_Node with
      record
         Extension_Aggregate_Expression : aliased Asis.Expression;
      end record;


   type Base_Array_Aggregate_Node is abstract
      new Expression_Node with
      record
         Array_Component_Associations   : aliased Primary_Association_Lists.List;
      end record;


   type Positional_Array_Aggregate_Node is 
      new Base_Array_Aggregate_Node with
      record
         null;
      end record;


   type Named_Array_Aggregate_Node is 
      new Base_Array_Aggregate_Node with
      record
         null;
      end record;


   type Base_Short_Circuit_Node is abstract
      new Expression_Node with
      record
         Short_Circuit_Operation_Left_Expression : aliased Asis.Expression;
         Short_Circuit_Operation_Right_Expression : aliased Asis.Expression;
      end record;


   type And_Then_Short_Circuit_Node is 
      new Base_Short_Circuit_Node with
      record
         null;
      end record;


   type Or_Else_Short_Circuit_Node is 
      new Base_Short_Circuit_Node with
      record
         null;
      end record;


   type In_Range_Membership_Test_Node is 
      new Expression_Node with
      record
         Membership_Test_Expression     : aliased Asis.Expression;
         Membership_Test_Range          : aliased Asis.Range_Constraint;
      end record;


   type Not_In_Range_Membership_Test_Node is 
      new In_Range_Membership_Test_Node with
      record
         null;
      end record;


   type In_Type_Membership_Test_Node is 
      new Expression_Node with
      record
         Membership_Test_Expression     : aliased Asis.Expression;
         Membership_Test_Subtype_Mark   : aliased Asis.Expression;
      end record;


   type Not_In_Type_Membership_Test_Node is 
      new In_Type_Membership_Test_Node with
      record
         null;
      end record;


   type Null_Literal_Node is 
      new Expression_Node with
      record
         null;
      end record;


   type Parenthesized_Expression_Node is 
      new Expression_Node with
      record
         Expression_Parenthesized       : aliased Asis.Expression;
      end record;


   type Base_Conversion_Node is abstract
      new Expression_Node with
      record
         Converted_Or_Qualified_Subtype_Mark : aliased Asis.Expression;
         Converted_Or_Qualified_Expression : aliased Asis.Expression;
      end record;


   type Type_Conversion_Node is 
      new Base_Conversion_Node with
      record
         null;
      end record;


   type Qualified_Expression_Node is 
      new Base_Conversion_Node with
      record
         null;
      end record;


   type Allocation_From_Subtype_Node is 
      new Expression_Node with
      record
         Allocator_Subtype_Indication   : aliased Asis.Subtype_Indication;
      end record;


   type Allocation_From_Qualified_Expression_Node is 
      new Expression_Node with
      record
         Allocator_Qualified_Expression : aliased Asis.Expression;
      end record;

end Asis.Gela.Elements.Expr;
