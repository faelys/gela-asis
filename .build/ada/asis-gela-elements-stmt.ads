
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
  
package Asis.Gela.Elements.Stmt is

   ------------------------------
   -- Base_Path_Statement_Node --
   ------------------------------

   type Base_Path_Statement_Node is abstract
      new Statement_Node with private;

   type Base_Path_Statement_Ptr is
      access all Base_Path_Statement_Node;
   for Base_Path_Statement_Ptr'Storage_Pool use Lists.Pool;

   function Statement_Paths
     (Element : Base_Path_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Statement_Paths
     (Element : in out Base_Path_Statement_Node;
      Value   : in     Asis.Element);

   function Statement_Paths_List
     (Element : Base_Path_Statement_Node) return Asis.Element;

   function Children (Element : access Base_Path_Statement_Node)
     return Traverse_List;

   -----------------------
   -- If_Statement_Node --
   -----------------------

   type If_Statement_Node is 
      new Base_Path_Statement_Node with private;

   type If_Statement_Ptr is
      access all If_Statement_Node;
   for If_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_If_Statement_Node
     (The_Context : ASIS.Context)
      return If_Statement_Ptr;

   function Statement_Kind (Element : If_Statement_Node)
      return Asis.Statement_Kinds;

   function Clone
     (Element : If_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access If_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------
   -- Case_Statement_Node --
   -------------------------

   type Case_Statement_Node is 
      new Base_Path_Statement_Node with private;

   type Case_Statement_Ptr is
      access all Case_Statement_Node;
   for Case_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Case_Statement_Node
     (The_Context : ASIS.Context)
      return Case_Statement_Ptr;

   function Case_Expression
     (Element : Case_Statement_Node) return Asis.Expression;

   procedure Set_Case_Expression
     (Element : in out Case_Statement_Node;
      Value   : in     Asis.Expression);

   function Statement_Kind (Element : Case_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access Case_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : Case_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Case_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------------
   -- Selective_Accept_Statement_Node --
   -------------------------------------

   type Selective_Accept_Statement_Node is 
      new Base_Path_Statement_Node with private;

   type Selective_Accept_Statement_Ptr is
      access all Selective_Accept_Statement_Node;
   for Selective_Accept_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Selective_Accept_Statement_Node
     (The_Context : ASIS.Context)
      return Selective_Accept_Statement_Ptr;

   function Statement_Kind (Element : Selective_Accept_Statement_Node)
      return Asis.Statement_Kinds;

   function Clone
     (Element : Selective_Accept_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Selective_Accept_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------------
   -- Timed_Entry_Call_Statement_Node --
   -------------------------------------

   type Timed_Entry_Call_Statement_Node is 
      new Base_Path_Statement_Node with private;

   type Timed_Entry_Call_Statement_Ptr is
      access all Timed_Entry_Call_Statement_Node;
   for Timed_Entry_Call_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Timed_Entry_Call_Statement_Node
     (The_Context : ASIS.Context)
      return Timed_Entry_Call_Statement_Ptr;

   function Statement_Kind (Element : Timed_Entry_Call_Statement_Node)
      return Asis.Statement_Kinds;

   function Clone
     (Element : Timed_Entry_Call_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Timed_Entry_Call_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------------------
   -- Conditional_Entry_Call_Statement_Node --
   -------------------------------------------

   type Conditional_Entry_Call_Statement_Node is 
      new Base_Path_Statement_Node with private;

   type Conditional_Entry_Call_Statement_Ptr is
      access all Conditional_Entry_Call_Statement_Node;
   for Conditional_Entry_Call_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Conditional_Entry_Call_Statement_Node
     (The_Context : ASIS.Context)
      return Conditional_Entry_Call_Statement_Ptr;

   function Statement_Kind (Element : Conditional_Entry_Call_Statement_Node)
      return Asis.Statement_Kinds;

   function Clone
     (Element : Conditional_Entry_Call_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Conditional_Entry_Call_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------------
   -- Asynchronous_Select_Statement_Node --
   ----------------------------------------

   type Asynchronous_Select_Statement_Node is 
      new Base_Path_Statement_Node with private;

   type Asynchronous_Select_Statement_Ptr is
      access all Asynchronous_Select_Statement_Node;
   for Asynchronous_Select_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Asynchronous_Select_Statement_Node
     (The_Context : ASIS.Context)
      return Asynchronous_Select_Statement_Ptr;

   function Statement_Kind (Element : Asynchronous_Select_Statement_Node)
      return Asis.Statement_Kinds;

   function Clone
     (Element : Asynchronous_Select_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Asynchronous_Select_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------
   -- Null_Statement_Node --
   -------------------------

   type Null_Statement_Node is 
      new Statement_Node with private;

   type Null_Statement_Ptr is
      access all Null_Statement_Node;
   for Null_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Null_Statement_Node
     (The_Context : ASIS.Context)
      return Null_Statement_Ptr;

   function Statement_Kind (Element : Null_Statement_Node)
      return Asis.Statement_Kinds;

   function Clone
     (Element : Null_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Null_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------
   -- Assignment_Statement_Node --
   -------------------------------

   type Assignment_Statement_Node is 
      new Statement_Node with private;

   type Assignment_Statement_Ptr is
      access all Assignment_Statement_Node;
   for Assignment_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Assignment_Statement_Node
     (The_Context : ASIS.Context)
      return Assignment_Statement_Ptr;

   function Assignment_Variable_Name
     (Element : Assignment_Statement_Node) return Asis.Expression;

   procedure Set_Assignment_Variable_Name
     (Element : in out Assignment_Statement_Node;
      Value   : in     Asis.Expression);

   function Assignment_Expression
     (Element : Assignment_Statement_Node) return Asis.Expression;

   procedure Set_Assignment_Expression
     (Element : in out Assignment_Statement_Node;
      Value   : in     Asis.Expression);

   function Statement_Kind (Element : Assignment_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access Assignment_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : Assignment_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Assignment_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------
   -- Loop_Statement_Node --
   -------------------------

   type Loop_Statement_Node is 
      new Statement_Node with private;

   type Loop_Statement_Ptr is
      access all Loop_Statement_Node;
   for Loop_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Loop_Statement_Node
     (The_Context : ASIS.Context)
      return Loop_Statement_Ptr;

   function Statement_Identifier
     (Element : Loop_Statement_Node) return Asis.Defining_Name;

   procedure Set_Statement_Identifier
     (Element : in out Loop_Statement_Node;
      Value   : in     Asis.Defining_Name);

   function Back_Identifier
     (Element : Loop_Statement_Node) return Asis.Identifier;

   procedure Set_Back_Identifier
     (Element : in out Loop_Statement_Node;
      Value   : in     Asis.Identifier);

   function Is_Name_Repeated
     (Element : Loop_Statement_Node) return Boolean;

   procedure Set_Is_Name_Repeated
     (Element : in out Loop_Statement_Node;
      Value   : in     Boolean);

   function Loop_Statements
     (Element : Loop_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Loop_Statements
     (Element : in out Loop_Statement_Node;
      Value   : in     Asis.Element);

   function Loop_Statements_List
     (Element : Loop_Statement_Node) return Asis.Element;

   function Statement_Kind (Element : Loop_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access Loop_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : Loop_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Loop_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------
   -- While_Loop_Statement_Node --
   -------------------------------

   type While_Loop_Statement_Node is 
      new Loop_Statement_Node with private;

   type While_Loop_Statement_Ptr is
      access all While_Loop_Statement_Node;
   for While_Loop_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_While_Loop_Statement_Node
     (The_Context : ASIS.Context)
      return While_Loop_Statement_Ptr;

   function While_Condition
     (Element : While_Loop_Statement_Node) return Asis.Expression;

   procedure Set_While_Condition
     (Element : in out While_Loop_Statement_Node;
      Value   : in     Asis.Expression);

   function Statement_Kind (Element : While_Loop_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access While_Loop_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : While_Loop_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access While_Loop_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------
   -- For_Loop_Statement_Node --
   -----------------------------

   type For_Loop_Statement_Node is 
      new Loop_Statement_Node with private;

   type For_Loop_Statement_Ptr is
      access all For_Loop_Statement_Node;
   for For_Loop_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_For_Loop_Statement_Node
     (The_Context : ASIS.Context)
      return For_Loop_Statement_Ptr;

   function Loop_Parameter_Specification
     (Element : For_Loop_Statement_Node) return Asis.Declaration;

   procedure Set_Loop_Parameter_Specification
     (Element : in out For_Loop_Statement_Node;
      Value   : in     Asis.Declaration);

   function Statement_Kind (Element : For_Loop_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access For_Loop_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : For_Loop_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access For_Loop_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------
   -- Block_Statement_Node --
   --------------------------

   type Block_Statement_Node is 
      new Statement_Node with private;

   type Block_Statement_Ptr is
      access all Block_Statement_Node;
   for Block_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Block_Statement_Node
     (The_Context : ASIS.Context)
      return Block_Statement_Ptr;

   function Is_Name_Repeated
     (Element : Block_Statement_Node) return Boolean;

   procedure Set_Is_Name_Repeated
     (Element : in out Block_Statement_Node;
      Value   : in     Boolean);

   function Is_Declare_Block
     (Element : Block_Statement_Node) return Boolean;

   procedure Set_Is_Declare_Block
     (Element : in out Block_Statement_Node;
      Value   : in     Boolean);

   function Block_Declarative_Items
     (Element : Block_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Block_Declarative_Items
     (Element : in out Block_Statement_Node;
      Value   : in     Asis.Element);

   function Block_Declarative_Items_List
     (Element : Block_Statement_Node) return Asis.Element;

   function Block_Statements
     (Element : Block_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Block_Statements
     (Element : in out Block_Statement_Node;
      Value   : in     Asis.Element);

   function Block_Statements_List
     (Element : Block_Statement_Node) return Asis.Element;

   function Block_Exception_Handlers
     (Element : Block_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Block_Exception_Handlers
     (Element : in out Block_Statement_Node;
      Value   : in     Asis.Element);

   function Block_Exception_Handlers_List
     (Element : Block_Statement_Node) return Asis.Element;

   function Statement_Identifier
     (Element : Block_Statement_Node) return Asis.Defining_Name;

   procedure Set_Statement_Identifier
     (Element : in out Block_Statement_Node;
      Value   : in     Asis.Defining_Name);

   function Back_Identifier
     (Element : Block_Statement_Node) return Asis.Identifier;

   procedure Set_Back_Identifier
     (Element : in out Block_Statement_Node;
      Value   : in     Asis.Identifier);

   function Handled_Statements
     (Element : Block_Statement_Node) return Asis.Element;

   procedure Set_Handled_Statements
     (Element : in out Block_Statement_Node;
      Value   : in     Asis.Element);

   function Statement_Kind (Element : Block_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access Block_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : Block_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Block_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------
   -- Exit_Statement_Node --
   -------------------------

   type Exit_Statement_Node is 
      new Statement_Node with private;

   type Exit_Statement_Ptr is
      access all Exit_Statement_Node;
   for Exit_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Exit_Statement_Node
     (The_Context : ASIS.Context)
      return Exit_Statement_Ptr;

   function Exit_Loop_Name
     (Element : Exit_Statement_Node) return Asis.Expression;

   procedure Set_Exit_Loop_Name
     (Element : in out Exit_Statement_Node;
      Value   : in     Asis.Expression);

   function Corresponding_Loop_Exited
     (Element : Exit_Statement_Node) return Asis.Statement;

   procedure Set_Corresponding_Loop_Exited
     (Element : in out Exit_Statement_Node;
      Value   : in     Asis.Statement);

   function Exit_Condition
     (Element : Exit_Statement_Node) return Asis.Expression;

   procedure Set_Exit_Condition
     (Element : in out Exit_Statement_Node;
      Value   : in     Asis.Expression);

   function Statement_Kind (Element : Exit_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access Exit_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : Exit_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Exit_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------
   -- Goto_Statement_Node --
   -------------------------

   type Goto_Statement_Node is 
      new Statement_Node with private;

   type Goto_Statement_Ptr is
      access all Goto_Statement_Node;
   for Goto_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Goto_Statement_Node
     (The_Context : ASIS.Context)
      return Goto_Statement_Ptr;

   function Goto_Label
     (Element : Goto_Statement_Node) return Asis.Expression;

   procedure Set_Goto_Label
     (Element : in out Goto_Statement_Node;
      Value   : in     Asis.Expression);

   function Corresponding_Destination_Statement
     (Element : Goto_Statement_Node) return Asis.Statement;

   procedure Set_Corresponding_Destination_Statement
     (Element : in out Goto_Statement_Node;
      Value   : in     Asis.Statement);

   function Statement_Kind (Element : Goto_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access Goto_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : Goto_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Goto_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------
   -- Base_Call_Statement_Node --
   ------------------------------

   type Base_Call_Statement_Node is abstract
      new Statement_Node with private;

   type Base_Call_Statement_Ptr is
      access all Base_Call_Statement_Node;
   for Base_Call_Statement_Ptr'Storage_Pool use Lists.Pool;

   function Called_Name
     (Element : Base_Call_Statement_Node) return Asis.Expression;

   procedure Set_Called_Name
     (Element : in out Base_Call_Statement_Node;
      Value   : in     Asis.Expression);

   function Corresponding_Called_Entity
     (Element : Base_Call_Statement_Node) return Asis.Declaration;

   procedure Set_Corresponding_Called_Entity
     (Element : in out Base_Call_Statement_Node;
      Value   : in     Asis.Declaration);

   function Call_Statement_Parameters
     (Element : Base_Call_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Call_Statement_Parameters
     (Element : in out Base_Call_Statement_Node;
      Value   : in     Asis.Element);

   function Call_Statement_Parameters_List
     (Element : Base_Call_Statement_Node) return Asis.Element;

   function Normalized_Call_Statement_Parameters
     (Element : Base_Call_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Normalized_Call_Statement_Parameters
     (Element : in out Base_Call_Statement_Node;
      Item    : in     Asis.Element);

   function Children (Element : access Base_Call_Statement_Node)
     return Traverse_List;

   -----------------------------------
   -- Procedure_Call_Statement_Node --
   -----------------------------------

   type Procedure_Call_Statement_Node is 
      new Base_Call_Statement_Node with private;

   type Procedure_Call_Statement_Ptr is
      access all Procedure_Call_Statement_Node;
   for Procedure_Call_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Procedure_Call_Statement_Node
     (The_Context : ASIS.Context)
      return Procedure_Call_Statement_Ptr;

   function Is_Call_On_Dispatching_Operation
     (Element : Procedure_Call_Statement_Node) return Boolean;

   procedure Set_Is_Call_On_Dispatching_Operation
     (Element : in out Procedure_Call_Statement_Node;
      Value   : in     Boolean);

   function Is_Dispatching_Call
     (Element : Procedure_Call_Statement_Node) return Boolean;

   procedure Set_Is_Dispatching_Call
     (Element : in out Procedure_Call_Statement_Node;
      Value   : in     Boolean);

   function Statement_Kind (Element : Procedure_Call_Statement_Node)
      return Asis.Statement_Kinds;

   function Clone
     (Element : Procedure_Call_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Procedure_Call_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------
   -- Entry_Call_Statement_Node --
   -------------------------------

   type Entry_Call_Statement_Node is 
      new Base_Call_Statement_Node with private;

   type Entry_Call_Statement_Ptr is
      access all Entry_Call_Statement_Node;
   for Entry_Call_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Entry_Call_Statement_Node
     (The_Context : ASIS.Context)
      return Entry_Call_Statement_Ptr;

   function Statement_Kind (Element : Entry_Call_Statement_Node)
      return Asis.Statement_Kinds;

   function Clone
     (Element : Entry_Call_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Entry_Call_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------
   -- Simple_Return_Statement_Node --
   ----------------------------------

   type Simple_Return_Statement_Node is 
      new Statement_Node with private;

   type Simple_Return_Statement_Ptr is
      access all Simple_Return_Statement_Node;
   for Simple_Return_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Simple_Return_Statement_Node
     (The_Context : ASIS.Context)
      return Simple_Return_Statement_Ptr;

   function Return_Expression
     (Element : Simple_Return_Statement_Node) return Asis.Expression;

   procedure Set_Return_Expression
     (Element : in out Simple_Return_Statement_Node;
      Value   : in     Asis.Expression);

   function Statement_Kind (Element : Simple_Return_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access Simple_Return_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : Simple_Return_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Simple_Return_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------------
   -- Extended_Return_Statement_Node --
   ------------------------------------

   type Extended_Return_Statement_Node is 
      new Statement_Node with private;

   type Extended_Return_Statement_Ptr is
      access all Extended_Return_Statement_Node;
   for Extended_Return_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Extended_Return_Statement_Node
     (The_Context : ASIS.Context)
      return Extended_Return_Statement_Ptr;

   function Return_Object_Specification
     (Element : Extended_Return_Statement_Node) return Asis.Declaration;

   procedure Set_Return_Object_Specification
     (Element : in out Extended_Return_Statement_Node;
      Value   : in     Asis.Declaration);

   function Extended_Return_Statements
     (Element : Extended_Return_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Extended_Return_Statements
     (Element : in out Extended_Return_Statement_Node;
      Value   : in     Asis.Element);

   function Extended_Return_Statements_List
     (Element : Extended_Return_Statement_Node) return Asis.Element;

   function Extended_Return_Exception_Handlers
     (Element : Extended_Return_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Extended_Return_Exception_Handlers
     (Element : in out Extended_Return_Statement_Node;
      Value   : in     Asis.Element);

   function Extended_Return_Exception_Handlers_List
     (Element : Extended_Return_Statement_Node) return Asis.Element;

   function Handled_Statements
     (Element : Extended_Return_Statement_Node) return Asis.Element;

   procedure Set_Handled_Statements
     (Element : in out Extended_Return_Statement_Node;
      Value   : in     Asis.Element);

   function Statement_Kind (Element : Extended_Return_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access Extended_Return_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : Extended_Return_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Extended_Return_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------
   -- Accept_Statement_Node --
   ---------------------------

   type Accept_Statement_Node is 
      new Statement_Node with private;

   type Accept_Statement_Ptr is
      access all Accept_Statement_Node;
   for Accept_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Accept_Statement_Node
     (The_Context : ASIS.Context)
      return Accept_Statement_Ptr;

   function Accept_Entry_Index
     (Element : Accept_Statement_Node) return Asis.Expression;

   procedure Set_Accept_Entry_Index
     (Element : in out Accept_Statement_Node;
      Value   : in     Asis.Expression);

   function Accept_Entry_Direct_Name
     (Element : Accept_Statement_Node) return Asis.Name;

   procedure Set_Accept_Entry_Direct_Name
     (Element : in out Accept_Statement_Node;
      Value   : in     Asis.Name);

   function Accept_Parameters
     (Element : Accept_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Accept_Parameters
     (Element : in out Accept_Statement_Node;
      Value   : in     Asis.Element);

   function Accept_Parameters_List
     (Element : Accept_Statement_Node) return Asis.Element;

   function Accept_Body_Statements
     (Element : Accept_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Accept_Body_Statements
     (Element : in out Accept_Statement_Node;
      Value   : in     Asis.Element);

   function Accept_Body_Statements_List
     (Element : Accept_Statement_Node) return Asis.Element;

   function Accept_Body_Exception_Handlers
     (Element : Accept_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Accept_Body_Exception_Handlers
     (Element : in out Accept_Statement_Node;
      Value   : in     Asis.Element);

   function Accept_Body_Exception_Handlers_List
     (Element : Accept_Statement_Node) return Asis.Element;

   function Corresponding_Entry
     (Element : Accept_Statement_Node) return Asis.Declaration;

   procedure Set_Corresponding_Entry
     (Element : in out Accept_Statement_Node;
      Value   : in     Asis.Declaration);

   function Is_Name_Repeated
     (Element : Accept_Statement_Node) return Boolean;

   procedure Set_Is_Name_Repeated
     (Element : in out Accept_Statement_Node;
      Value   : in     Boolean);

   function Handled_Statements
     (Element : Accept_Statement_Node) return Asis.Element;

   procedure Set_Handled_Statements
     (Element : in out Accept_Statement_Node;
      Value   : in     Asis.Element);

   function Statement_Kind (Element : Accept_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access Accept_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : Accept_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Accept_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------
   -- Requeue_Statement_Node --
   ----------------------------

   type Requeue_Statement_Node is 
      new Statement_Node with private;

   type Requeue_Statement_Ptr is
      access all Requeue_Statement_Node;
   for Requeue_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Requeue_Statement_Node
     (The_Context : ASIS.Context)
      return Requeue_Statement_Ptr;

   function Requeue_Entry_Name
     (Element : Requeue_Statement_Node) return Asis.Name;

   procedure Set_Requeue_Entry_Name
     (Element : in out Requeue_Statement_Node;
      Value   : in     Asis.Name);

   function Statement_Kind (Element : Requeue_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access Requeue_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : Requeue_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Requeue_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------------------
   -- Requeue_Statement_With_Abort_Node --
   ---------------------------------------

   type Requeue_Statement_With_Abort_Node is 
      new Requeue_Statement_Node with private;

   type Requeue_Statement_With_Abort_Ptr is
      access all Requeue_Statement_With_Abort_Node;
   for Requeue_Statement_With_Abort_Ptr'Storage_Pool use Lists.Pool;

   function New_Requeue_Statement_With_Abort_Node
     (The_Context : ASIS.Context)
      return Requeue_Statement_With_Abort_Ptr;

   function Statement_Kind (Element : Requeue_Statement_With_Abort_Node)
      return Asis.Statement_Kinds;

   function Clone
     (Element : Requeue_Statement_With_Abort_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Requeue_Statement_With_Abort_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------
   -- Delay_Until_Statement_Node --
   --------------------------------

   type Delay_Until_Statement_Node is 
      new Statement_Node with private;

   type Delay_Until_Statement_Ptr is
      access all Delay_Until_Statement_Node;
   for Delay_Until_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Delay_Until_Statement_Node
     (The_Context : ASIS.Context)
      return Delay_Until_Statement_Ptr;

   function Delay_Expression
     (Element : Delay_Until_Statement_Node) return Asis.Expression;

   procedure Set_Delay_Expression
     (Element : in out Delay_Until_Statement_Node;
      Value   : in     Asis.Expression);

   function Statement_Kind (Element : Delay_Until_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access Delay_Until_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : Delay_Until_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Delay_Until_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -----------------------------------
   -- Delay_Relative_Statement_Node --
   -----------------------------------

   type Delay_Relative_Statement_Node is 
      new Delay_Until_Statement_Node with private;

   type Delay_Relative_Statement_Ptr is
      access all Delay_Relative_Statement_Node;
   for Delay_Relative_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Delay_Relative_Statement_Node
     (The_Context : ASIS.Context)
      return Delay_Relative_Statement_Ptr;

   function Statement_Kind (Element : Delay_Relative_Statement_Node)
      return Asis.Statement_Kinds;

   function Clone
     (Element : Delay_Relative_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Delay_Relative_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ------------------------------------------
   -- Terminate_Alternative_Statement_Node --
   ------------------------------------------

   type Terminate_Alternative_Statement_Node is 
      new Statement_Node with private;

   type Terminate_Alternative_Statement_Ptr is
      access all Terminate_Alternative_Statement_Node;
   for Terminate_Alternative_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Terminate_Alternative_Statement_Node
     (The_Context : ASIS.Context)
      return Terminate_Alternative_Statement_Ptr;

   function Statement_Kind (Element : Terminate_Alternative_Statement_Node)
      return Asis.Statement_Kinds;

   function Clone
     (Element : Terminate_Alternative_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Terminate_Alternative_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------
   -- Abort_Statement_Node --
   --------------------------

   type Abort_Statement_Node is 
      new Statement_Node with private;

   type Abort_Statement_Ptr is
      access all Abort_Statement_Node;
   for Abort_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Abort_Statement_Node
     (The_Context : ASIS.Context)
      return Abort_Statement_Ptr;

   function Aborted_Tasks
     (Element : Abort_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Aborted_Tasks
     (Element : in out Abort_Statement_Node;
      Value   : in     Asis.Element);

   function Aborted_Tasks_List
     (Element : Abort_Statement_Node) return Asis.Element;

   function Statement_Kind (Element : Abort_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access Abort_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : Abort_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Abort_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------
   -- Raise_Statement_Node --
   --------------------------

   type Raise_Statement_Node is 
      new Statement_Node with private;

   type Raise_Statement_Ptr is
      access all Raise_Statement_Node;
   for Raise_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Raise_Statement_Node
     (The_Context : ASIS.Context)
      return Raise_Statement_Ptr;

   function Raised_Exception
     (Element : Raise_Statement_Node) return Asis.Expression;

   procedure Set_Raised_Exception
     (Element : in out Raise_Statement_Node;
      Value   : in     Asis.Expression);

   function Raise_Statement_Message
     (Element : Raise_Statement_Node) return Asis.Expression;

   procedure Set_Raise_Statement_Message
     (Element : in out Raise_Statement_Node;
      Value   : in     Asis.Expression);

   function Statement_Kind (Element : Raise_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access Raise_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : Raise_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Raise_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------
   -- Code_Statement_Node --
   -------------------------

   type Code_Statement_Node is 
      new Statement_Node with private;

   type Code_Statement_Ptr is
      access all Code_Statement_Node;
   for Code_Statement_Ptr'Storage_Pool use Lists.Pool;

   function New_Code_Statement_Node
     (The_Context : ASIS.Context)
      return Code_Statement_Ptr;

   function Qualified_Expression
     (Element : Code_Statement_Node) return Asis.Expression;

   procedure Set_Qualified_Expression
     (Element : in out Code_Statement_Node;
      Value   : in     Asis.Expression);

   function Statement_Kind (Element : Code_Statement_Node)
      return Asis.Statement_Kinds;

   function Children (Element : access Code_Statement_Node)
     return Traverse_List;

   function Clone
     (Element : Code_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Code_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

private


   type Base_Path_Statement_Node is abstract
      new Statement_Node with
      record
         Statement_Paths                : aliased Primary_Path_Lists.List;
      end record;


   type If_Statement_Node is 
      new Base_Path_Statement_Node with
      record
         null;
      end record;


   type Case_Statement_Node is 
      new Base_Path_Statement_Node with
      record
         Case_Expression                : aliased Asis.Expression;
      end record;


   type Selective_Accept_Statement_Node is 
      new Base_Path_Statement_Node with
      record
         null;
      end record;


   type Timed_Entry_Call_Statement_Node is 
      new Base_Path_Statement_Node with
      record
         null;
      end record;


   type Conditional_Entry_Call_Statement_Node is 
      new Base_Path_Statement_Node with
      record
         null;
      end record;


   type Asynchronous_Select_Statement_Node is 
      new Base_Path_Statement_Node with
      record
         null;
      end record;


   type Null_Statement_Node is 
      new Statement_Node with
      record
         null;
      end record;


   type Assignment_Statement_Node is 
      new Statement_Node with
      record
         Assignment_Variable_Name       : aliased Asis.Expression;
         Assignment_Expression          : aliased Asis.Expression;
      end record;


   type Loop_Statement_Node is 
      new Statement_Node with
      record
         Statement_Identifier           : aliased Asis.Defining_Name;
         Back_Identifier                : aliased Asis.Identifier;
         Is_Name_Repeated               : aliased Boolean := False;
         Loop_Statements                : aliased Primary_Statement_Lists.List;
      end record;


   type While_Loop_Statement_Node is 
      new Loop_Statement_Node with
      record
         While_Condition                : aliased Asis.Expression;
      end record;


   type For_Loop_Statement_Node is 
      new Loop_Statement_Node with
      record
         Loop_Parameter_Specification   : aliased Asis.Declaration;
      end record;


   type Block_Statement_Node is 
      new Statement_Node with
      record
         Is_Name_Repeated               : aliased Boolean := False;
         Is_Declare_Block               : aliased Boolean := False;
         Block_Declarative_Items        : aliased Primary_Declaration_Lists.List;
         Block_Statements               : aliased Primary_Statement_Lists.List;
         Block_Exception_Handlers       : aliased Primary_Handler_Lists.List;
         Statement_Identifier           : aliased Asis.Defining_Name;
         Back_Identifier                : aliased Asis.Identifier;
         Handled_Statements             : aliased Asis.Element;
      end record;


   type Exit_Statement_Node is 
      new Statement_Node with
      record
         Exit_Loop_Name                 : aliased Asis.Expression;
         Corresponding_Loop_Exited      : aliased Asis.Statement;
         Exit_Condition                 : aliased Asis.Expression;
      end record;


   type Goto_Statement_Node is 
      new Statement_Node with
      record
         Goto_Label                     : aliased Asis.Expression;
         Corresponding_Destination_Statement : aliased Asis.Statement;
      end record;


   type Base_Call_Statement_Node is abstract
      new Statement_Node with
      record
         Called_Name                    : aliased Asis.Expression;
         Corresponding_Called_Entity    : aliased Asis.Declaration;
         Call_Statement_Parameters      : aliased Primary_Association_Lists.List;
         Normalized_Call_Statement_Parameters : aliased Secondary_Association_Lists.List_Node;
      end record;


   type Procedure_Call_Statement_Node is 
      new Base_Call_Statement_Node with
      record
         Is_Call_On_Dispatching_Operation : aliased Boolean := False;
         Is_Dispatching_Call            : aliased Boolean := False;
      end record;


   type Entry_Call_Statement_Node is 
      new Base_Call_Statement_Node with
      record
         null;
      end record;


   type Simple_Return_Statement_Node is 
      new Statement_Node with
      record
         Return_Expression              : aliased Asis.Expression;
      end record;


   type Extended_Return_Statement_Node is 
      new Statement_Node with
      record
         Return_Object_Specification    : aliased Asis.Declaration;
         Extended_Return_Statements     : aliased Primary_Statement_Lists.List;
         Extended_Return_Exception_Handlers : aliased Primary_Handler_Lists.List;
         Handled_Statements             : aliased Asis.Element;
      end record;


   type Accept_Statement_Node is 
      new Statement_Node with
      record
         Accept_Entry_Index             : aliased Asis.Expression;
         Accept_Entry_Direct_Name       : aliased Asis.Name;
         Accept_Parameters              : aliased Primary_Parameter_Lists.List;
         Accept_Body_Statements         : aliased Primary_Statement_Lists.List;
         Accept_Body_Exception_Handlers : aliased Primary_Handler_Lists.List;
         Corresponding_Entry            : aliased Asis.Declaration;
         Is_Name_Repeated               : aliased Boolean := False;
         Handled_Statements             : aliased Asis.Element;
      end record;


   type Requeue_Statement_Node is 
      new Statement_Node with
      record
         Requeue_Entry_Name             : aliased Asis.Name;
      end record;


   type Requeue_Statement_With_Abort_Node is 
      new Requeue_Statement_Node with
      record
         null;
      end record;


   type Delay_Until_Statement_Node is 
      new Statement_Node with
      record
         Delay_Expression               : aliased Asis.Expression;
      end record;


   type Delay_Relative_Statement_Node is 
      new Delay_Until_Statement_Node with
      record
         null;
      end record;


   type Terminate_Alternative_Statement_Node is 
      new Statement_Node with
      record
         null;
      end record;


   type Abort_Statement_Node is 
      new Statement_Node with
      record
         Aborted_Tasks                  : aliased Primary_Expression_Lists.List;
      end record;


   type Raise_Statement_Node is 
      new Statement_Node with
      record
         Raised_Exception               : aliased Asis.Expression;
         Raise_Statement_Message        : aliased Asis.Expression;
      end record;


   type Code_Statement_Node is 
      new Statement_Node with
      record
         Qualified_Expression           : aliased Asis.Expression;
      end record;

end Asis.Gela.Elements.Stmt;
