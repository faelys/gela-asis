
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
  
with Asis.Gela.Lists;                  use Asis.Gela.Lists;
with Asis.Gela.Compilations;
  
package Asis.Gela.Elements.Helpers is

   -----------------------
   -- Fake_Element_Node --
   -----------------------

   type Fake_Element_Node is abstract
      new Element_Node with private;

   type Fake_Element_Ptr is
      access all Fake_Element_Node;
   for Fake_Element_Ptr'Storage_Pool use Lists.Pool;

   function Start_Position
     (Element : Fake_Element_Node) return Asis.Text_Position;

   procedure Set_Start_Position
     (Element : in out Fake_Element_Node;
      Value   : in     Asis.Text_Position);

   ----------------
   -- Token_Node --
   ----------------

   type Token_Node is 
      new Fake_Element_Node with private;

   type Token_Ptr is
      access all Token_Node;
   for Token_Ptr'Storage_Pool use Lists.Pool;

   function New_Token_Node
     (The_Context : ASIS.Context)
      return Token_Ptr;

   function End_Position
     (Element : Token_Node) return Asis.Text_Position;

   procedure Set_End_Position
     (Element : in out Token_Node;
      Value   : in     Asis.Text_Position);

   function Raw_Image
     (Element : Token_Node) return Gela_String;

   procedure Set_Raw_Image
     (Element : in out Token_Node;
      Value   : in     Gela_String);

   function Clone
     (Element : Token_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   ----------------------------
   -- Private_Indicator_Node --
   ----------------------------

   type Private_Indicator_Node is 
      new Fake_Element_Node with private;

   type Private_Indicator_Ptr is
      access all Private_Indicator_Node;
   for Private_Indicator_Ptr'Storage_Pool use Lists.Pool;

   function New_Private_Indicator_Node
     (The_Context : ASIS.Context)
      return Private_Indicator_Ptr;

   function Next_Element
     (Element : Private_Indicator_Node) return Asis.Element;

   procedure Set_Next_Element
     (Element : in out Private_Indicator_Node;
      Value   : in     Asis.Element);

   function End_Position
     (Element : Private_Indicator_Node) return Asis.Text_Position;

   procedure Set_End_Position
     (Element : in out Private_Indicator_Node;
      Value   : in     Asis.Text_Position);

   function Clone
     (Element : Private_Indicator_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   -----------------------------
   -- Handled_Statements_Node --
   -----------------------------

   type Handled_Statements_Node is 
      new Fake_Element_Node with private;

   type Handled_Statements_Ptr is
      access all Handled_Statements_Node;
   for Handled_Statements_Ptr'Storage_Pool use Lists.Pool;

   function New_Handled_Statements_Node
     (The_Context : ASIS.Context)
      return Handled_Statements_Ptr;

   function Statements
     (Element : Handled_Statements_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Statements
     (Element : in out Handled_Statements_Node;
      Value   : in     Asis.Element);

   function Statements_List
     (Element : Handled_Statements_Node) return Asis.Element;

   function Exception_Handlers
     (Element : Handled_Statements_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Exception_Handlers
     (Element : in out Handled_Statements_Node;
      Value   : in     Asis.Element);

   function Exception_Handlers_List
     (Element : Handled_Statements_Node) return Asis.Element;

   function Get_Identifier
     (Element : Handled_Statements_Node) return Asis.Element;

   procedure Set_Identifier
     (Element : in out Handled_Statements_Node;
      Value   : in     Asis.Element);

   function End_Position
     (Element : Handled_Statements_Node) return Asis.Text_Position;

   procedure Set_End_Position
     (Element : in out Handled_Statements_Node;
      Value   : in     Asis.Text_Position);

   function Clone
     (Element : Handled_Statements_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   ---------------------------
   -- Function_Profile_Node --
   ---------------------------

   type Function_Profile_Node is 
      new Fake_Element_Node with private;

   type Function_Profile_Ptr is
      access all Function_Profile_Node;
   for Function_Profile_Ptr'Storage_Pool use Lists.Pool;

   function New_Function_Profile_Node
     (The_Context : ASIS.Context)
      return Function_Profile_Ptr;

   function Parameter_Profile
     (Element : Function_Profile_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Parameter_Profile
     (Element : in out Function_Profile_Node;
      Value   : in     Asis.Element);

   function Parameter_Profile_List
     (Element : Function_Profile_Node) return Asis.Element;

   function Result_Profile
     (Element : Function_Profile_Node) return Asis.Expression;

   procedure Set_Result_Profile
     (Element : in out Function_Profile_Node;
      Value   : in     Asis.Expression);

   function End_Position
     (Element : Function_Profile_Node) return Asis.Text_Position;

   procedure Set_End_Position
     (Element : in out Function_Profile_Node;
      Value   : in     Asis.Text_Position);

   function Clone
     (Element : Function_Profile_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   ----------------------------------
   -- Procedure_Specification_Node --
   ----------------------------------

   type Procedure_Specification_Node is 
      new Fake_Element_Node with private;

   type Procedure_Specification_Ptr is
      access all Procedure_Specification_Node;
   for Procedure_Specification_Ptr'Storage_Pool use Lists.Pool;

   function New_Procedure_Specification_Node
     (The_Context : ASIS.Context)
      return Procedure_Specification_Ptr;

   function Names
     (Element : Procedure_Specification_Node) return Asis.Element;

   procedure Set_Names
     (Element : in out Procedure_Specification_Node;
      Value   : in     Asis.Element);

   function Profile
     (Element : Procedure_Specification_Node) return Asis.Element;

   procedure Set_Profile
     (Element : in out Procedure_Specification_Node;
      Value   : in     Asis.Element);

   function End_Position
     (Element : Procedure_Specification_Node) return Asis.Text_Position;

   procedure Set_End_Position
     (Element : in out Procedure_Specification_Node;
      Value   : in     Asis.Text_Position);

   function Clone
     (Element : Procedure_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   ---------------------------------
   -- Function_Specification_Node --
   ---------------------------------

   type Function_Specification_Node is 
      new Procedure_Specification_Node with private;

   type Function_Specification_Ptr is
      access all Function_Specification_Node;
   for Function_Specification_Ptr'Storage_Pool use Lists.Pool;

   function New_Function_Specification_Node
     (The_Context : ASIS.Context)
      return Function_Specification_Ptr;

   function Clone
     (Element : Function_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   --------------------------------
   -- Package_Specification_Node --
   --------------------------------

   type Package_Specification_Node is 
      new Fake_Element_Node with private;

   type Package_Specification_Ptr is
      access all Package_Specification_Node;
   for Package_Specification_Ptr'Storage_Pool use Lists.Pool;

   function New_Package_Specification_Node
     (The_Context : ASIS.Context)
      return Package_Specification_Ptr;

   function Names
     (Element : Package_Specification_Node) return Asis.Element;

   procedure Set_Names
     (Element : in out Package_Specification_Node;
      Value   : in     Asis.Element);

   function Visible_Part_Declarative_Items
     (Element : Package_Specification_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Visible_Part_Declarative_Items
     (Element : in out Package_Specification_Node;
      Value   : in     Asis.Element);

   function Visible_Part_Declarative_Items_List
     (Element : Package_Specification_Node) return Asis.Element;

   function Private_Part_Declarative_Items
     (Element : Package_Specification_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Private_Part_Declarative_Items
     (Element : in out Package_Specification_Node;
      Value   : in     Asis.Element);

   function Private_Part_Declarative_Items_List
     (Element : Package_Specification_Node) return Asis.Element;

   function Compound_Name
     (Element : Package_Specification_Node) return Asis.Element;

   procedure Set_Compound_Name
     (Element : in out Package_Specification_Node;
      Value   : in     Asis.Element);

   function End_Position
     (Element : Package_Specification_Node) return Asis.Text_Position;

   procedure Set_End_Position
     (Element : in out Package_Specification_Node;
      Value   : in     Asis.Text_Position);

   function Clone
     (Element : Package_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element;

private


   type Fake_Element_Node is abstract
      new Element_Node with
      record
         Start_Position                 : aliased Asis.Text_Position;
      end record;


   type Token_Node is 
      new Fake_Element_Node with
      record
         End_Position                   : aliased Asis.Text_Position;
         Raw_Image                      : aliased Gela_String;
      end record;


   type Private_Indicator_Node is 
      new Fake_Element_Node with
      record
         Next_Element                   : aliased Asis.Element;
         End_Position                   : aliased Asis.Text_Position;
      end record;


   type Handled_Statements_Node is 
      new Fake_Element_Node with
      record
         Statements                     : aliased Primary_Statement_Lists.List;
         Exception_Handlers             : aliased Primary_Handler_Lists.List;
         Identifier                     : aliased Asis.Element;
         End_Position                   : aliased Asis.Text_Position;
      end record;


   type Function_Profile_Node is 
      new Fake_Element_Node with
      record
         Parameter_Profile              : aliased Primary_Parameter_Lists.List;
         Result_Profile                 : aliased Asis.Expression;
         End_Position                   : aliased Asis.Text_Position;
      end record;


   type Procedure_Specification_Node is 
      new Fake_Element_Node with
      record
         Names                          : aliased Asis.Element;
         Profile                        : aliased Asis.Element;
         End_Position                   : aliased Asis.Text_Position;
      end record;


   type Function_Specification_Node is 
      new Procedure_Specification_Node with
      record
         null;
      end record;


   type Package_Specification_Node is 
      new Fake_Element_Node with
      record
         Names                          : aliased Asis.Element;
         Visible_Part_Declarative_Items : aliased Primary_Declaration_Lists.List;
         Private_Part_Declarative_Items : aliased Primary_Declaration_Lists.List;
         Compound_Name                  : aliased Asis.Element;
         End_Position                   : aliased Asis.Text_Position;
      end record;

end Asis.Gela.Elements.Helpers;
