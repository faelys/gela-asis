
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

package body Asis.Gela.Elements.Helpers is

   function Start_Position
     (Element : Fake_Element_Node) return Asis.Text_Position is
   begin
      return Element.Start_Position;
   end Start_Position;

   procedure Set_Start_Position
     (Element : in out Fake_Element_Node;
      Value   : in     Asis.Text_Position) is
   begin
      Element.Start_Position := Value;
   end Set_Start_Position;

   function End_Position
     (Element : Token_Node) return Asis.Text_Position is
   begin
      return Element.End_Position;
   end End_Position;

   procedure Set_End_Position
     (Element : in out Token_Node;
      Value   : in     Asis.Text_Position) is
   begin
      Element.End_Position := Value;
   end Set_End_Position;

   function Raw_Image
     (Element : Token_Node) return Gela_String is
   begin
      return Element.Raw_Image;
   end Raw_Image;

   procedure Set_Raw_Image
     (Element : in out Token_Node;
      Value   : in     Gela_String) is
   begin
      Element.Raw_Image := Value;
   end Set_Raw_Image;

   function New_Token_Node
     (The_Context : ASIS.Context)
      return Token_Ptr
   is
      Result : Token_Ptr :=
       new Token_Node;
   begin

      return Result;
   end New_Token_Node;
  
   function Clone
     (Element : Token_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Token_Ptr := new Token_Node;
   begin
      Result.Start_Position := Element.Start_Position;
      Result.End_Position := Element.End_Position;
      Result.Raw_Image := Element.Raw_Image;
      return Asis.Element (Result);
   end Clone;

   function Next_Element
     (Element : Private_Indicator_Node) return Asis.Element is
   begin
      return Element.Next_Element;
   end Next_Element;

   procedure Set_Next_Element
     (Element : in out Private_Indicator_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Next_Element := Value;
   end Set_Next_Element;

   function End_Position
     (Element : Private_Indicator_Node) return Asis.Text_Position is
   begin
      return Element.End_Position;
   end End_Position;

   procedure Set_End_Position
     (Element : in out Private_Indicator_Node;
      Value   : in     Asis.Text_Position) is
   begin
      Element.End_Position := Value;
   end Set_End_Position;

   function New_Private_Indicator_Node
     (The_Context : ASIS.Context)
      return Private_Indicator_Ptr
   is
      Result : Private_Indicator_Ptr :=
       new Private_Indicator_Node;
   begin

      return Result;
   end New_Private_Indicator_Node;
  
   function Clone
     (Element : Private_Indicator_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Private_Indicator_Ptr := new Private_Indicator_Node;
   begin
      Result.Start_Position := Element.Start_Position;
      Result.End_Position := Element.End_Position;
      return Asis.Element (Result);
   end Clone;

   function Statements
     (Element : Handled_Statements_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Statement_Lists.To_Element_List
        (Element.Statements, Include_Pragmas);
   end Statements;

   procedure Set_Statements
     (Element : in out Handled_Statements_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Statements := Primary_Statement_Lists.List (Value);
   end Set_Statements;

   function Statements_List
     (Element : Handled_Statements_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Statements);
   end Statements_List;

   function Exception_Handlers
     (Element : Handled_Statements_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Handler_Lists.To_Element_List
        (Element.Exception_Handlers, Include_Pragmas);
   end Exception_Handlers;

   procedure Set_Exception_Handlers
     (Element : in out Handled_Statements_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Exception_Handlers := Primary_Handler_Lists.List (Value);
   end Set_Exception_Handlers;

   function Exception_Handlers_List
     (Element : Handled_Statements_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Exception_Handlers);
   end Exception_Handlers_List;

   function Get_Identifier
     (Element : Handled_Statements_Node) return Asis.Element is
   begin
      return Element.Identifier;
   end Get_Identifier;

   procedure Set_Identifier
     (Element : in out Handled_Statements_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Identifier := Value;
   end Set_Identifier;

   function End_Position
     (Element : Handled_Statements_Node) return Asis.Text_Position is
   begin
      return Element.End_Position;
   end End_Position;

   procedure Set_End_Position
     (Element : in out Handled_Statements_Node;
      Value   : in     Asis.Text_Position) is
   begin
      Element.End_Position := Value;
   end Set_End_Position;

   function New_Handled_Statements_Node
     (The_Context : ASIS.Context)
      return Handled_Statements_Ptr
   is
      Result : Handled_Statements_Ptr :=
       new Handled_Statements_Node;
   begin

      return Result;
   end New_Handled_Statements_Node;
  
   function Clone
     (Element : Handled_Statements_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Handled_Statements_Ptr := new Handled_Statements_Node;
   begin
      Result.Start_Position := Element.Start_Position;
      Result.Identifier := Element.Identifier;
      Result.End_Position := Element.End_Position;
      return Asis.Element (Result);
   end Clone;

   function Parameter_Profile
     (Element : Function_Profile_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Parameter_Lists.To_Element_List
        (Element.Parameter_Profile, Include_Pragmas);
   end Parameter_Profile;

   procedure Set_Parameter_Profile
     (Element : in out Function_Profile_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Parameter_Profile := Primary_Parameter_Lists.List (Value);
   end Set_Parameter_Profile;

   function Parameter_Profile_List
     (Element : Function_Profile_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Parameter_Profile);
   end Parameter_Profile_List;

   function Result_Profile
     (Element : Function_Profile_Node) return Asis.Expression is
   begin
      return Element.Result_Profile;
   end Result_Profile;

   procedure Set_Result_Profile
     (Element : in out Function_Profile_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Result_Profile := Value;
   end Set_Result_Profile;

   function End_Position
     (Element : Function_Profile_Node) return Asis.Text_Position is
   begin
      return Element.End_Position;
   end End_Position;

   procedure Set_End_Position
     (Element : in out Function_Profile_Node;
      Value   : in     Asis.Text_Position) is
   begin
      Element.End_Position := Value;
   end Set_End_Position;

   function New_Function_Profile_Node
     (The_Context : ASIS.Context)
      return Function_Profile_Ptr
   is
      Result : Function_Profile_Ptr :=
       new Function_Profile_Node;
   begin

      return Result;
   end New_Function_Profile_Node;
  
   function Clone
     (Element : Function_Profile_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Function_Profile_Ptr := new Function_Profile_Node;
   begin
      Result.Start_Position := Element.Start_Position;
      Result.Result_Profile := Element.Result_Profile;
      Result.End_Position := Element.End_Position;
      return Asis.Element (Result);
   end Clone;

   function Names
     (Element : Procedure_Specification_Node) return Asis.Element is
   begin
      return Element.Names;
   end Names;

   procedure Set_Names
     (Element : in out Procedure_Specification_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Names := Value;
   end Set_Names;

   function Profile
     (Element : Procedure_Specification_Node) return Asis.Element is
   begin
      return Element.Profile;
   end Profile;

   procedure Set_Profile
     (Element : in out Procedure_Specification_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Profile := Value;
   end Set_Profile;

   function End_Position
     (Element : Procedure_Specification_Node) return Asis.Text_Position is
   begin
      return Element.End_Position;
   end End_Position;

   procedure Set_End_Position
     (Element : in out Procedure_Specification_Node;
      Value   : in     Asis.Text_Position) is
   begin
      Element.End_Position := Value;
   end Set_End_Position;

   function New_Procedure_Specification_Node
     (The_Context : ASIS.Context)
      return Procedure_Specification_Ptr
   is
      Result : Procedure_Specification_Ptr :=
       new Procedure_Specification_Node;
   begin

      return Result;
   end New_Procedure_Specification_Node;
  
   function Clone
     (Element : Procedure_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Procedure_Specification_Ptr := new Procedure_Specification_Node;
   begin
      Result.Start_Position := Element.Start_Position;
      Result.Names := Element.Names;
      Result.Profile := Element.Profile;
      Result.End_Position := Element.End_Position;
      return Asis.Element (Result);
   end Clone;

   function New_Function_Specification_Node
     (The_Context : ASIS.Context)
      return Function_Specification_Ptr
   is
      Result : Function_Specification_Ptr :=
       new Function_Specification_Node;
   begin

      return Result;
   end New_Function_Specification_Node;
  
   function Clone
     (Element : Function_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Function_Specification_Ptr := new Function_Specification_Node;
   begin
      Result.Start_Position := Element.Start_Position;
      Result.Names := Element.Names;
      Result.Profile := Element.Profile;
      Result.End_Position := Element.End_Position;
      return Asis.Element (Result);
   end Clone;

   function Names
     (Element : Package_Specification_Node) return Asis.Element is
   begin
      return Element.Names;
   end Names;

   procedure Set_Names
     (Element : in out Package_Specification_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Names := Value;
   end Set_Names;

   function Visible_Part_Declarative_Items
     (Element : Package_Specification_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Visible_Part_Declarative_Items, Include_Pragmas);
   end Visible_Part_Declarative_Items;

   procedure Set_Visible_Part_Declarative_Items
     (Element : in out Package_Specification_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Visible_Part_Declarative_Items := Primary_Declaration_Lists.List (Value);
   end Set_Visible_Part_Declarative_Items;

   function Visible_Part_Declarative_Items_List
     (Element : Package_Specification_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Visible_Part_Declarative_Items);
   end Visible_Part_Declarative_Items_List;

   function Private_Part_Declarative_Items
     (Element : Package_Specification_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Private_Part_Declarative_Items, Include_Pragmas);
   end Private_Part_Declarative_Items;

   procedure Set_Private_Part_Declarative_Items
     (Element : in out Package_Specification_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Private_Part_Declarative_Items := Primary_Declaration_Lists.List (Value);
   end Set_Private_Part_Declarative_Items;

   function Private_Part_Declarative_Items_List
     (Element : Package_Specification_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Private_Part_Declarative_Items);
   end Private_Part_Declarative_Items_List;

   function Compound_Name
     (Element : Package_Specification_Node) return Asis.Element is
   begin
      return Element.Compound_Name;
   end Compound_Name;

   procedure Set_Compound_Name
     (Element : in out Package_Specification_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Compound_Name := Value;
   end Set_Compound_Name;

   function End_Position
     (Element : Package_Specification_Node) return Asis.Text_Position is
   begin
      return Element.End_Position;
   end End_Position;

   procedure Set_End_Position
     (Element : in out Package_Specification_Node;
      Value   : in     Asis.Text_Position) is
   begin
      Element.End_Position := Value;
   end Set_End_Position;

   function New_Package_Specification_Node
     (The_Context : ASIS.Context)
      return Package_Specification_Ptr
   is
      Result : Package_Specification_Ptr :=
       new Package_Specification_Node;
   begin

      return Result;
   end New_Package_Specification_Node;
  
   function Clone
     (Element : Package_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Package_Specification_Ptr := new Package_Specification_Node;
   begin
      Result.Start_Position := Element.Start_Position;
      Result.Names := Element.Names;
      Result.Compound_Name := Element.Compound_Name;
      Result.End_Position := Element.End_Position;
      return Asis.Element (Result);
   end Clone;

end Asis.Gela.Elements.Helpers;
