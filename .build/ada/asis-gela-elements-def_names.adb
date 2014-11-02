
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

package body Asis.Gela.Elements.Def_Names is

   function New_Defining_Identifier_Node
     (The_Context : ASIS.Context)
      return Defining_Identifier_Ptr
   is
      Result : Defining_Identifier_Ptr :=
       new Defining_Identifier_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Defining_Identifier_Node;
  
   function Defining_Name_Kind (Element : Defining_Identifier_Node)
      return Asis.Defining_Name_Kinds is
   begin
      return A_Defining_Identifier;
   end;

   function Clone
     (Element : Defining_Identifier_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Defining_Identifier_Ptr := new Defining_Identifier_Node;
   begin
      Result.Enclosing_Element := Parent;
      Result.Is_Part_Of_Implicit := Element.Is_Part_Of_Implicit;
      Result.Is_Part_Of_Inherited := Element.Is_Part_Of_Inherited;
      Result.Is_Part_Of_Instance := Element.Is_Part_Of_Instance;
      Result.Start_Position := Element.Start_Position;
      Result.End_Position := Element.End_Position;
      Result.Enclosing_Compilation_Unit :=
        Enclosing_Compilation_Unit (Parent.all);
      Result.Hash := Element.Hash;
      Result.Defining_Name_Image := Element.Defining_Name_Image;
      Result.Corresponding_Constant_Declaration := Element.Corresponding_Constant_Declaration;
      null;
      Result.Corresponding_Generic_Element := Element.Corresponding_Generic_Element;
      Result.Override := Element.Override;
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   function Position_Number_Image
     (Element : Defining_Enumeration_Literal_Node) return Wide_String is
   begin
      return W.To_Wide_String (Element.Position_Number_Image);
   end Position_Number_Image;

   procedure Set_Position_Number_Image
     (Element : in out Defining_Enumeration_Literal_Node;
      Value   : in     Wide_String) is
   begin
      Element.Position_Number_Image := W.To_Unbounded_Wide_String (Value);
   end Set_Position_Number_Image;

   function Representation_Value_Image
     (Element : Defining_Enumeration_Literal_Node) return Wide_String is
   begin
      return W.To_Wide_String (Element.Representation_Value_Image);
   end Representation_Value_Image;

   procedure Set_Representation_Value_Image
     (Element : in out Defining_Enumeration_Literal_Node;
      Value   : in     Wide_String) is
   begin
      Element.Representation_Value_Image := W.To_Unbounded_Wide_String (Value);
   end Set_Representation_Value_Image;

   function New_Defining_Enumeration_Literal_Node
     (The_Context : ASIS.Context)
      return Defining_Enumeration_Literal_Ptr
   is
      Result : Defining_Enumeration_Literal_Ptr :=
       new Defining_Enumeration_Literal_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Defining_Enumeration_Literal_Node;
  
   function Defining_Name_Kind (Element : Defining_Enumeration_Literal_Node)
      return Asis.Defining_Name_Kinds is
   begin
      return A_Defining_Enumeration_Literal;
   end;

   function Clone
     (Element : Defining_Enumeration_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Defining_Enumeration_Literal_Ptr := new Defining_Enumeration_Literal_Node;
   begin
      Result.Enclosing_Element := Parent;
      Result.Is_Part_Of_Implicit := Element.Is_Part_Of_Implicit;
      Result.Is_Part_Of_Inherited := Element.Is_Part_Of_Inherited;
      Result.Is_Part_Of_Instance := Element.Is_Part_Of_Instance;
      Result.Start_Position := Element.Start_Position;
      Result.End_Position := Element.End_Position;
      Result.Enclosing_Compilation_Unit :=
        Enclosing_Compilation_Unit (Parent.all);
      Result.Hash := Element.Hash;
      Result.Defining_Name_Image := Element.Defining_Name_Image;
      Result.Corresponding_Constant_Declaration := Element.Corresponding_Constant_Declaration;
      null;
      Result.Corresponding_Generic_Element := Element.Corresponding_Generic_Element;
      Result.Override := Element.Override;
      Result.Place := Element.Place;
      Result.Position_Number_Image := Element.Position_Number_Image;
      Result.Representation_Value_Image := Element.Representation_Value_Image;
      return Asis.Element (Result);
   end Clone;

   function New_Defining_Character_Literal_Node
     (The_Context : ASIS.Context)
      return Defining_Character_Literal_Ptr
   is
      Result : Defining_Character_Literal_Ptr :=
       new Defining_Character_Literal_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Defining_Character_Literal_Node;
  
   function Defining_Name_Kind (Element : Defining_Character_Literal_Node)
      return Asis.Defining_Name_Kinds is
   begin
      return A_Defining_Character_Literal;
   end;

   function Clone
     (Element : Defining_Character_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Defining_Character_Literal_Ptr := new Defining_Character_Literal_Node;
   begin
      Result.Enclosing_Element := Parent;
      Result.Is_Part_Of_Implicit := Element.Is_Part_Of_Implicit;
      Result.Is_Part_Of_Inherited := Element.Is_Part_Of_Inherited;
      Result.Is_Part_Of_Instance := Element.Is_Part_Of_Instance;
      Result.Start_Position := Element.Start_Position;
      Result.End_Position := Element.End_Position;
      Result.Enclosing_Compilation_Unit :=
        Enclosing_Compilation_Unit (Parent.all);
      Result.Hash := Element.Hash;
      Result.Defining_Name_Image := Element.Defining_Name_Image;
      Result.Corresponding_Constant_Declaration := Element.Corresponding_Constant_Declaration;
      null;
      Result.Corresponding_Generic_Element := Element.Corresponding_Generic_Element;
      Result.Override := Element.Override;
      Result.Place := Element.Place;
      Result.Position_Number_Image := Element.Position_Number_Image;
      Result.Representation_Value_Image := Element.Representation_Value_Image;
      return Asis.Element (Result);
   end Clone;

   function Operator_Kind
     (Element : Defining_Operator_Symbol_Node) return Asis.Operator_Kinds is
   begin
      return Element.Operator_Kind;
   end Operator_Kind;

   procedure Set_Operator_Kind
     (Element : in out Defining_Operator_Symbol_Node;
      Value   : in     Asis.Operator_Kinds) is
   begin
      Element.Operator_Kind := Value;
   end Set_Operator_Kind;

   function New_Defining_Operator_Symbol_Node
     (The_Context : ASIS.Context)
      return Defining_Operator_Symbol_Ptr
   is
      Result : Defining_Operator_Symbol_Ptr :=
       new Defining_Operator_Symbol_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Defining_Operator_Symbol_Node;
  
   function Defining_Name_Kind (Element : Defining_Operator_Symbol_Node)
      return Asis.Defining_Name_Kinds is
   begin
      return A_Defining_Operator_Symbol;
   end;

   function Clone
     (Element : Defining_Operator_Symbol_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Defining_Operator_Symbol_Ptr := new Defining_Operator_Symbol_Node;
   begin
      Result.Enclosing_Element := Parent;
      Result.Is_Part_Of_Implicit := Element.Is_Part_Of_Implicit;
      Result.Is_Part_Of_Inherited := Element.Is_Part_Of_Inherited;
      Result.Is_Part_Of_Instance := Element.Is_Part_Of_Instance;
      Result.Start_Position := Element.Start_Position;
      Result.End_Position := Element.End_Position;
      Result.Enclosing_Compilation_Unit :=
        Enclosing_Compilation_Unit (Parent.all);
      Result.Hash := Element.Hash;
      Result.Defining_Name_Image := Element.Defining_Name_Image;
      Result.Corresponding_Constant_Declaration := Element.Corresponding_Constant_Declaration;
      null;
      Result.Corresponding_Generic_Element := Element.Corresponding_Generic_Element;
      Result.Override := Element.Override;
      Result.Place := Element.Place;
      Result.Operator_Kind := Element.Operator_Kind;
      return Asis.Element (Result);
   end Clone;

   function Defining_Prefix
     (Element : Defining_Expanded_Name_Node) return Asis.Name is
   begin
      return Element.Defining_Prefix;
   end Defining_Prefix;

   procedure Set_Defining_Prefix
     (Element : in out Defining_Expanded_Name_Node;
      Value   : in     Asis.Name) is
   begin
      Element.Defining_Prefix := Value;
   end Set_Defining_Prefix;

   function Defining_Selector
     (Element : Defining_Expanded_Name_Node) return Asis.Defining_Name is
   begin
      return Element.Defining_Selector;
   end Defining_Selector;

   procedure Set_Defining_Selector
     (Element : in out Defining_Expanded_Name_Node;
      Value   : in     Asis.Defining_Name) is
   begin
      Element.Defining_Selector := Value;
   end Set_Defining_Selector;

   function New_Defining_Expanded_Name_Node
     (The_Context : ASIS.Context)
      return Defining_Expanded_Name_Ptr
   is
      Result : Defining_Expanded_Name_Ptr :=
       new Defining_Expanded_Name_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Defining_Expanded_Name_Node;
  
   function Defining_Name_Kind (Element : Defining_Expanded_Name_Node)
      return Asis.Defining_Name_Kinds is
   begin
      return A_Defining_Expanded_Name;
   end;

   function Children (Element : access Defining_Expanded_Name_Node)
     return Traverse_List is
   begin
      return ((False, Element.Defining_Prefix'Access),
        (False, Element.Defining_Selector'Access));
   end Children;

   function Clone
     (Element : Defining_Expanded_Name_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Defining_Expanded_Name_Ptr := new Defining_Expanded_Name_Node;
   begin
      Result.Enclosing_Element := Parent;
      Result.Is_Part_Of_Implicit := Element.Is_Part_Of_Implicit;
      Result.Is_Part_Of_Inherited := Element.Is_Part_Of_Inherited;
      Result.Is_Part_Of_Instance := Element.Is_Part_Of_Instance;
      Result.Start_Position := Element.Start_Position;
      Result.End_Position := Element.End_Position;
      Result.Enclosing_Compilation_Unit :=
        Enclosing_Compilation_Unit (Parent.all);
      Result.Hash := Element.Hash;
      Result.Defining_Name_Image := Element.Defining_Name_Image;
      Result.Corresponding_Constant_Declaration := Element.Corresponding_Constant_Declaration;
      null;
      Result.Corresponding_Generic_Element := Element.Corresponding_Generic_Element;
      Result.Override := Element.Override;
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Defining_Expanded_Name_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Defining_Prefix :=
        Copy (Cloner, Defining_Prefix (Source.all), Asis.Element (Target));
      Target.Defining_Selector :=
        Copy (Cloner, Defining_Selector (Source.all), Asis.Element (Target));
   end Copy;

end Asis.Gela.Elements.Def_Names;
