
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

package body Asis.Gela.Elements.Assoc is

   function Formal_Parameter
     (Element : Pragma_Argument_Association_Node) return Asis.Identifier is
   begin
      return Element.Formal_Parameter;
   end Formal_Parameter;

   procedure Set_Formal_Parameter
     (Element : in out Pragma_Argument_Association_Node;
      Value   : in     Asis.Identifier) is
   begin
      Element.Formal_Parameter := Value;
   end Set_Formal_Parameter;

   function Actual_Parameter
     (Element : Pragma_Argument_Association_Node) return Asis.Expression is
   begin
      return Element.Actual_Parameter;
   end Actual_Parameter;

   procedure Set_Actual_Parameter
     (Element : in out Pragma_Argument_Association_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Actual_Parameter := Value;
   end Set_Actual_Parameter;

   function New_Pragma_Argument_Association_Node
     (The_Context : ASIS.Context)
      return Pragma_Argument_Association_Ptr
   is
      Result : Pragma_Argument_Association_Ptr :=
       new Pragma_Argument_Association_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Pragma_Argument_Association_Node;
  
   function Association_Kind (Element : Pragma_Argument_Association_Node)
      return Asis.Association_Kinds is
   begin
      return A_Pragma_Argument_Association;
   end;

   function Children (Element : access Pragma_Argument_Association_Node)
     return Traverse_List is
   begin
      return ((False, Element.Formal_Parameter'Access),
        (False, Element.Actual_Parameter'Access));
   end Children;

   function Clone
     (Element : Pragma_Argument_Association_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Pragma_Argument_Association_Ptr := new Pragma_Argument_Association_Node;
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
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Pragma_Argument_Association_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Formal_Parameter :=
        Copy (Cloner, Formal_Parameter (Source.all), Asis.Element (Target));
      Target.Actual_Parameter :=
        Copy (Cloner, Actual_Parameter (Source.all), Asis.Element (Target));
   end Copy;

   function Is_Normalized
     (Element : Parameter_Association_Node) return Boolean is
   begin
      return Element.Is_Normalized;
   end Is_Normalized;

   procedure Set_Is_Normalized
     (Element : in out Parameter_Association_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Normalized := Value;
   end Set_Is_Normalized;

   function Is_Defaulted_Association
     (Element : Parameter_Association_Node) return Boolean is
   begin
      return Element.Is_Defaulted_Association;
   end Is_Defaulted_Association;

   procedure Set_Is_Defaulted_Association
     (Element : in out Parameter_Association_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Defaulted_Association := Value;
   end Set_Is_Defaulted_Association;

   function New_Parameter_Association_Node
     (The_Context : ASIS.Context)
      return Parameter_Association_Ptr
   is
      Result : Parameter_Association_Ptr :=
       new Parameter_Association_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Parameter_Association_Node;
  
   function Association_Kind (Element : Parameter_Association_Node)
      return Asis.Association_Kinds is
   begin
      return A_Parameter_Association;
   end;

   function Clone
     (Element : Parameter_Association_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Parameter_Association_Ptr := new Parameter_Association_Node;
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
      Result.Is_Normalized := Element.Is_Normalized;
      Result.Is_Defaulted_Association := Element.Is_Defaulted_Association;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Parameter_Association_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Formal_Parameter :=
        Copy (Cloner, Formal_Parameter (Source.all), Asis.Element (Target));
      Target.Actual_Parameter :=
        Copy (Cloner, Actual_Parameter (Source.all), Asis.Element (Target));
   end Copy;

   function New_Generic_Association_Node
     (The_Context : ASIS.Context)
      return Generic_Association_Ptr
   is
      Result : Generic_Association_Ptr :=
       new Generic_Association_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Generic_Association_Node;
  
   function Association_Kind (Element : Generic_Association_Node)
      return Asis.Association_Kinds is
   begin
      return A_Generic_Association;
   end;

   function Clone
     (Element : Generic_Association_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Generic_Association_Ptr := new Generic_Association_Node;
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
      Result.Is_Normalized := Element.Is_Normalized;
      Result.Is_Defaulted_Association := Element.Is_Defaulted_Association;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Generic_Association_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Formal_Parameter :=
        Copy (Cloner, Formal_Parameter (Source.all), Asis.Element (Target));
      Target.Actual_Parameter :=
        Copy (Cloner, Actual_Parameter (Source.all), Asis.Element (Target));
   end Copy;

   function Is_Normalized
     (Element : Discriminant_Association_Node) return Boolean is
   begin
      return Element.Is_Normalized;
   end Is_Normalized;

   procedure Set_Is_Normalized
     (Element : in out Discriminant_Association_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Normalized := Value;
   end Set_Is_Normalized;

   function Discriminant_Expression
     (Element : Discriminant_Association_Node) return Asis.Expression is
   begin
      return Element.Discriminant_Expression;
   end Discriminant_Expression;

   procedure Set_Discriminant_Expression
     (Element : in out Discriminant_Association_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Discriminant_Expression := Value;
   end Set_Discriminant_Expression;

   function Discriminant_Selector_Name
     (Element : Discriminant_Association_Node) return Asis.Expression is
   begin
      return Element.Discriminant_Selector_Name;
   end Discriminant_Selector_Name;

   procedure Set_Discriminant_Selector_Name
     (Element : in out Discriminant_Association_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Discriminant_Selector_Name := Value;
   end Set_Discriminant_Selector_Name;

   function Discriminant_Selector_Names
     (Element : Discriminant_Association_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Choise_Lists.To_Element_List
        (Element.Discriminant_Selector_Names, Include_Pragmas);
   end Discriminant_Selector_Names;

   procedure Set_Discriminant_Selector_Names
     (Element : in out Discriminant_Association_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Discriminant_Selector_Names := Primary_Choise_Lists.List (Value);
   end Set_Discriminant_Selector_Names;

   function Discriminant_Selector_Names_List
     (Element : Discriminant_Association_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Discriminant_Selector_Names);
   end Discriminant_Selector_Names_List;

   function New_Discriminant_Association_Node
     (The_Context : ASIS.Context)
      return Discriminant_Association_Ptr
   is
      Result : Discriminant_Association_Ptr :=
       new Discriminant_Association_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Discriminant_Association_Node;
  
   function Association_Kind (Element : Discriminant_Association_Node)
      return Asis.Association_Kinds is
   begin
      return A_Discriminant_Association;
   end;

   function Children (Element : access Discriminant_Association_Node)
     return Traverse_List is
   begin
      if Element.Is_Normalized then
         return ((False, Element.Discriminant_Selector_Name'Access),
           (False, Element.Discriminant_Expression'Access));
      
      end if;
      return ((True, Asis.Element (Element.Discriminant_Selector_Names)),
        (False, Element.Discriminant_Expression'Access));
   end Children;

   function Clone
     (Element : Discriminant_Association_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Discriminant_Association_Ptr := new Discriminant_Association_Node;
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
      Result.Is_Normalized := Element.Is_Normalized;
      Result.Discriminant_Selector_Name := Element.Discriminant_Selector_Name;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Discriminant_Association_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Discriminant_Selector_Names
        (Target.all,
         Primary_Choise_Lists.Deep_Copy 
           (Discriminant_Selector_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Discriminant_Expression :=
        Copy (Cloner, Discriminant_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function Is_Normalized
     (Element : Record_Component_Association_Node) return Boolean is
   begin
      return Element.Is_Normalized;
   end Is_Normalized;

   procedure Set_Is_Normalized
     (Element : in out Record_Component_Association_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Normalized := Value;
   end Set_Is_Normalized;

   function Component_Expression
     (Element : Record_Component_Association_Node) return Asis.Expression is
   begin
      return Element.Component_Expression;
   end Component_Expression;

   procedure Set_Component_Expression
     (Element : in out Record_Component_Association_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Component_Expression := Value;
   end Set_Component_Expression;

   function Record_Component_Choice
     (Element : Record_Component_Association_Node) return Asis.Defining_Name is
   begin
      return Element.Record_Component_Choice;
   end Record_Component_Choice;

   procedure Set_Record_Component_Choice
     (Element : in out Record_Component_Association_Node;
      Value   : in     Asis.Defining_Name) is
   begin
      Element.Record_Component_Choice := Value;
   end Set_Record_Component_Choice;

   function Record_Component_Choices
     (Element : Record_Component_Association_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Choise_Lists.To_Element_List
        (Element.Record_Component_Choices, Include_Pragmas);
   end Record_Component_Choices;

   procedure Set_Record_Component_Choices
     (Element : in out Record_Component_Association_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Record_Component_Choices := Primary_Choise_Lists.List (Value);
   end Set_Record_Component_Choices;

   function Record_Component_Choices_List
     (Element : Record_Component_Association_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Record_Component_Choices);
   end Record_Component_Choices_List;

   function New_Record_Component_Association_Node
     (The_Context : ASIS.Context)
      return Record_Component_Association_Ptr
   is
      Result : Record_Component_Association_Ptr :=
       new Record_Component_Association_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Record_Component_Association_Node;
  
   function Association_Kind (Element : Record_Component_Association_Node)
      return Asis.Association_Kinds is
   begin
      return A_Record_Component_Association;
   end;

   function Children (Element : access Record_Component_Association_Node)
     return Traverse_List is
   begin
      if Element.Is_Normalized then
         return ((False, Element.Record_Component_Choice'Access),
           (False, Element.Component_Expression'Access));
      
      end if;
      return ((True, Asis.Element (Element.Record_Component_Choices)),
        (False, Element.Component_Expression'Access));
   end Children;

   function Clone
     (Element : Record_Component_Association_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Record_Component_Association_Ptr := new Record_Component_Association_Node;
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
      Result.Is_Normalized := Element.Is_Normalized;
      Result.Record_Component_Choice := Element.Record_Component_Choice;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Record_Component_Association_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Record_Component_Choices
        (Target.all,
         Primary_Choise_Lists.Deep_Copy 
           (Record_Component_Choices (Source.all), Cloner, Asis.Element (Target)));
      Target.Component_Expression :=
        Copy (Cloner, Component_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function Array_Component_Choices
     (Element : Array_Component_Association_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Choise_Lists.To_Element_List
        (Element.Array_Component_Choices, Include_Pragmas);
   end Array_Component_Choices;

   procedure Set_Array_Component_Choices
     (Element : in out Array_Component_Association_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Array_Component_Choices := Primary_Choise_Lists.List (Value);
   end Set_Array_Component_Choices;

   function Array_Component_Choices_List
     (Element : Array_Component_Association_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Array_Component_Choices);
   end Array_Component_Choices_List;

   function Component_Expression
     (Element : Array_Component_Association_Node) return Asis.Expression is
   begin
      return Element.Component_Expression;
   end Component_Expression;

   procedure Set_Component_Expression
     (Element : in out Array_Component_Association_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Component_Expression := Value;
   end Set_Component_Expression;

   function New_Array_Component_Association_Node
     (The_Context : ASIS.Context)
      return Array_Component_Association_Ptr
   is
      Result : Array_Component_Association_Ptr :=
       new Array_Component_Association_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Array_Component_Association_Node;
  
   function Association_Kind (Element : Array_Component_Association_Node)
      return Asis.Association_Kinds is
   begin
      return An_Array_Component_Association;
   end;

   function Children (Element : access Array_Component_Association_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Array_Component_Choices)),
        (False, Element.Component_Expression'Access));
   end Children;

   function Clone
     (Element : Array_Component_Association_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Array_Component_Association_Ptr := new Array_Component_Association_Node;
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
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Array_Component_Association_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Array_Component_Choices
        (Target.all,
         Primary_Choise_Lists.Deep_Copy 
           (Array_Component_Choices (Source.all), Cloner, Asis.Element (Target)));
      Target.Component_Expression :=
        Copy (Cloner, Component_Expression (Source.all), Asis.Element (Target));
   end Copy;

end Asis.Gela.Elements.Assoc;
