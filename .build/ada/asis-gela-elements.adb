
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

package body Asis.Gela.Elements is

   function Enclosing_Element
     (Element : Base_Element_Node) return Asis.Element is
   begin
      return Element.Enclosing_Element;
   end Enclosing_Element;

   procedure Set_Enclosing_Element
     (Element : in out Base_Element_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Enclosing_Element := Value;
   end Set_Enclosing_Element;

   function Next_Element
     (Element : Base_Element_Node) return Asis.Element is
   begin
      return Element.Next_Element;
   end Next_Element;

   procedure Set_Next_Element
     (Element : in out Base_Element_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Next_Element := Value;
   end Set_Next_Element;

   function Is_Part_Of_Implicit
     (Element : Base_Element_Node) return Boolean is
   begin
      return Element.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   procedure Set_Is_Part_Of_Implicit
     (Element : in out Base_Element_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Part_Of_Implicit := Value;
   end Set_Is_Part_Of_Implicit;

   function Is_Part_Of_Inherited
     (Element : Base_Element_Node) return Boolean is
   begin
      return Element.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   procedure Set_Is_Part_Of_Inherited
     (Element : in out Base_Element_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Part_Of_Inherited := Value;
   end Set_Is_Part_Of_Inherited;

   function Is_Part_Of_Instance
     (Element : Base_Element_Node) return Boolean is
   begin
      return Element.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Set_Is_Part_Of_Instance
     (Element : in out Base_Element_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Part_Of_Instance := Value;
   end Set_Is_Part_Of_Instance;

   function Start_Position
     (Element : Base_Element_Node) return Asis.Text_Position is
   begin
      return Element.Start_Position;
   end Start_Position;

   procedure Set_Start_Position
     (Element : in out Base_Element_Node;
      Value   : in     Asis.Text_Position) is
   begin
      Element.Start_Position := Value;
   end Set_Start_Position;

   function End_Position
     (Element : Base_Element_Node) return Asis.Text_Position is
   begin
      return Element.End_Position;
   end End_Position;

   procedure Set_End_Position
     (Element : in out Base_Element_Node;
      Value   : in     Asis.Text_Position) is
   begin
      Element.End_Position := Value;
   end Set_End_Position;

   function Enclosing_Compilation_Unit
     (Element : Base_Element_Node) return Asis.Compilation_Unit is
   begin
      return Element.Enclosing_Compilation_Unit;
   end Enclosing_Compilation_Unit;

   procedure Set_Enclosing_Compilation_Unit
     (Element : in out Base_Element_Node;
      Value   : in     Asis.Compilation_Unit) is
   begin
      Element.Enclosing_Compilation_Unit := Value;
   end Set_Enclosing_Compilation_Unit;

   function Hash
     (Element : Base_Element_Node) return Asis.ASIS_Integer is
   begin
      return Element.Hash;
   end Hash;

   procedure Set_Hash
     (Element : in out Base_Element_Node;
      Value   : in     Asis.ASIS_Integer) is
   begin
      Element.Hash := Value;
   end Set_Hash;

   function Pragma_Kind
     (Element : Pragma_Node) return Asis.Pragma_Kinds is
   begin
      return Element.Pragma_Kind;
   end Pragma_Kind;

   procedure Set_Pragma_Kind
     (Element : in out Pragma_Node;
      Value   : in     Asis.Pragma_Kinds) is
   begin
      Element.Pragma_Kind := Value;
   end Set_Pragma_Kind;

   function Pragma_Name_Image
     (Element : Pragma_Node) return Wide_String is
   begin
      return W.To_Wide_String (Element.Pragma_Name_Image);
   end Pragma_Name_Image;

   procedure Set_Pragma_Name_Image
     (Element : in out Pragma_Node;
      Value   : in     Wide_String) is
   begin
      Element.Pragma_Name_Image := W.To_Unbounded_Wide_String (Value);
   end Set_Pragma_Name_Image;

   function Pragma_Argument_Associations
     (Element : Pragma_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Association_Lists.To_Element_List
        (Element.Pragma_Argument_Associations, Include_Pragmas);
   end Pragma_Argument_Associations;

   procedure Set_Pragma_Argument_Associations
     (Element : in out Pragma_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Pragma_Argument_Associations := Primary_Association_Lists.List (Value);
   end Set_Pragma_Argument_Associations;

   function Pragma_Argument_Associations_List
     (Element : Pragma_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Pragma_Argument_Associations);
   end Pragma_Argument_Associations_List;

   function New_Pragma_Node
     (The_Context : ASIS.Context)
      return Pragma_Ptr
   is
      Result : Pragma_Ptr :=
       new Pragma_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Pragma_Node;
  
   function Element_Kind (Element : Pragma_Node)
      return Asis.Element_Kinds is
   begin
      return A_Pragma;
   end;

   function Children (Element : access Pragma_Node)
     return Traverse_List is
   begin
      return (1 => (True, Asis.Element (Element.Pragma_Argument_Associations)));
   end Children;

   function Clone
     (Element : Pragma_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Pragma_Ptr := new Pragma_Node;
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
      Result.Pragma_Kind := Element.Pragma_Kind;
      Result.Pragma_Name_Image := Element.Pragma_Name_Image;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Pragma_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Pragma_Argument_Associations
        (Target.all,
         Primary_Association_Lists.Deep_Copy 
           (Pragma_Argument_Associations (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Defining_Name_Image
     (Element : Defining_Name_Node) return Wide_String is
   begin
      return W.To_Wide_String (Element.Defining_Name_Image);
   end Defining_Name_Image;

   procedure Set_Defining_Name_Image
     (Element : in out Defining_Name_Node;
      Value   : in     Wide_String) is
   begin
      Element.Defining_Name_Image := W.To_Unbounded_Wide_String (Value);
   end Set_Defining_Name_Image;

   function Corresponding_Constant_Declaration
     (Element : Defining_Name_Node) return Asis.Element is
   begin
      return Element.Corresponding_Constant_Declaration;
   end Corresponding_Constant_Declaration;

   procedure Set_Corresponding_Constant_Declaration
     (Element : in out Defining_Name_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Corresponding_Constant_Declaration := Value;
   end Set_Corresponding_Constant_Declaration;

   function References
     (Element : Defining_Name_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Reference_Lists.To_Element_List
        (Element.References, Include_Pragmas);
   end References;

   procedure Add_To_References
     (Element : in out Defining_Name_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Reference_Lists.Add (Element.References, Item);
   end Add_To_References;

   function Corresponding_Generic_Element
     (Element : Defining_Name_Node) return Asis.Defining_Name is
   begin
      return Element.Corresponding_Generic_Element;
   end Corresponding_Generic_Element;

   procedure Set_Corresponding_Generic_Element
     (Element : in out Defining_Name_Node;
      Value   : in     Asis.Defining_Name) is
   begin
      Element.Corresponding_Generic_Element := Value;
   end Set_Corresponding_Generic_Element;

   function Override
     (Element : Defining_Name_Node) return Asis.Defining_Name is
   begin
      return Element.Override;
   end Override;

   procedure Set_Override
     (Element : in out Defining_Name_Node;
      Value   : in     Asis.Defining_Name) is
   begin
      Element.Override := Value;
   end Set_Override;

   function Place
     (Element : Defining_Name_Node) return Visibility.Region_Item_Access is
   begin
      return Element.Place;
   end Place;

   procedure Set_Place
     (Element : in out Defining_Name_Node;
      Value   : in     Visibility.Region_Item_Access) is
   begin
      Element.Place := Value;
   end Set_Place;

   function Element_Kind (Element : Defining_Name_Node)
      return Asis.Element_Kinds is
   begin
      return A_Defining_Name;
   end;

   function Declaration_Origin
     (Element : Declaration_Node) return Asis.Declaration_Origins is
   begin
      return Element.Declaration_Origin;
   end Declaration_Origin;

   procedure Set_Declaration_Origin
     (Element : in out Declaration_Node;
      Value   : in     Asis.Declaration_Origins) is
   begin
      Element.Declaration_Origin := Value;
   end Set_Declaration_Origin;

   function Name
     (Element : Declaration_Node) return Asis.Element is
   begin
      return Element.Name;
   end Name;

   procedure Set_Name
     (Element : in out Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Name := Value;
   end Set_Name;

   function Names
     (Element : Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Defining_Name_Lists.To_Element_List
        (Element.Names, Include_Pragmas);
   end Names;

   procedure Set_Names
     (Element : in out Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Names := Primary_Defining_Name_Lists.List (Value);
   end Set_Names;

   function Names_List
     (Element : Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Names);
   end Names_List;

   function Corresponding_Representation_Clauses
     (Element : Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Clause_Lists.To_Element_List
        (Element.Corresponding_Representation_Clauses, Include_Pragmas);
   end Corresponding_Representation_Clauses;

   procedure Add_To_Corresponding_Representation_Clauses
     (Element : in out Declaration_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Clause_Lists.Add (Element.Corresponding_Representation_Clauses, Item);
   end Add_To_Corresponding_Representation_Clauses;

   function Corresponding_Pragmas
     (Element : Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Pragma_Lists.To_Element_List
        (Element.Corresponding_Pragmas, Include_Pragmas);
   end Corresponding_Pragmas;

   procedure Add_To_Corresponding_Pragmas
     (Element : in out Declaration_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Pragma_Lists.Add (Element.Corresponding_Pragmas, Item);
   end Add_To_Corresponding_Pragmas;

   function Place
     (Element : Declaration_Node) return Visibility.Region_Item_Access is
   begin
      return Element.Place;
   end Place;

   procedure Set_Place
     (Element : in out Declaration_Node;
      Value   : in     Visibility.Region_Item_Access) is
   begin
      Element.Place := Value;
   end Set_Place;

   function Element_Kind (Element : Declaration_Node)
      return Asis.Element_Kinds is
   begin
      return A_Declaration;
   end;

   function Children (Element : access Declaration_Node)
     return Traverse_List is
   begin
      return (1 => (True, Asis.Element (Element.Names)));
   end Children;

   function Element_Kind (Element : Definition_Node)
      return Asis.Element_Kinds is
   begin
      return A_Definition;
   end;

   function Corresponding_Expression_Type
     (Element : Expression_Node) return Asis.Element is
   begin
      return Element.Corresponding_Expression_Type;
   end Corresponding_Expression_Type;

   procedure Set_Corresponding_Expression_Type
     (Element : in out Expression_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Corresponding_Expression_Type := Value;
   end Set_Corresponding_Expression_Type;

   function Is_Static_Expression
     (Element : Expression_Node) return Asis.Gela.Fuzzy_Boolean is
   begin
      return Element.Is_Static_Expression;
   end Is_Static_Expression;

   procedure Set_Is_Static_Expression
     (Element : in out Expression_Node;
      Value   : in     Asis.Gela.Fuzzy_Boolean) is
   begin
      Element.Is_Static_Expression := Value;
   end Set_Is_Static_Expression;

   function Element_Kind (Element : Expression_Node)
      return Asis.Element_Kinds is
   begin
      return An_Expression;
   end;

   function Element_Kind (Element : Association_Node)
      return Asis.Element_Kinds is
   begin
      return An_Association;
   end;

   function Label_Names
     (Element : Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Defining_Name_Lists.To_Element_List
        (Element.Label_Names, Include_Pragmas);
   end Label_Names;

   procedure Set_Label_Names
     (Element : in out Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Label_Names := Primary_Defining_Name_Lists.List (Value);
   end Set_Label_Names;

   function Label_Names_List
     (Element : Statement_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Label_Names);
   end Label_Names_List;

   function Corresponding_Pragmas
     (Element : Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Pragma_Lists.To_Element_List
        (Element.Corresponding_Pragmas, Include_Pragmas);
   end Corresponding_Pragmas;

   procedure Add_To_Corresponding_Pragmas
     (Element : in out Statement_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Pragma_Lists.Add (Element.Corresponding_Pragmas, Item);
   end Add_To_Corresponding_Pragmas;

   function Place
     (Element : Statement_Node) return Visibility.Region_Item_Access is
   begin
      return Element.Place;
   end Place;

   procedure Set_Place
     (Element : in out Statement_Node;
      Value   : in     Visibility.Region_Item_Access) is
   begin
      Element.Place := Value;
   end Set_Place;

   function Element_Kind (Element : Statement_Node)
      return Asis.Element_Kinds is
   begin
      return A_Statement;
   end;

   function Children (Element : access Statement_Node)
     return Traverse_List is
   begin
      return (1 => (True, Asis.Element (Element.Label_Names)));
   end Children;

   function Sequence_Of_Statements
     (Element : Path_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Statement_Lists.To_Element_List
        (Element.Sequence_Of_Statements, Include_Pragmas);
   end Sequence_Of_Statements;

   procedure Set_Sequence_Of_Statements
     (Element : in out Path_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Sequence_Of_Statements := Primary_Statement_Lists.List (Value);
   end Set_Sequence_Of_Statements;

   function Sequence_Of_Statements_List
     (Element : Path_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Sequence_Of_Statements);
   end Sequence_Of_Statements_List;

   function Element_Kind (Element : Path_Node)
      return Asis.Element_Kinds is
   begin
      return A_Path;
   end;

   function Children (Element : access Path_Node)
     return Traverse_List is
   begin
      return (1 => (True, Asis.Element (Element.Sequence_Of_Statements)));
   end Children;

   function Place
     (Element : Clause_Node) return Visibility.Region_Item_Access is
   begin
      return Element.Place;
   end Place;

   procedure Set_Place
     (Element : in out Clause_Node;
      Value   : in     Visibility.Region_Item_Access) is
   begin
      Element.Place := Value;
   end Set_Place;

   function Element_Kind (Element : Clause_Node)
      return Asis.Element_Kinds is
   begin
      return A_Clause;
   end;

   function Choice_Parameter_Specification
     (Element : Exception_Handler_Node) return Asis.Element is
   begin
      return Element.Choice_Parameter_Specification;
   end Choice_Parameter_Specification;

   procedure Set_Choice_Parameter_Specification
     (Element : in out Exception_Handler_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Choice_Parameter_Specification := Value;
   end Set_Choice_Parameter_Specification;

   function Exception_Choices
     (Element : Exception_Handler_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Choise_Lists.To_Element_List
        (Element.Exception_Choices, Include_Pragmas);
   end Exception_Choices;

   procedure Set_Exception_Choices
     (Element : in out Exception_Handler_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Exception_Choices := Primary_Choise_Lists.List (Value);
   end Set_Exception_Choices;

   function Exception_Choices_List
     (Element : Exception_Handler_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Exception_Choices);
   end Exception_Choices_List;

   function Handler_Statements
     (Element : Exception_Handler_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Statement_Lists.To_Element_List
        (Element.Handler_Statements, Include_Pragmas);
   end Handler_Statements;

   procedure Set_Handler_Statements
     (Element : in out Exception_Handler_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Handler_Statements := Primary_Statement_Lists.List (Value);
   end Set_Handler_Statements;

   function Handler_Statements_List
     (Element : Exception_Handler_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Handler_Statements);
   end Handler_Statements_List;

   function Pragmas
     (Element : Exception_Handler_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Pragma_Lists.To_Element_List
        (Element.Pragmas, Include_Pragmas);
   end Pragmas;

   procedure Set_Pragmas
     (Element : in out Exception_Handler_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Pragmas := Primary_Pragma_Lists.List (Value);
   end Set_Pragmas;

   function Pragmas_List
     (Element : Exception_Handler_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Pragmas);
   end Pragmas_List;

   function Place
     (Element : Exception_Handler_Node) return Visibility.Region_Item_Access is
   begin
      return Element.Place;
   end Place;

   procedure Set_Place
     (Element : in out Exception_Handler_Node;
      Value   : in     Visibility.Region_Item_Access) is
   begin
      Element.Place := Value;
   end Set_Place;

   function New_Exception_Handler_Node
     (The_Context : ASIS.Context)
      return Exception_Handler_Ptr
   is
      Result : Exception_Handler_Ptr :=
       new Exception_Handler_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Exception_Handler_Node;
  
   function Element_Kind (Element : Exception_Handler_Node)
      return Asis.Element_Kinds is
   begin
      return An_Exception_Handler;
   end;

   function Children (Element : access Exception_Handler_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Pragmas)),
        (False, Element.Choice_Parameter_Specification'Access),
        (True, Asis.Element (Element.Exception_Choices)),
        (True, Asis.Element (Element.Handler_Statements)));
   end Children;

   function Clone
     (Element : Exception_Handler_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Exception_Handler_Ptr := new Exception_Handler_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Exception_Handler_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Pragmas
        (Target.all,
         Primary_Pragma_Lists.Deep_Copy 
           (Pragmas (Source.all), Cloner, Asis.Element (Target)));
      Target.Choice_Parameter_Specification :=
        Copy (Cloner, Choice_Parameter_Specification (Source.all), Asis.Element (Target));
      Set_Exception_Choices
        (Target.all,
         Primary_Choise_Lists.Deep_Copy 
           (Exception_Choices (Source.all), Cloner, Asis.Element (Target)));
      Set_Handler_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Handler_Statements (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

end Asis.Gela.Elements;
