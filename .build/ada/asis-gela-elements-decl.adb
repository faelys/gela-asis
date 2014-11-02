
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

package body Asis.Gela.Elements.Decl is

   function Discriminant_Part
     (Element : Base_Type_Declaration_Node) return Asis.Definition is
   begin
      return Element.Discriminant_Part;
   end Discriminant_Part;

   procedure Set_Discriminant_Part
     (Element : in out Base_Type_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Discriminant_Part := Value;
   end Set_Discriminant_Part;

   function Type_Declaration_View
     (Element : Base_Type_Declaration_Node) return Asis.Definition is
   begin
      return Element.Type_Declaration_View;
   end Type_Declaration_View;

   procedure Set_Type_Declaration_View
     (Element : in out Base_Type_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Type_Declaration_View := Value;
   end Set_Type_Declaration_View;

   function Children (Element : access Base_Type_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Discriminant_Part'Access),
        (False, Element.Type_Declaration_View'Access));
   end Children;

   function Corresponding_Type_Declaration
     (Element : Ordinary_Type_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Type_Declaration;
   end Corresponding_Type_Declaration;

   procedure Set_Corresponding_Type_Declaration
     (Element : in out Ordinary_Type_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Type_Declaration := Value;
   end Set_Corresponding_Type_Declaration;

   function New_Ordinary_Type_Declaration_Node
     (The_Context : ASIS.Context)
      return Ordinary_Type_Declaration_Ptr
   is
      Result : Ordinary_Type_Declaration_Ptr :=
       new Ordinary_Type_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Ordinary_Type_Declaration_Node;
  
   function Declaration_Kind (Element : Ordinary_Type_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return An_Ordinary_Type_Declaration;
   end;

   function Clone
     (Element : Ordinary_Type_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Ordinary_Type_Declaration_Ptr := new Ordinary_Type_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Type_Declaration := Element.Corresponding_Type_Declaration;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Ordinary_Type_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Discriminant_Part :=
        Copy (Cloner, Discriminant_Part (Source.all), Asis.Element (Target));
      Target.Type_Declaration_View :=
        Copy (Cloner, Type_Declaration_View (Source.all), Asis.Element (Target));
   end Copy;

   function Is_Name_Repeated
     (Element : Protected_Type_Declaration_Node) return Boolean is
   begin
      return Element.Is_Name_Repeated;
   end Is_Name_Repeated;

   procedure Set_Is_Name_Repeated
     (Element : in out Protected_Type_Declaration_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Name_Repeated := Value;
   end Set_Is_Name_Repeated;

   function Corresponding_Body
     (Element : Protected_Type_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Body;
   end Corresponding_Body;

   procedure Set_Corresponding_Body
     (Element : in out Protected_Type_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Body := Value;
   end Set_Corresponding_Body;

   function Progenitor_List
     (Element : Protected_Type_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Expression_Lists.To_Element_List
        (Element.Progenitor_List, Include_Pragmas);
   end Progenitor_List;

   procedure Set_Progenitor_List
     (Element : in out Protected_Type_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Progenitor_List := Primary_Expression_Lists.List (Value);
   end Set_Progenitor_List;

   function Progenitor_List_List
     (Element : Protected_Type_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Progenitor_List);
   end Progenitor_List_List;

   function New_Protected_Type_Declaration_Node
     (The_Context : ASIS.Context)
      return Protected_Type_Declaration_Ptr
   is
      Result : Protected_Type_Declaration_Ptr :=
       new Protected_Type_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Protected_Type_Declaration_Node;
  
   function Declaration_Kind (Element : Protected_Type_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Protected_Type_Declaration;
   end;

   function Children (Element : access Protected_Type_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Discriminant_Part'Access),
        (True, Asis.Element (Element.Progenitor_List)),
        (False, Element.Type_Declaration_View'Access));
   end Children;

   function Clone
     (Element : Protected_Type_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Protected_Type_Declaration_Ptr := new Protected_Type_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Type_Declaration := Element.Corresponding_Type_Declaration;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      Result.Corresponding_Body := Element.Corresponding_Body;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Protected_Type_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Discriminant_Part :=
        Copy (Cloner, Discriminant_Part (Source.all), Asis.Element (Target));
      Set_Progenitor_List
        (Target.all,
         Primary_Expression_Lists.Deep_Copy 
           (Progenitor_List (Source.all), Cloner, Asis.Element (Target)));
      Target.Type_Declaration_View :=
        Copy (Cloner, Type_Declaration_View (Source.all), Asis.Element (Target));
   end Copy;

   function New_Task_Type_Declaration_Node
     (The_Context : ASIS.Context)
      return Task_Type_Declaration_Ptr
   is
      Result : Task_Type_Declaration_Ptr :=
       new Task_Type_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Task_Type_Declaration_Node;
  
   function Declaration_Kind (Element : Task_Type_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Task_Type_Declaration;
   end;

   function Clone
     (Element : Task_Type_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Task_Type_Declaration_Ptr := new Task_Type_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Type_Declaration := Element.Corresponding_Type_Declaration;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      Result.Corresponding_Body := Element.Corresponding_Body;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Task_Type_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Discriminant_Part :=
        Copy (Cloner, Discriminant_Part (Source.all), Asis.Element (Target));
      Set_Progenitor_List
        (Target.all,
         Primary_Expression_Lists.Deep_Copy 
           (Progenitor_List (Source.all), Cloner, Asis.Element (Target)));
      Target.Type_Declaration_View :=
        Copy (Cloner, Type_Declaration_View (Source.all), Asis.Element (Target));
   end Copy;

   function Trait_Kind
     (Element : Private_Type_Declaration_Node) return Asis.Trait_Kinds is
   begin
      return Element.Trait_Kind;
   end Trait_Kind;

   procedure Set_Trait_Kind
     (Element : in out Private_Type_Declaration_Node;
      Value   : in     Asis.Trait_Kinds) is
   begin
      Element.Trait_Kind := Value;
   end Set_Trait_Kind;

   function New_Private_Type_Declaration_Node
     (The_Context : ASIS.Context)
      return Private_Type_Declaration_Ptr
   is
      Result : Private_Type_Declaration_Ptr :=
       new Private_Type_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Private_Type_Declaration_Node;
  
   function Declaration_Kind (Element : Private_Type_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Private_Type_Declaration;
   end;

   function Clone
     (Element : Private_Type_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Private_Type_Declaration_Ptr := new Private_Type_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Type_Declaration := Element.Corresponding_Type_Declaration;
      Result.Trait_Kind := Element.Trait_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Private_Type_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Discriminant_Part :=
        Copy (Cloner, Discriminant_Part (Source.all), Asis.Element (Target));
      Target.Type_Declaration_View :=
        Copy (Cloner, Type_Declaration_View (Source.all), Asis.Element (Target));
   end Copy;

   function Progenitor_List
     (Element : Private_Extension_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Expression_Lists.To_Element_List
        (Element.Progenitor_List, Include_Pragmas);
   end Progenitor_List;

   procedure Set_Progenitor_List
     (Element : in out Private_Extension_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Progenitor_List := Primary_Expression_Lists.List (Value);
   end Set_Progenitor_List;

   function Progenitor_List_List
     (Element : Private_Extension_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Progenitor_List);
   end Progenitor_List_List;

   function New_Private_Extension_Declaration_Node
     (The_Context : ASIS.Context)
      return Private_Extension_Declaration_Ptr
   is
      Result : Private_Extension_Declaration_Ptr :=
       new Private_Extension_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Private_Extension_Declaration_Node;
  
   function Declaration_Kind (Element : Private_Extension_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Private_Extension_Declaration;
   end;

   function Children (Element : access Private_Extension_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Discriminant_Part'Access),
        (True, Asis.Element (Element.Progenitor_List)),
        (False, Element.Type_Declaration_View'Access));
   end Children;

   function Clone
     (Element : Private_Extension_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Private_Extension_Declaration_Ptr := new Private_Extension_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Type_Declaration := Element.Corresponding_Type_Declaration;
      Result.Trait_Kind := Element.Trait_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Private_Extension_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Discriminant_Part :=
        Copy (Cloner, Discriminant_Part (Source.all), Asis.Element (Target));
      Set_Progenitor_List
        (Target.all,
         Primary_Expression_Lists.Deep_Copy 
           (Progenitor_List (Source.all), Cloner, Asis.Element (Target)));
      Target.Type_Declaration_View :=
        Copy (Cloner, Type_Declaration_View (Source.all), Asis.Element (Target));
   end Copy;

   function Generic_Actual
     (Element : Formal_Type_Declaration_Node) return Asis.Expression is
   begin
      return Element.Generic_Actual;
   end Generic_Actual;

   procedure Set_Generic_Actual
     (Element : in out Formal_Type_Declaration_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Generic_Actual := Value;
   end Set_Generic_Actual;

   function New_Formal_Type_Declaration_Node
     (The_Context : ASIS.Context)
      return Formal_Type_Declaration_Ptr
   is
      Result : Formal_Type_Declaration_Ptr :=
       new Formal_Type_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Type_Declaration_Node;
  
   function Declaration_Kind (Element : Formal_Type_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Formal_Type_Declaration;
   end;

   function Clone
     (Element : Formal_Type_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Type_Declaration_Ptr := new Formal_Type_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Generic_Actual := Element.Generic_Actual;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Type_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Discriminant_Part :=
        Copy (Cloner, Discriminant_Part (Source.all), Asis.Element (Target));
      Target.Type_Declaration_View :=
        Copy (Cloner, Type_Declaration_View (Source.all), Asis.Element (Target));
   end Copy;

   function Parameter_Profile
     (Element : Base_Callable_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Parameter_Lists.To_Element_List
        (Element.Parameter_Profile, Include_Pragmas);
   end Parameter_Profile;

   procedure Set_Parameter_Profile
     (Element : in out Base_Callable_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Parameter_Profile := Primary_Parameter_Lists.List (Value);
   end Set_Parameter_Profile;

   function Parameter_Profile_List
     (Element : Base_Callable_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Parameter_Profile);
   end Parameter_Profile_List;

   function Corresponding_Body
     (Element : Base_Callable_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Body;
   end Corresponding_Body;

   procedure Set_Corresponding_Body
     (Element : in out Base_Callable_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Body := Value;
   end Set_Corresponding_Body;

   function Specification
     (Element : Base_Callable_Declaration_Node) return Asis.Element is
   begin
      return Element.Specification;
   end Specification;

   procedure Set_Specification
     (Element : in out Base_Callable_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Specification := Value;
   end Set_Specification;

   function Children (Element : access Base_Callable_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Parameter_Profile)));
   end Children;

   function Corresponding_Subprogram_Derivation
     (Element : Procedure_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Subprogram_Derivation;
   end Corresponding_Subprogram_Derivation;

   procedure Set_Corresponding_Subprogram_Derivation
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Subprogram_Derivation := Value;
   end Set_Corresponding_Subprogram_Derivation;

   function Corresponding_Type
     (Element : Procedure_Declaration_Node) return Asis.Type_Definition is
   begin
      return Element.Corresponding_Type;
   end Corresponding_Type;

   procedure Set_Corresponding_Type
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Asis.Type_Definition) is
   begin
      Element.Corresponding_Type := Value;
   end Set_Corresponding_Type;

   function Is_Dispatching_Operation
     (Element : Procedure_Declaration_Node) return Boolean is
   begin
      return Element.Is_Dispatching_Operation;
   end Is_Dispatching_Operation;

   procedure Set_Is_Dispatching_Operation
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Dispatching_Operation := Value;
   end Set_Is_Dispatching_Operation;

   function Trait_Kind
     (Element : Procedure_Declaration_Node) return Asis.Trait_Kinds is
   begin
      return Element.Trait_Kind;
   end Trait_Kind;

   procedure Set_Trait_Kind
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Asis.Trait_Kinds) is
   begin
      Element.Trait_Kind := Value;
   end Set_Trait_Kind;

   function Overriding_Indicator_Kind
     (Element : Procedure_Declaration_Node) return Asis.Overriding_Indicator_Kinds is
   begin
      return Element.Overriding_Indicator_Kind;
   end Overriding_Indicator_Kind;

   procedure Set_Overriding_Indicator_Kind
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Asis.Overriding_Indicator_Kinds) is
   begin
      Element.Overriding_Indicator_Kind := Value;
   end Set_Overriding_Indicator_Kind;

   function Has_Abstract
     (Element : Procedure_Declaration_Node) return Boolean is
   begin
      return Element.Has_Abstract;
   end Has_Abstract;

   procedure Set_Has_Abstract
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Abstract := Value;
   end Set_Has_Abstract;

   function Is_Null_Procedure
     (Element : Procedure_Declaration_Node) return Boolean is
   begin
      return Element.Is_Null_Procedure;
   end Is_Null_Procedure;

   procedure Set_Is_Null_Procedure
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Null_Procedure := Value;
   end Set_Is_Null_Procedure;

   function Generic_Formal_Part
     (Element : Procedure_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Generic_Formal_Part, Include_Pragmas);
   end Generic_Formal_Part;

   procedure Set_Generic_Formal_Part
     (Element : in out Procedure_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Generic_Formal_Part := Primary_Declaration_Lists.List (Value);
   end Set_Generic_Formal_Part;

   function Generic_Formal_Part_List
     (Element : Procedure_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Generic_Formal_Part);
   end Generic_Formal_Part_List;

   function New_Procedure_Declaration_Node
     (The_Context : ASIS.Context)
      return Procedure_Declaration_Ptr
   is
      Result : Procedure_Declaration_Ptr :=
       new Procedure_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Procedure_Declaration_Node;
  
   function Declaration_Kind (Element : Procedure_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Procedure_Declaration;
   end;

   function Children (Element : access Procedure_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Generic_Formal_Part)),
        (True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Parameter_Profile)));
   end Children;

   function Clone
     (Element : Procedure_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Procedure_Declaration_Ptr := new Procedure_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Body := Element.Corresponding_Body;
      Result.Specification := Element.Specification;
      Result.Corresponding_Subprogram_Derivation := Element.Corresponding_Subprogram_Derivation;
      Result.Corresponding_Type := Element.Corresponding_Type;
      Result.Is_Dispatching_Operation := Element.Is_Dispatching_Operation;
      Result.Trait_Kind := Element.Trait_Kind;
      Result.Overriding_Indicator_Kind := Element.Overriding_Indicator_Kind;
      Result.Has_Abstract := Element.Has_Abstract;
      Result.Is_Null_Procedure := Element.Is_Null_Procedure;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Procedure_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Generic_Formal_Part
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Generic_Formal_Part (Source.all), Cloner, Asis.Element (Target)));
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Result_Subtype
     (Element : Function_Declaration_Node) return Asis.Definition is
   begin
      return Element.Result_Subtype;
   end Result_Subtype;

   procedure Set_Result_Subtype
     (Element : in out Function_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Result_Subtype := Value;
   end Set_Result_Subtype;

   function Corresponding_Equality_Operator
     (Element : Function_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Equality_Operator;
   end Corresponding_Equality_Operator;

   procedure Set_Corresponding_Equality_Operator
     (Element : in out Function_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Equality_Operator := Value;
   end Set_Corresponding_Equality_Operator;

   function New_Function_Declaration_Node
     (The_Context : ASIS.Context)
      return Function_Declaration_Ptr
   is
      Result : Function_Declaration_Ptr :=
       new Function_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Function_Declaration_Node;
  
   function Declaration_Kind (Element : Function_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Function_Declaration;
   end;

   function Children (Element : access Function_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Generic_Formal_Part)),
        (True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Parameter_Profile)),
        (False, Element.Result_Subtype'Access));
   end Children;

   function Clone
     (Element : Function_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Function_Declaration_Ptr := new Function_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Body := Element.Corresponding_Body;
      Result.Specification := Element.Specification;
      Result.Corresponding_Subprogram_Derivation := Element.Corresponding_Subprogram_Derivation;
      Result.Corresponding_Type := Element.Corresponding_Type;
      Result.Is_Dispatching_Operation := Element.Is_Dispatching_Operation;
      Result.Trait_Kind := Element.Trait_Kind;
      Result.Overriding_Indicator_Kind := Element.Overriding_Indicator_Kind;
      Result.Has_Abstract := Element.Has_Abstract;
      Result.Is_Null_Procedure := Element.Is_Null_Procedure;
      Result.Corresponding_Equality_Operator := Element.Corresponding_Equality_Operator;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Function_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Generic_Formal_Part
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Generic_Formal_Part (Source.all), Cloner, Asis.Element (Target)));
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
      Target.Result_Subtype :=
        Copy (Cloner, Result_Subtype (Source.all), Asis.Element (Target));
   end Copy;

   function Is_Dispatching_Operation
     (Element : Procedure_Renaming_Declaration_Node) return Boolean is
   begin
      return Element.Is_Dispatching_Operation;
   end Is_Dispatching_Operation;

   procedure Set_Is_Dispatching_Operation
     (Element : in out Procedure_Renaming_Declaration_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Dispatching_Operation := Value;
   end Set_Is_Dispatching_Operation;

   function Renamed_Entity
     (Element : Procedure_Renaming_Declaration_Node) return Asis.Expression is
   begin
      return Element.Renamed_Entity;
   end Renamed_Entity;

   procedure Set_Renamed_Entity
     (Element : in out Procedure_Renaming_Declaration_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Renamed_Entity := Value;
   end Set_Renamed_Entity;

   function Corresponding_Base_Entity
     (Element : Procedure_Renaming_Declaration_Node) return Asis.Expression is
   begin
      return Element.Corresponding_Base_Entity;
   end Corresponding_Base_Entity;

   procedure Set_Corresponding_Base_Entity
     (Element : in out Procedure_Renaming_Declaration_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Corresponding_Base_Entity := Value;
   end Set_Corresponding_Base_Entity;

   function Corresponding_Declaration
     (Element : Procedure_Renaming_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Declaration;
   end Corresponding_Declaration;

   procedure Set_Corresponding_Declaration
     (Element : in out Procedure_Renaming_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Declaration := Value;
   end Set_Corresponding_Declaration;

   function Overriding_Indicator_Kind
     (Element : Procedure_Renaming_Declaration_Node) return Asis.Overriding_Indicator_Kinds is
   begin
      return Element.Overriding_Indicator_Kind;
   end Overriding_Indicator_Kind;

   procedure Set_Overriding_Indicator_Kind
     (Element : in out Procedure_Renaming_Declaration_Node;
      Value   : in     Asis.Overriding_Indicator_Kinds) is
   begin
      Element.Overriding_Indicator_Kind := Value;
   end Set_Overriding_Indicator_Kind;

   function New_Procedure_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Procedure_Renaming_Declaration_Ptr
   is
      Result : Procedure_Renaming_Declaration_Ptr :=
       new Procedure_Renaming_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Procedure_Renaming_Declaration_Node;
  
   function Declaration_Kind (Element : Procedure_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Procedure_Renaming_Declaration;
   end;

   function Children (Element : access Procedure_Renaming_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Parameter_Profile)),
        (False, Element.Renamed_Entity'Access));
   end Children;

   function Clone
     (Element : Procedure_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Procedure_Renaming_Declaration_Ptr := new Procedure_Renaming_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Body := Element.Corresponding_Body;
      Result.Specification := Element.Specification;
      Result.Is_Dispatching_Operation := Element.Is_Dispatching_Operation;
      Result.Corresponding_Base_Entity := Element.Corresponding_Base_Entity;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Overriding_Indicator_Kind := Element.Overriding_Indicator_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Procedure_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
      Target.Renamed_Entity :=
        Copy (Cloner, Renamed_Entity (Source.all), Asis.Element (Target));
   end Copy;

   function Result_Subtype
     (Element : Function_Renaming_Declaration_Node) return Asis.Definition is
   begin
      return Element.Result_Subtype;
   end Result_Subtype;

   procedure Set_Result_Subtype
     (Element : in out Function_Renaming_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Result_Subtype := Value;
   end Set_Result_Subtype;

   function Corresponding_Equality_Operator
     (Element : Function_Renaming_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Equality_Operator;
   end Corresponding_Equality_Operator;

   procedure Set_Corresponding_Equality_Operator
     (Element : in out Function_Renaming_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Equality_Operator := Value;
   end Set_Corresponding_Equality_Operator;

   function New_Function_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Function_Renaming_Declaration_Ptr
   is
      Result : Function_Renaming_Declaration_Ptr :=
       new Function_Renaming_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Function_Renaming_Declaration_Node;
  
   function Declaration_Kind (Element : Function_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Function_Renaming_Declaration;
   end;

   function Children (Element : access Function_Renaming_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Parameter_Profile)),
        (False, Element.Result_Subtype'Access),
        (False, Element.Renamed_Entity'Access));
   end Children;

   function Clone
     (Element : Function_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Function_Renaming_Declaration_Ptr := new Function_Renaming_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Body := Element.Corresponding_Body;
      Result.Specification := Element.Specification;
      Result.Is_Dispatching_Operation := Element.Is_Dispatching_Operation;
      Result.Corresponding_Base_Entity := Element.Corresponding_Base_Entity;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Overriding_Indicator_Kind := Element.Overriding_Indicator_Kind;
      Result.Corresponding_Equality_Operator := Element.Corresponding_Equality_Operator;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Function_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
      Target.Result_Subtype :=
        Copy (Cloner, Result_Subtype (Source.all), Asis.Element (Target));
      Target.Renamed_Entity :=
        Copy (Cloner, Renamed_Entity (Source.all), Asis.Element (Target));
   end Copy;

   function Entry_Family_Definition
     (Element : Entry_Declaration_Node) return Asis.Discrete_Subtype_Definition is
   begin
      return Element.Entry_Family_Definition;
   end Entry_Family_Definition;

   procedure Set_Entry_Family_Definition
     (Element : in out Entry_Declaration_Node;
      Value   : in     Asis.Discrete_Subtype_Definition) is
   begin
      Element.Entry_Family_Definition := Value;
   end Set_Entry_Family_Definition;

   function Overriding_Indicator_Kind
     (Element : Entry_Declaration_Node) return Asis.Overriding_Indicator_Kinds is
   begin
      return Element.Overriding_Indicator_Kind;
   end Overriding_Indicator_Kind;

   procedure Set_Overriding_Indicator_Kind
     (Element : in out Entry_Declaration_Node;
      Value   : in     Asis.Overriding_Indicator_Kinds) is
   begin
      Element.Overriding_Indicator_Kind := Value;
   end Set_Overriding_Indicator_Kind;

   function New_Entry_Declaration_Node
     (The_Context : ASIS.Context)
      return Entry_Declaration_Ptr
   is
      Result : Entry_Declaration_Ptr :=
       new Entry_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Entry_Declaration_Node;
  
   function Declaration_Kind (Element : Entry_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return An_Entry_Declaration;
   end;

   function Children (Element : access Entry_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Entry_Family_Definition'Access),
        (True, Asis.Element (Element.Parameter_Profile)));
   end Children;

   function Clone
     (Element : Entry_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Entry_Declaration_Ptr := new Entry_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Body := Element.Corresponding_Body;
      Result.Specification := Element.Specification;
      Result.Overriding_Indicator_Kind := Element.Overriding_Indicator_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Entry_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Entry_Family_Definition :=
        Copy (Cloner, Entry_Family_Definition (Source.all), Asis.Element (Target));
      Set_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Corresponding_Subunit
     (Element : Procedure_Body_Stub_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Subunit;
   end Corresponding_Subunit;

   procedure Set_Corresponding_Subunit
     (Element : in out Procedure_Body_Stub_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Subunit := Value;
   end Set_Corresponding_Subunit;

   function Corresponding_Declaration
     (Element : Procedure_Body_Stub_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Declaration;
   end Corresponding_Declaration;

   procedure Set_Corresponding_Declaration
     (Element : in out Procedure_Body_Stub_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Declaration := Value;
   end Set_Corresponding_Declaration;

   function Overriding_Indicator_Kind
     (Element : Procedure_Body_Stub_Node) return Asis.Overriding_Indicator_Kinds is
   begin
      return Element.Overriding_Indicator_Kind;
   end Overriding_Indicator_Kind;

   procedure Set_Overriding_Indicator_Kind
     (Element : in out Procedure_Body_Stub_Node;
      Value   : in     Asis.Overriding_Indicator_Kinds) is
   begin
      Element.Overriding_Indicator_Kind := Value;
   end Set_Overriding_Indicator_Kind;

   function New_Procedure_Body_Stub_Node
     (The_Context : ASIS.Context)
      return Procedure_Body_Stub_Ptr
   is
      Result : Procedure_Body_Stub_Ptr :=
       new Procedure_Body_Stub_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Procedure_Body_Stub_Node;
  
   function Declaration_Kind (Element : Procedure_Body_Stub_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Procedure_Body_Stub;
   end;

   function Clone
     (Element : Procedure_Body_Stub_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Procedure_Body_Stub_Ptr := new Procedure_Body_Stub_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Body := Element.Corresponding_Body;
      Result.Specification := Element.Specification;
      Result.Corresponding_Subunit := Element.Corresponding_Subunit;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Overriding_Indicator_Kind := Element.Overriding_Indicator_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Procedure_Body_Stub_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Result_Subtype
     (Element : Function_Body_Stub_Node) return Asis.Definition is
   begin
      return Element.Result_Subtype;
   end Result_Subtype;

   procedure Set_Result_Subtype
     (Element : in out Function_Body_Stub_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Result_Subtype := Value;
   end Set_Result_Subtype;

   function New_Function_Body_Stub_Node
     (The_Context : ASIS.Context)
      return Function_Body_Stub_Ptr
   is
      Result : Function_Body_Stub_Ptr :=
       new Function_Body_Stub_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Function_Body_Stub_Node;
  
   function Declaration_Kind (Element : Function_Body_Stub_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Function_Body_Stub;
   end;

   function Children (Element : access Function_Body_Stub_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Parameter_Profile)),
        (False, Element.Result_Subtype'Access));
   end Children;

   function Clone
     (Element : Function_Body_Stub_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Function_Body_Stub_Ptr := new Function_Body_Stub_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Body := Element.Corresponding_Body;
      Result.Specification := Element.Specification;
      Result.Corresponding_Subunit := Element.Corresponding_Subunit;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Overriding_Indicator_Kind := Element.Overriding_Indicator_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Function_Body_Stub_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
      Target.Result_Subtype :=
        Copy (Cloner, Result_Subtype (Source.all), Asis.Element (Target));
   end Copy;

   function Generic_Formal_Part
     (Element : Generic_Procedure_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Generic_Formal_Part, Include_Pragmas);
   end Generic_Formal_Part;

   procedure Set_Generic_Formal_Part
     (Element : in out Generic_Procedure_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Generic_Formal_Part := Primary_Declaration_Lists.List (Value);
   end Set_Generic_Formal_Part;

   function Generic_Formal_Part_List
     (Element : Generic_Procedure_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Generic_Formal_Part);
   end Generic_Formal_Part_List;

   function New_Generic_Procedure_Declaration_Node
     (The_Context : ASIS.Context)
      return Generic_Procedure_Declaration_Ptr
   is
      Result : Generic_Procedure_Declaration_Ptr :=
       new Generic_Procedure_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Generic_Procedure_Declaration_Node;
  
   function Declaration_Kind (Element : Generic_Procedure_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Generic_Procedure_Declaration;
   end;

   function Children (Element : access Generic_Procedure_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Generic_Formal_Part)),
        (True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Parameter_Profile)));
   end Children;

   function Clone
     (Element : Generic_Procedure_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Generic_Procedure_Declaration_Ptr := new Generic_Procedure_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Body := Element.Corresponding_Body;
      Result.Specification := Element.Specification;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Generic_Procedure_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Generic_Formal_Part
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Generic_Formal_Part (Source.all), Cloner, Asis.Element (Target)));
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Result_Subtype
     (Element : Generic_Function_Declaration_Node) return Asis.Definition is
   begin
      return Element.Result_Subtype;
   end Result_Subtype;

   procedure Set_Result_Subtype
     (Element : in out Generic_Function_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Result_Subtype := Value;
   end Set_Result_Subtype;

   function New_Generic_Function_Declaration_Node
     (The_Context : ASIS.Context)
      return Generic_Function_Declaration_Ptr
   is
      Result : Generic_Function_Declaration_Ptr :=
       new Generic_Function_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Generic_Function_Declaration_Node;
  
   function Declaration_Kind (Element : Generic_Function_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Generic_Function_Declaration;
   end;

   function Children (Element : access Generic_Function_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Generic_Formal_Part)),
        (True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Parameter_Profile)),
        (False, Element.Result_Subtype'Access));
   end Children;

   function Clone
     (Element : Generic_Function_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Generic_Function_Declaration_Ptr := new Generic_Function_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Body := Element.Corresponding_Body;
      Result.Specification := Element.Specification;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Generic_Function_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Generic_Formal_Part
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Generic_Formal_Part (Source.all), Cloner, Asis.Element (Target)));
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
      Target.Result_Subtype :=
        Copy (Cloner, Result_Subtype (Source.all), Asis.Element (Target));
   end Copy;

   function Body_Declarative_Items
     (Element : Base_Body_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Body_Declarative_Items, Include_Pragmas);
   end Body_Declarative_Items;

   procedure Set_Body_Declarative_Items
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Body_Declarative_Items := Primary_Declaration_Lists.List (Value);
   end Set_Body_Declarative_Items;

   function Body_Declarative_Items_List
     (Element : Base_Body_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Body_Declarative_Items);
   end Body_Declarative_Items_List;

   function Body_Statements
     (Element : Base_Body_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Statement_Lists.To_Element_List
        (Element.Body_Statements, Include_Pragmas);
   end Body_Statements;

   procedure Set_Body_Statements
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Body_Statements := Primary_Statement_Lists.List (Value);
   end Set_Body_Statements;

   function Body_Statements_List
     (Element : Base_Body_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Body_Statements);
   end Body_Statements_List;

   function Body_Exception_Handlers
     (Element : Base_Body_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Handler_Lists.To_Element_List
        (Element.Body_Exception_Handlers, Include_Pragmas);
   end Body_Exception_Handlers;

   procedure Set_Body_Exception_Handlers
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Body_Exception_Handlers := Primary_Handler_Lists.List (Value);
   end Set_Body_Exception_Handlers;

   function Body_Exception_Handlers_List
     (Element : Base_Body_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Body_Exception_Handlers);
   end Body_Exception_Handlers_List;

   function Corresponding_Declaration
     (Element : Base_Body_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Declaration;
   end Corresponding_Declaration;

   procedure Set_Corresponding_Declaration
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Declaration := Value;
   end Set_Corresponding_Declaration;

   function Corresponding_Body_Stub
     (Element : Base_Body_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Body_Stub;
   end Corresponding_Body_Stub;

   procedure Set_Corresponding_Body_Stub
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Body_Stub := Value;
   end Set_Corresponding_Body_Stub;

   function Is_Name_Repeated
     (Element : Base_Body_Declaration_Node) return Boolean is
   begin
      return Element.Is_Name_Repeated;
   end Is_Name_Repeated;

   procedure Set_Is_Name_Repeated
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Name_Repeated := Value;
   end Set_Is_Name_Repeated;

   function Handled_Statements
     (Element : Base_Body_Declaration_Node) return Asis.Element is
   begin
      return Element.Handled_Statements;
   end Handled_Statements;

   procedure Set_Handled_Statements
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Handled_Statements := Value;
   end Set_Handled_Statements;

   function Compound_Name
     (Element : Base_Body_Declaration_Node) return Asis.Element is
   begin
      return Element.Compound_Name;
   end Compound_Name;

   procedure Set_Compound_Name
     (Element : in out Base_Body_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Compound_Name := Value;
   end Set_Compound_Name;

   function Children (Element : access Base_Body_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Body_Declarative_Items)),
        (True, Asis.Element (Element.Body_Statements)),
        (True, Asis.Element (Element.Body_Exception_Handlers)));
   end Children;

   function Parameter_Profile
     (Element : Procedure_Body_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Parameter_Lists.To_Element_List
        (Element.Parameter_Profile, Include_Pragmas);
   end Parameter_Profile;

   procedure Set_Parameter_Profile
     (Element : in out Procedure_Body_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Parameter_Profile := Primary_Parameter_Lists.List (Value);
   end Set_Parameter_Profile;

   function Parameter_Profile_List
     (Element : Procedure_Body_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Parameter_Profile);
   end Parameter_Profile_List;

   function Specification
     (Element : Procedure_Body_Declaration_Node) return Asis.Element is
   begin
      return Element.Specification;
   end Specification;

   procedure Set_Specification
     (Element : in out Procedure_Body_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Specification := Value;
   end Set_Specification;

   function Overriding_Indicator_Kind
     (Element : Procedure_Body_Declaration_Node) return Asis.Overriding_Indicator_Kinds is
   begin
      return Element.Overriding_Indicator_Kind;
   end Overriding_Indicator_Kind;

   procedure Set_Overriding_Indicator_Kind
     (Element : in out Procedure_Body_Declaration_Node;
      Value   : in     Asis.Overriding_Indicator_Kinds) is
   begin
      Element.Overriding_Indicator_Kind := Value;
   end Set_Overriding_Indicator_Kind;

   function New_Procedure_Body_Declaration_Node
     (The_Context : ASIS.Context)
      return Procedure_Body_Declaration_Ptr
   is
      Result : Procedure_Body_Declaration_Ptr :=
       new Procedure_Body_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Procedure_Body_Declaration_Node;
  
   function Declaration_Kind (Element : Procedure_Body_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Procedure_Body_Declaration;
   end;

   function Children (Element : access Procedure_Body_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Parameter_Profile)),
        (True, Asis.Element (Element.Body_Declarative_Items)),
        (True, Asis.Element (Element.Body_Statements)),
        (True, Asis.Element (Element.Body_Exception_Handlers)));
   end Children;

   function Clone
     (Element : Procedure_Body_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Procedure_Body_Declaration_Ptr := new Procedure_Body_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Corresponding_Body_Stub := Element.Corresponding_Body_Stub;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      Result.Handled_Statements := Element.Handled_Statements;
      Result.Compound_Name := Element.Compound_Name;
      Result.Specification := Element.Specification;
      Result.Overriding_Indicator_Kind := Element.Overriding_Indicator_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Procedure_Body_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
      Set_Body_Declarative_Items
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Body_Declarative_Items (Source.all), Cloner, Asis.Element (Target)));
      Set_Body_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Body_Statements (Source.all), Cloner, Asis.Element (Target)));
      Set_Body_Exception_Handlers
        (Target.all,
         Primary_Handler_Lists.Deep_Copy 
           (Body_Exception_Handlers (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Result_Subtype
     (Element : Function_Body_Declaration_Node) return Asis.Definition is
   begin
      return Element.Result_Subtype;
   end Result_Subtype;

   procedure Set_Result_Subtype
     (Element : in out Function_Body_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Result_Subtype := Value;
   end Set_Result_Subtype;

   function New_Function_Body_Declaration_Node
     (The_Context : ASIS.Context)
      return Function_Body_Declaration_Ptr
   is
      Result : Function_Body_Declaration_Ptr :=
       new Function_Body_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Function_Body_Declaration_Node;
  
   function Declaration_Kind (Element : Function_Body_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Function_Body_Declaration;
   end;

   function Children (Element : access Function_Body_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Parameter_Profile)),
        (False, Element.Result_Subtype'Access),
        (True, Asis.Element (Element.Body_Declarative_Items)),
        (True, Asis.Element (Element.Body_Statements)),
        (True, Asis.Element (Element.Body_Exception_Handlers)));
   end Children;

   function Clone
     (Element : Function_Body_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Function_Body_Declaration_Ptr := new Function_Body_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Corresponding_Body_Stub := Element.Corresponding_Body_Stub;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      Result.Handled_Statements := Element.Handled_Statements;
      Result.Compound_Name := Element.Compound_Name;
      Result.Specification := Element.Specification;
      Result.Overriding_Indicator_Kind := Element.Overriding_Indicator_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Function_Body_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
      Target.Result_Subtype :=
        Copy (Cloner, Result_Subtype (Source.all), Asis.Element (Target));
      Set_Body_Declarative_Items
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Body_Declarative_Items (Source.all), Cloner, Asis.Element (Target)));
      Set_Body_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Body_Statements (Source.all), Cloner, Asis.Element (Target)));
      Set_Body_Exception_Handlers
        (Target.all,
         Primary_Handler_Lists.Deep_Copy 
           (Body_Exception_Handlers (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Package_Body_Declaration_Node
     (The_Context : ASIS.Context)
      return Package_Body_Declaration_Ptr
   is
      Result : Package_Body_Declaration_Ptr :=
       new Package_Body_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Package_Body_Declaration_Node;
  
   function Declaration_Kind (Element : Package_Body_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Package_Body_Declaration;
   end;

   function Clone
     (Element : Package_Body_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Package_Body_Declaration_Ptr := new Package_Body_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Corresponding_Body_Stub := Element.Corresponding_Body_Stub;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      Result.Handled_Statements := Element.Handled_Statements;
      Result.Compound_Name := Element.Compound_Name;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Package_Body_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Body_Declarative_Items
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Body_Declarative_Items (Source.all), Cloner, Asis.Element (Target)));
      Set_Body_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Body_Statements (Source.all), Cloner, Asis.Element (Target)));
      Set_Body_Exception_Handlers
        (Target.all,
         Primary_Handler_Lists.Deep_Copy 
           (Body_Exception_Handlers (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Task_Body_Declaration_Node
     (The_Context : ASIS.Context)
      return Task_Body_Declaration_Ptr
   is
      Result : Task_Body_Declaration_Ptr :=
       new Task_Body_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Task_Body_Declaration_Node;
  
   function Declaration_Kind (Element : Task_Body_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Task_Body_Declaration;
   end;

   function Clone
     (Element : Task_Body_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Task_Body_Declaration_Ptr := new Task_Body_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Corresponding_Body_Stub := Element.Corresponding_Body_Stub;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      Result.Handled_Statements := Element.Handled_Statements;
      Result.Compound_Name := Element.Compound_Name;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Task_Body_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Body_Declarative_Items
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Body_Declarative_Items (Source.all), Cloner, Asis.Element (Target)));
      Set_Body_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Body_Statements (Source.all), Cloner, Asis.Element (Target)));
      Set_Body_Exception_Handlers
        (Target.all,
         Primary_Handler_Lists.Deep_Copy 
           (Body_Exception_Handlers (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Parameter_Profile
     (Element : Entry_Body_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Parameter_Lists.To_Element_List
        (Element.Parameter_Profile, Include_Pragmas);
   end Parameter_Profile;

   procedure Set_Parameter_Profile
     (Element : in out Entry_Body_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Parameter_Profile := Primary_Parameter_Lists.List (Value);
   end Set_Parameter_Profile;

   function Parameter_Profile_List
     (Element : Entry_Body_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Parameter_Profile);
   end Parameter_Profile_List;

   function Entry_Index_Specification
     (Element : Entry_Body_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Entry_Index_Specification;
   end Entry_Index_Specification;

   procedure Set_Entry_Index_Specification
     (Element : in out Entry_Body_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Entry_Index_Specification := Value;
   end Set_Entry_Index_Specification;

   function Entry_Barrier
     (Element : Entry_Body_Declaration_Node) return Asis.Expression is
   begin
      return Element.Entry_Barrier;
   end Entry_Barrier;

   procedure Set_Entry_Barrier
     (Element : in out Entry_Body_Declaration_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Entry_Barrier := Value;
   end Set_Entry_Barrier;

   function Specification
     (Element : Entry_Body_Declaration_Node) return Asis.Element is
   begin
      return Element.Specification;
   end Specification;

   procedure Set_Specification
     (Element : in out Entry_Body_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Specification := Value;
   end Set_Specification;

   function New_Entry_Body_Declaration_Node
     (The_Context : ASIS.Context)
      return Entry_Body_Declaration_Ptr
   is
      Result : Entry_Body_Declaration_Ptr :=
       new Entry_Body_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Entry_Body_Declaration_Node;
  
   function Declaration_Kind (Element : Entry_Body_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return An_Entry_Body_Declaration;
   end;

   function Children (Element : access Entry_Body_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Entry_Index_Specification'Access),
        (True, Asis.Element (Element.Parameter_Profile)),
        (False, Element.Entry_Barrier'Access),
        (True, Asis.Element (Element.Body_Declarative_Items)),
        (True, Asis.Element (Element.Body_Statements)),
        (True, Asis.Element (Element.Body_Exception_Handlers)));
   end Children;

   function Clone
     (Element : Entry_Body_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Entry_Body_Declaration_Ptr := new Entry_Body_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Corresponding_Body_Stub := Element.Corresponding_Body_Stub;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      Result.Handled_Statements := Element.Handled_Statements;
      Result.Compound_Name := Element.Compound_Name;
      Result.Specification := Element.Specification;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Entry_Body_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Entry_Index_Specification :=
        Copy (Cloner, Entry_Index_Specification (Source.all), Asis.Element (Target));
      Set_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
      Target.Entry_Barrier :=
        Copy (Cloner, Entry_Barrier (Source.all), Asis.Element (Target));
      Set_Body_Declarative_Items
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Body_Declarative_Items (Source.all), Cloner, Asis.Element (Target)));
      Set_Body_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Body_Statements (Source.all), Cloner, Asis.Element (Target)));
      Set_Body_Exception_Handlers
        (Target.all,
         Primary_Handler_Lists.Deep_Copy 
           (Body_Exception_Handlers (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Renamed_Entity
     (Element : Base_Renaming_Declaration_Node) return Asis.Expression is
   begin
      return Element.Renamed_Entity;
   end Renamed_Entity;

   procedure Set_Renamed_Entity
     (Element : in out Base_Renaming_Declaration_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Renamed_Entity := Value;
   end Set_Renamed_Entity;

   function Corresponding_Base_Entity
     (Element : Base_Renaming_Declaration_Node) return Asis.Expression is
   begin
      return Element.Corresponding_Base_Entity;
   end Corresponding_Base_Entity;

   procedure Set_Corresponding_Base_Entity
     (Element : in out Base_Renaming_Declaration_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Corresponding_Base_Entity := Value;
   end Set_Corresponding_Base_Entity;

   function Children (Element : access Base_Renaming_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Renamed_Entity'Access));
   end Children;

   function Object_Declaration_Subtype
     (Element : Object_Renaming_Declaration_Node) return Asis.Definition is
   begin
      return Element.Object_Declaration_Subtype;
   end Object_Declaration_Subtype;

   procedure Set_Object_Declaration_Subtype
     (Element : in out Object_Renaming_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Object_Declaration_Subtype := Value;
   end Set_Object_Declaration_Subtype;

   function Has_Null_Exclusion
     (Element : Object_Renaming_Declaration_Node) return Boolean is
   begin
      return Element.Has_Null_Exclusion;
   end Has_Null_Exclusion;

   procedure Set_Has_Null_Exclusion
     (Element : in out Object_Renaming_Declaration_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Null_Exclusion := Value;
   end Set_Has_Null_Exclusion;

   function New_Object_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Object_Renaming_Declaration_Ptr
   is
      Result : Object_Renaming_Declaration_Ptr :=
       new Object_Renaming_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Object_Renaming_Declaration_Node;
  
   function Declaration_Kind (Element : Object_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return An_Object_Renaming_Declaration;
   end;

   function Children (Element : access Object_Renaming_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Object_Declaration_Subtype'Access),
        (False, Element.Renamed_Entity'Access));
   end Children;

   function Clone
     (Element : Object_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Object_Renaming_Declaration_Ptr := new Object_Renaming_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Base_Entity := Element.Corresponding_Base_Entity;
      Result.Has_Null_Exclusion := Element.Has_Null_Exclusion;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Object_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Object_Declaration_Subtype :=
        Copy (Cloner, Object_Declaration_Subtype (Source.all), Asis.Element (Target));
      Target.Renamed_Entity :=
        Copy (Cloner, Renamed_Entity (Source.all), Asis.Element (Target));
   end Copy;

   function New_Exception_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Exception_Renaming_Declaration_Ptr
   is
      Result : Exception_Renaming_Declaration_Ptr :=
       new Exception_Renaming_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Exception_Renaming_Declaration_Node;
  
   function Declaration_Kind (Element : Exception_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return An_Exception_Renaming_Declaration;
   end;

   function Clone
     (Element : Exception_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Exception_Renaming_Declaration_Ptr := new Exception_Renaming_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Base_Entity := Element.Corresponding_Base_Entity;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Exception_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Renamed_Entity :=
        Copy (Cloner, Renamed_Entity (Source.all), Asis.Element (Target));
   end Copy;

   function New_Package_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Package_Renaming_Declaration_Ptr
   is
      Result : Package_Renaming_Declaration_Ptr :=
       new Package_Renaming_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Package_Renaming_Declaration_Node;
  
   function Declaration_Kind (Element : Package_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Package_Renaming_Declaration;
   end;

   function Clone
     (Element : Package_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Package_Renaming_Declaration_Ptr := new Package_Renaming_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Base_Entity := Element.Corresponding_Base_Entity;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Package_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Renamed_Entity :=
        Copy (Cloner, Renamed_Entity (Source.all), Asis.Element (Target));
   end Copy;

   function Empty_Generic_Part
     (Element : Generic_Package_Renaming_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Empty_Generic_Part, Include_Pragmas);
   end Empty_Generic_Part;

   procedure Set_Empty_Generic_Part
     (Element : in out Generic_Package_Renaming_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Empty_Generic_Part := Primary_Declaration_Lists.List (Value);
   end Set_Empty_Generic_Part;

   function Empty_Generic_Part_List
     (Element : Generic_Package_Renaming_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Empty_Generic_Part);
   end Empty_Generic_Part_List;

   function New_Generic_Package_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Generic_Package_Renaming_Declaration_Ptr
   is
      Result : Generic_Package_Renaming_Declaration_Ptr :=
       new Generic_Package_Renaming_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Generic_Package_Renaming_Declaration_Node;
  
   function Declaration_Kind (Element : Generic_Package_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Generic_Package_Renaming_Declaration;
   end;

   function Clone
     (Element : Generic_Package_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Generic_Package_Renaming_Declaration_Ptr := new Generic_Package_Renaming_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Base_Entity := Element.Corresponding_Base_Entity;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Generic_Package_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Renamed_Entity :=
        Copy (Cloner, Renamed_Entity (Source.all), Asis.Element (Target));
   end Copy;

   function Specification
     (Element : Generic_Procedure_Renaming_Declaration_Node) return Asis.Element is
   begin
      return Element.Specification;
   end Specification;

   procedure Set_Specification
     (Element : in out Generic_Procedure_Renaming_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Specification := Value;
   end Set_Specification;

   function New_Generic_Procedure_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Generic_Procedure_Renaming_Declaration_Ptr
   is
      Result : Generic_Procedure_Renaming_Declaration_Ptr :=
       new Generic_Procedure_Renaming_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Generic_Procedure_Renaming_Declaration_Node;
  
   function Declaration_Kind (Element : Generic_Procedure_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Generic_Procedure_Renaming_Declaration;
   end;

   function Clone
     (Element : Generic_Procedure_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Generic_Procedure_Renaming_Declaration_Ptr := new Generic_Procedure_Renaming_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Base_Entity := Element.Corresponding_Base_Entity;
      Result.Specification := Element.Specification;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Generic_Procedure_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Renamed_Entity :=
        Copy (Cloner, Renamed_Entity (Source.all), Asis.Element (Target));
   end Copy;

   function New_Generic_Function_Renaming_Declaration_Node
     (The_Context : ASIS.Context)
      return Generic_Function_Renaming_Declaration_Ptr
   is
      Result : Generic_Function_Renaming_Declaration_Ptr :=
       new Generic_Function_Renaming_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Generic_Function_Renaming_Declaration_Node;
  
   function Declaration_Kind (Element : Generic_Function_Renaming_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Generic_Function_Renaming_Declaration;
   end;

   function Clone
     (Element : Generic_Function_Renaming_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Generic_Function_Renaming_Declaration_Ptr := new Generic_Function_Renaming_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Base_Entity := Element.Corresponding_Base_Entity;
      Result.Specification := Element.Specification;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Generic_Function_Renaming_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Renamed_Entity :=
        Copy (Cloner, Renamed_Entity (Source.all), Asis.Element (Target));
   end Copy;

   function Discriminant_Part
     (Element : Incomplete_Type_Declaration_Node) return Asis.Definition is
   begin
      return Element.Discriminant_Part;
   end Discriminant_Part;

   procedure Set_Discriminant_Part
     (Element : in out Incomplete_Type_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Discriminant_Part := Value;
   end Set_Discriminant_Part;

   function Corresponding_Type_Declaration
     (Element : Incomplete_Type_Declaration_Node) return Asis.Definition is
   begin
      return Element.Corresponding_Type_Declaration;
   end Corresponding_Type_Declaration;

   procedure Set_Corresponding_Type_Declaration
     (Element : in out Incomplete_Type_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Corresponding_Type_Declaration := Value;
   end Set_Corresponding_Type_Declaration;

   function Type_Declaration_View
     (Element : Incomplete_Type_Declaration_Node) return Asis.Definition is
   begin
      return Element.Type_Declaration_View;
   end Type_Declaration_View;

   procedure Set_Type_Declaration_View
     (Element : in out Incomplete_Type_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Type_Declaration_View := Value;
   end Set_Type_Declaration_View;

   function New_Incomplete_Type_Declaration_Node
     (The_Context : ASIS.Context)
      return Incomplete_Type_Declaration_Ptr
   is
      Result : Incomplete_Type_Declaration_Ptr :=
       new Incomplete_Type_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Incomplete_Type_Declaration_Node;
  
   function Declaration_Kind (Element : Incomplete_Type_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return An_Incomplete_Type_Declaration;
   end;

   function Children (Element : access Incomplete_Type_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Discriminant_Part'Access),
        (False, Element.Type_Declaration_View'Access));
   end Children;

   function Clone
     (Element : Incomplete_Type_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Incomplete_Type_Declaration_Ptr := new Incomplete_Type_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Type_Declaration := Element.Corresponding_Type_Declaration;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Incomplete_Type_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Discriminant_Part :=
        Copy (Cloner, Discriminant_Part (Source.all), Asis.Element (Target));
      Target.Type_Declaration_View :=
        Copy (Cloner, Type_Declaration_View (Source.all), Asis.Element (Target));
   end Copy;

   function Type_Declaration_View
     (Element : Subtype_Declaration_Node) return Asis.Definition is
   begin
      return Element.Type_Declaration_View;
   end Type_Declaration_View;

   procedure Set_Type_Declaration_View
     (Element : in out Subtype_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Type_Declaration_View := Value;
   end Set_Type_Declaration_View;

   function Corresponding_First_Subtype
     (Element : Subtype_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_First_Subtype;
   end Corresponding_First_Subtype;

   procedure Set_Corresponding_First_Subtype
     (Element : in out Subtype_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_First_Subtype := Value;
   end Set_Corresponding_First_Subtype;

   function Corresponding_Last_Constraint
     (Element : Subtype_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Last_Constraint;
   end Corresponding_Last_Constraint;

   procedure Set_Corresponding_Last_Constraint
     (Element : in out Subtype_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Last_Constraint := Value;
   end Set_Corresponding_Last_Constraint;

   function Corresponding_Last_Subtype
     (Element : Subtype_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Last_Subtype;
   end Corresponding_Last_Subtype;

   procedure Set_Corresponding_Last_Subtype
     (Element : in out Subtype_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Last_Subtype := Value;
   end Set_Corresponding_Last_Subtype;

   function New_Subtype_Declaration_Node
     (The_Context : ASIS.Context)
      return Subtype_Declaration_Ptr
   is
      Result : Subtype_Declaration_Ptr :=
       new Subtype_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Subtype_Declaration_Node;
  
   function Declaration_Kind (Element : Subtype_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Subtype_Declaration;
   end;

   function Children (Element : access Subtype_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Type_Declaration_View'Access));
   end Children;

   function Clone
     (Element : Subtype_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Subtype_Declaration_Ptr := new Subtype_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_First_Subtype := Element.Corresponding_First_Subtype;
      Result.Corresponding_Last_Constraint := Element.Corresponding_Last_Constraint;
      Result.Corresponding_Last_Subtype := Element.Corresponding_Last_Subtype;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Subtype_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Type_Declaration_View :=
        Copy (Cloner, Type_Declaration_View (Source.all), Asis.Element (Target));
   end Copy;

   function Object_Declaration_Subtype
     (Element : Component_Declaration_Node) return Asis.Definition is
   begin
      return Element.Object_Declaration_Subtype;
   end Object_Declaration_Subtype;

   procedure Set_Object_Declaration_Subtype
     (Element : in out Component_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Object_Declaration_Subtype := Value;
   end Set_Object_Declaration_Subtype;

   function Initialization_Expression
     (Element : Component_Declaration_Node) return Asis.Expression is
   begin
      return Element.Initialization_Expression;
   end Initialization_Expression;

   procedure Set_Initialization_Expression
     (Element : in out Component_Declaration_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Initialization_Expression := Value;
   end Set_Initialization_Expression;

   function New_Component_Declaration_Node
     (The_Context : ASIS.Context)
      return Component_Declaration_Ptr
   is
      Result : Component_Declaration_Ptr :=
       new Component_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Component_Declaration_Node;
  
   function Declaration_Kind (Element : Component_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Component_Declaration;
   end;

   function Children (Element : access Component_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Object_Declaration_Subtype'Access),
        (False, Element.Initialization_Expression'Access));
   end Children;

   function Clone
     (Element : Component_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Component_Declaration_Ptr := new Component_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Component_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Object_Declaration_Subtype :=
        Copy (Cloner, Object_Declaration_Subtype (Source.all), Asis.Element (Target));
      Target.Initialization_Expression :=
        Copy (Cloner, Initialization_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function Trait_Kind
     (Element : Variable_Declaration_Node) return Asis.Trait_Kinds is
   begin
      return Element.Trait_Kind;
   end Trait_Kind;

   procedure Set_Trait_Kind
     (Element : in out Variable_Declaration_Node;
      Value   : in     Asis.Trait_Kinds) is
   begin
      Element.Trait_Kind := Value;
   end Set_Trait_Kind;

   function New_Variable_Declaration_Node
     (The_Context : ASIS.Context)
      return Variable_Declaration_Ptr
   is
      Result : Variable_Declaration_Ptr :=
       new Variable_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Variable_Declaration_Node;
  
   function Declaration_Kind (Element : Variable_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Variable_Declaration;
   end;

   function Clone
     (Element : Variable_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Variable_Declaration_Ptr := new Variable_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Trait_Kind := Element.Trait_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Variable_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Object_Declaration_Subtype :=
        Copy (Cloner, Object_Declaration_Subtype (Source.all), Asis.Element (Target));
      Target.Initialization_Expression :=
        Copy (Cloner, Initialization_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function New_Constant_Declaration_Node
     (The_Context : ASIS.Context)
      return Constant_Declaration_Ptr
   is
      Result : Constant_Declaration_Ptr :=
       new Constant_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Constant_Declaration_Node;
  
   function Declaration_Kind (Element : Constant_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Constant_Declaration;
   end;

   function Clone
     (Element : Constant_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Constant_Declaration_Ptr := new Constant_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Trait_Kind := Element.Trait_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Constant_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Object_Declaration_Subtype :=
        Copy (Cloner, Object_Declaration_Subtype (Source.all), Asis.Element (Target));
      Target.Initialization_Expression :=
        Copy (Cloner, Initialization_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function New_Return_Object_Specification_Node
     (The_Context : ASIS.Context)
      return Return_Object_Specification_Ptr
   is
      Result : Return_Object_Specification_Ptr :=
       new Return_Object_Specification_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Return_Object_Specification_Node;
  
   function Declaration_Kind (Element : Return_Object_Specification_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Return_Object_Specification;
   end;

   function Clone
     (Element : Return_Object_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Return_Object_Specification_Ptr := new Return_Object_Specification_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Trait_Kind := Element.Trait_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Return_Object_Specification_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Object_Declaration_Subtype :=
        Copy (Cloner, Object_Declaration_Subtype (Source.all), Asis.Element (Target));
      Target.Initialization_Expression :=
        Copy (Cloner, Initialization_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function Object_Declaration_Subtype
     (Element : Deferred_Constant_Declaration_Node) return Asis.Definition is
   begin
      return Element.Object_Declaration_Subtype;
   end Object_Declaration_Subtype;

   procedure Set_Object_Declaration_Subtype
     (Element : in out Deferred_Constant_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Object_Declaration_Subtype := Value;
   end Set_Object_Declaration_Subtype;

   function Trait_Kind
     (Element : Deferred_Constant_Declaration_Node) return Asis.Trait_Kinds is
   begin
      return Element.Trait_Kind;
   end Trait_Kind;

   procedure Set_Trait_Kind
     (Element : in out Deferred_Constant_Declaration_Node;
      Value   : in     Asis.Trait_Kinds) is
   begin
      Element.Trait_Kind := Value;
   end Set_Trait_Kind;

   function New_Deferred_Constant_Declaration_Node
     (The_Context : ASIS.Context)
      return Deferred_Constant_Declaration_Ptr
   is
      Result : Deferred_Constant_Declaration_Ptr :=
       new Deferred_Constant_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Deferred_Constant_Declaration_Node;
  
   function Declaration_Kind (Element : Deferred_Constant_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Deferred_Constant_Declaration;
   end;

   function Children (Element : access Deferred_Constant_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Object_Declaration_Subtype'Access));
   end Children;

   function Clone
     (Element : Deferred_Constant_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Deferred_Constant_Declaration_Ptr := new Deferred_Constant_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Trait_Kind := Element.Trait_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Deferred_Constant_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Object_Declaration_Subtype :=
        Copy (Cloner, Object_Declaration_Subtype (Source.all), Asis.Element (Target));
   end Copy;

   function Progenitor_List
     (Element : Single_Protected_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Expression_Lists.To_Element_List
        (Element.Progenitor_List, Include_Pragmas);
   end Progenitor_List;

   procedure Set_Progenitor_List
     (Element : in out Single_Protected_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Progenitor_List := Primary_Expression_Lists.List (Value);
   end Set_Progenitor_List;

   function Progenitor_List_List
     (Element : Single_Protected_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Progenitor_List);
   end Progenitor_List_List;

   function Object_Declaration_Subtype
     (Element : Single_Protected_Declaration_Node) return Asis.Definition is
   begin
      return Element.Object_Declaration_Subtype;
   end Object_Declaration_Subtype;

   procedure Set_Object_Declaration_Subtype
     (Element : in out Single_Protected_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Object_Declaration_Subtype := Value;
   end Set_Object_Declaration_Subtype;

   function Is_Name_Repeated
     (Element : Single_Protected_Declaration_Node) return Boolean is
   begin
      return Element.Is_Name_Repeated;
   end Is_Name_Repeated;

   procedure Set_Is_Name_Repeated
     (Element : in out Single_Protected_Declaration_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Name_Repeated := Value;
   end Set_Is_Name_Repeated;

   function Corresponding_Body
     (Element : Single_Protected_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Body;
   end Corresponding_Body;

   procedure Set_Corresponding_Body
     (Element : in out Single_Protected_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Body := Value;
   end Set_Corresponding_Body;

   function New_Single_Protected_Declaration_Node
     (The_Context : ASIS.Context)
      return Single_Protected_Declaration_Ptr
   is
      Result : Single_Protected_Declaration_Ptr :=
       new Single_Protected_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Single_Protected_Declaration_Node;
  
   function Declaration_Kind (Element : Single_Protected_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Single_Protected_Declaration;
   end;

   function Children (Element : access Single_Protected_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Progenitor_List)),
        (False, Element.Object_Declaration_Subtype'Access));
   end Children;

   function Clone
     (Element : Single_Protected_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Single_Protected_Declaration_Ptr := new Single_Protected_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      Result.Corresponding_Body := Element.Corresponding_Body;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Single_Protected_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Progenitor_List
        (Target.all,
         Primary_Expression_Lists.Deep_Copy 
           (Progenitor_List (Source.all), Cloner, Asis.Element (Target)));
      Target.Object_Declaration_Subtype :=
        Copy (Cloner, Object_Declaration_Subtype (Source.all), Asis.Element (Target));
   end Copy;

   function New_Single_Task_Declaration_Node
     (The_Context : ASIS.Context)
      return Single_Task_Declaration_Ptr
   is
      Result : Single_Task_Declaration_Ptr :=
       new Single_Task_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Single_Task_Declaration_Node;
  
   function Declaration_Kind (Element : Single_Task_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Single_Task_Declaration;
   end;

   function Clone
     (Element : Single_Task_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Single_Task_Declaration_Ptr := new Single_Task_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      Result.Corresponding_Body := Element.Corresponding_Body;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Single_Task_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Progenitor_List
        (Target.all,
         Primary_Expression_Lists.Deep_Copy 
           (Progenitor_List (Source.all), Cloner, Asis.Element (Target)));
      Target.Object_Declaration_Subtype :=
        Copy (Cloner, Object_Declaration_Subtype (Source.all), Asis.Element (Target));
   end Copy;

   function Initialization_Expression
     (Element : Integer_Number_Declaration_Node) return Asis.Expression is
   begin
      return Element.Initialization_Expression;
   end Initialization_Expression;

   procedure Set_Initialization_Expression
     (Element : in out Integer_Number_Declaration_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Initialization_Expression := Value;
   end Set_Initialization_Expression;

   function Declaration_Kind
     (Element : Integer_Number_Declaration_Node) return Asis.Declaration_Kinds is
   begin
      return Element.Declaration_Kind;
   end Declaration_Kind;

   procedure Set_Declaration_Kind
     (Element : in out Integer_Number_Declaration_Node;
      Value   : in     Asis.Declaration_Kinds) is
   begin
      Element.Declaration_Kind := Value;
   end Set_Declaration_Kind;

   function New_Integer_Number_Declaration_Node
     (The_Context : ASIS.Context)
      return Integer_Number_Declaration_Ptr
   is
      Result : Integer_Number_Declaration_Ptr :=
       new Integer_Number_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Integer_Number_Declaration_Node;
  
   function Children (Element : access Integer_Number_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Initialization_Expression'Access));
   end Children;

   function Clone
     (Element : Integer_Number_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Integer_Number_Declaration_Ptr := new Integer_Number_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Declaration_Kind := Element.Declaration_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Integer_Number_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Initialization_Expression :=
        Copy (Cloner, Initialization_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function New_Enumeration_Literal_Specification_Node
     (The_Context : ASIS.Context)
      return Enumeration_Literal_Specification_Ptr
   is
      Result : Enumeration_Literal_Specification_Ptr :=
       new Enumeration_Literal_Specification_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Enumeration_Literal_Specification_Node;
  
   function Declaration_Kind (Element : Enumeration_Literal_Specification_Node)
      return Asis.Declaration_Kinds is
   begin
      return An_Enumeration_Literal_Specification;
   end;

   function Clone
     (Element : Enumeration_Literal_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Enumeration_Literal_Specification_Ptr := new Enumeration_Literal_Specification_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Enumeration_Literal_Specification_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Object_Declaration_Subtype
     (Element : Discriminant_Specification_Node) return Asis.Definition is
   begin
      return Element.Object_Declaration_Subtype;
   end Object_Declaration_Subtype;

   procedure Set_Object_Declaration_Subtype
     (Element : in out Discriminant_Specification_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Object_Declaration_Subtype := Value;
   end Set_Object_Declaration_Subtype;

   function Initialization_Expression
     (Element : Discriminant_Specification_Node) return Asis.Expression is
   begin
      return Element.Initialization_Expression;
   end Initialization_Expression;

   procedure Set_Initialization_Expression
     (Element : in out Discriminant_Specification_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Initialization_Expression := Value;
   end Set_Initialization_Expression;

   function Trait_Kind
     (Element : Discriminant_Specification_Node) return Asis.Trait_Kinds is
   begin
      return Element.Trait_Kind;
   end Trait_Kind;

   procedure Set_Trait_Kind
     (Element : in out Discriminant_Specification_Node;
      Value   : in     Asis.Trait_Kinds) is
   begin
      Element.Trait_Kind := Value;
   end Set_Trait_Kind;

   function Has_Null_Exclusion
     (Element : Discriminant_Specification_Node) return Boolean is
   begin
      return Element.Has_Null_Exclusion;
   end Has_Null_Exclusion;

   procedure Set_Has_Null_Exclusion
     (Element : in out Discriminant_Specification_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Null_Exclusion := Value;
   end Set_Has_Null_Exclusion;

   function New_Discriminant_Specification_Node
     (The_Context : ASIS.Context)
      return Discriminant_Specification_Ptr
   is
      Result : Discriminant_Specification_Ptr :=
       new Discriminant_Specification_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Discriminant_Specification_Node;
  
   function Declaration_Kind (Element : Discriminant_Specification_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Discriminant_Specification;
   end;

   function Children (Element : access Discriminant_Specification_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Object_Declaration_Subtype'Access),
        (False, Element.Initialization_Expression'Access));
   end Children;

   function Clone
     (Element : Discriminant_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Discriminant_Specification_Ptr := new Discriminant_Specification_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Trait_Kind := Element.Trait_Kind;
      Result.Has_Null_Exclusion := Element.Has_Null_Exclusion;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Discriminant_Specification_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Object_Declaration_Subtype :=
        Copy (Cloner, Object_Declaration_Subtype (Source.all), Asis.Element (Target));
      Target.Initialization_Expression :=
        Copy (Cloner, Initialization_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function Specification_Subtype_Definition
     (Element : Entry_Index_Specification_Node) return Asis.Discrete_Subtype_Definition is
   begin
      return Element.Specification_Subtype_Definition;
   end Specification_Subtype_Definition;

   procedure Set_Specification_Subtype_Definition
     (Element : in out Entry_Index_Specification_Node;
      Value   : in     Asis.Discrete_Subtype_Definition) is
   begin
      Element.Specification_Subtype_Definition := Value;
   end Set_Specification_Subtype_Definition;

   function New_Entry_Index_Specification_Node
     (The_Context : ASIS.Context)
      return Entry_Index_Specification_Ptr
   is
      Result : Entry_Index_Specification_Ptr :=
       new Entry_Index_Specification_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Entry_Index_Specification_Node;
  
   function Declaration_Kind (Element : Entry_Index_Specification_Node)
      return Asis.Declaration_Kinds is
   begin
      return An_Entry_Index_Specification;
   end;

   function Children (Element : access Entry_Index_Specification_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Specification_Subtype_Definition'Access));
   end Children;

   function Clone
     (Element : Entry_Index_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Entry_Index_Specification_Ptr := new Entry_Index_Specification_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Entry_Index_Specification_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Specification_Subtype_Definition :=
        Copy (Cloner, Specification_Subtype_Definition (Source.all), Asis.Element (Target));
   end Copy;

   function Trait_Kind
     (Element : Loop_Parameter_Specification_Node) return Asis.Trait_Kinds is
   begin
      return Element.Trait_Kind;
   end Trait_Kind;

   procedure Set_Trait_Kind
     (Element : in out Loop_Parameter_Specification_Node;
      Value   : in     Asis.Trait_Kinds) is
   begin
      Element.Trait_Kind := Value;
   end Set_Trait_Kind;

   function New_Loop_Parameter_Specification_Node
     (The_Context : ASIS.Context)
      return Loop_Parameter_Specification_Ptr
   is
      Result : Loop_Parameter_Specification_Ptr :=
       new Loop_Parameter_Specification_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Loop_Parameter_Specification_Node;
  
   function Declaration_Kind (Element : Loop_Parameter_Specification_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Loop_Parameter_Specification;
   end;

   function Clone
     (Element : Loop_Parameter_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Loop_Parameter_Specification_Ptr := new Loop_Parameter_Specification_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Trait_Kind := Element.Trait_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Loop_Parameter_Specification_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Specification_Subtype_Definition :=
        Copy (Cloner, Specification_Subtype_Definition (Source.all), Asis.Element (Target));
   end Copy;

   function Mode_Kind
     (Element : Formal_Object_Declaration_Node) return Asis.Mode_Kinds is
   begin
      return Element.Mode_Kind;
   end Mode_Kind;

   procedure Set_Mode_Kind
     (Element : in out Formal_Object_Declaration_Node;
      Value   : in     Asis.Mode_Kinds) is
   begin
      Element.Mode_Kind := Value;
   end Set_Mode_Kind;

   function Object_Declaration_Subtype
     (Element : Formal_Object_Declaration_Node) return Asis.Definition is
   begin
      return Element.Object_Declaration_Subtype;
   end Object_Declaration_Subtype;

   procedure Set_Object_Declaration_Subtype
     (Element : in out Formal_Object_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Object_Declaration_Subtype := Value;
   end Set_Object_Declaration_Subtype;

   function Initialization_Expression
     (Element : Formal_Object_Declaration_Node) return Asis.Expression is
   begin
      return Element.Initialization_Expression;
   end Initialization_Expression;

   procedure Set_Initialization_Expression
     (Element : in out Formal_Object_Declaration_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Initialization_Expression := Value;
   end Set_Initialization_Expression;

   function Has_Null_Exclusion
     (Element : Formal_Object_Declaration_Node) return Boolean is
   begin
      return Element.Has_Null_Exclusion;
   end Has_Null_Exclusion;

   procedure Set_Has_Null_Exclusion
     (Element : in out Formal_Object_Declaration_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Null_Exclusion := Value;
   end Set_Has_Null_Exclusion;

   function Generic_Actual
     (Element : Formal_Object_Declaration_Node) return Asis.Expression is
   begin
      return Element.Generic_Actual;
   end Generic_Actual;

   procedure Set_Generic_Actual
     (Element : in out Formal_Object_Declaration_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Generic_Actual := Value;
   end Set_Generic_Actual;

   function New_Formal_Object_Declaration_Node
     (The_Context : ASIS.Context)
      return Formal_Object_Declaration_Ptr
   is
      Result : Formal_Object_Declaration_Ptr :=
       new Formal_Object_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Object_Declaration_Node;
  
   function Declaration_Kind (Element : Formal_Object_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Formal_Object_Declaration;
   end;

   function Children (Element : access Formal_Object_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Object_Declaration_Subtype'Access),
        (False, Element.Initialization_Expression'Access));
   end Children;

   function Clone
     (Element : Formal_Object_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Object_Declaration_Ptr := new Formal_Object_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Mode_Kind := Element.Mode_Kind;
      Result.Has_Null_Exclusion := Element.Has_Null_Exclusion;
      Result.Generic_Actual := Element.Generic_Actual;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Object_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Object_Declaration_Subtype :=
        Copy (Cloner, Object_Declaration_Subtype (Source.all), Asis.Element (Target));
      Target.Initialization_Expression :=
        Copy (Cloner, Initialization_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function Trait_Kind
     (Element : Parameter_Specification_Node) return Asis.Trait_Kinds is
   begin
      return Element.Trait_Kind;
   end Trait_Kind;

   procedure Set_Trait_Kind
     (Element : in out Parameter_Specification_Node;
      Value   : in     Asis.Trait_Kinds) is
   begin
      Element.Trait_Kind := Value;
   end Set_Trait_Kind;

   function New_Parameter_Specification_Node
     (The_Context : ASIS.Context)
      return Parameter_Specification_Ptr
   is
      Result : Parameter_Specification_Ptr :=
       new Parameter_Specification_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Parameter_Specification_Node;
  
   function Declaration_Kind (Element : Parameter_Specification_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Parameter_Specification;
   end;

   function Clone
     (Element : Parameter_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Parameter_Specification_Ptr := new Parameter_Specification_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Mode_Kind := Element.Mode_Kind;
      Result.Has_Null_Exclusion := Element.Has_Null_Exclusion;
      Result.Generic_Actual := Element.Generic_Actual;
      Result.Trait_Kind := Element.Trait_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Parameter_Specification_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Object_Declaration_Subtype :=
        Copy (Cloner, Object_Declaration_Subtype (Source.all), Asis.Element (Target));
      Target.Initialization_Expression :=
        Copy (Cloner, Initialization_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function Is_Name_Repeated
     (Element : Package_Declaration_Node) return Boolean is
   begin
      return Element.Is_Name_Repeated;
   end Is_Name_Repeated;

   procedure Set_Is_Name_Repeated
     (Element : in out Package_Declaration_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Name_Repeated := Value;
   end Set_Is_Name_Repeated;

   function Corresponding_Declaration
     (Element : Package_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Declaration;
   end Corresponding_Declaration;

   procedure Set_Corresponding_Declaration
     (Element : in out Package_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Declaration := Value;
   end Set_Corresponding_Declaration;

   function Corresponding_Body
     (Element : Package_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Body;
   end Corresponding_Body;

   procedure Set_Corresponding_Body
     (Element : in out Package_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Body := Value;
   end Set_Corresponding_Body;

   function Is_Private_Present
     (Element : Package_Declaration_Node) return Boolean is
   begin
      return Element.Is_Private_Present;
   end Is_Private_Present;

   procedure Set_Is_Private_Present
     (Element : in out Package_Declaration_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Private_Present := Value;
   end Set_Is_Private_Present;

   function Generic_Formal_Part
     (Element : Package_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Generic_Formal_Part, Include_Pragmas);
   end Generic_Formal_Part;

   procedure Set_Generic_Formal_Part
     (Element : in out Package_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Generic_Formal_Part := Primary_Declaration_Lists.List (Value);
   end Set_Generic_Formal_Part;

   function Generic_Formal_Part_List
     (Element : Package_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Generic_Formal_Part);
   end Generic_Formal_Part_List;

   function Visible_Part_Declarative_Items
     (Element : Package_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Visible_Part_Declarative_Items, Include_Pragmas);
   end Visible_Part_Declarative_Items;

   procedure Set_Visible_Part_Declarative_Items
     (Element : in out Package_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Visible_Part_Declarative_Items := Primary_Declaration_Lists.List (Value);
   end Set_Visible_Part_Declarative_Items;

   function Visible_Part_Declarative_Items_List
     (Element : Package_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Visible_Part_Declarative_Items);
   end Visible_Part_Declarative_Items_List;

   function Private_Part_Declarative_Items
     (Element : Package_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Private_Part_Declarative_Items, Include_Pragmas);
   end Private_Part_Declarative_Items;

   procedure Set_Private_Part_Declarative_Items
     (Element : in out Package_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Private_Part_Declarative_Items := Primary_Declaration_Lists.List (Value);
   end Set_Private_Part_Declarative_Items;

   function Private_Part_Declarative_Items_List
     (Element : Package_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Private_Part_Declarative_Items);
   end Private_Part_Declarative_Items_List;

   function Package_Specification
     (Element : Package_Declaration_Node) return Asis.Element is
   begin
      return Element.Package_Specification;
   end Package_Specification;

   procedure Set_Package_Specification
     (Element : in out Package_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Package_Specification := Value;
   end Set_Package_Specification;

   function New_Package_Declaration_Node
     (The_Context : ASIS.Context)
      return Package_Declaration_Ptr
   is
      Result : Package_Declaration_Ptr :=
       new Package_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Package_Declaration_Node;
  
   function Declaration_Kind (Element : Package_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Package_Declaration;
   end;

   function Children (Element : access Package_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Generic_Formal_Part)),
        (True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Visible_Part_Declarative_Items)),
        (True, Asis.Element (Element.Private_Part_Declarative_Items)));
   end Children;

   function Clone
     (Element : Package_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Package_Declaration_Ptr := new Package_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Corresponding_Body := Element.Corresponding_Body;
      Result.Is_Private_Present := Element.Is_Private_Present;
      Result.Package_Specification := Element.Package_Specification;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Package_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Generic_Formal_Part
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Generic_Formal_Part (Source.all), Cloner, Asis.Element (Target)));
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Visible_Part_Declarative_Items
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Visible_Part_Declarative_Items (Source.all), Cloner, Asis.Element (Target)));
      Set_Private_Part_Declarative_Items
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Private_Part_Declarative_Items (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Is_Name_Repeated
     (Element : Protected_Body_Declaration_Node) return Boolean is
   begin
      return Element.Is_Name_Repeated;
   end Is_Name_Repeated;

   procedure Set_Is_Name_Repeated
     (Element : in out Protected_Body_Declaration_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Name_Repeated := Value;
   end Set_Is_Name_Repeated;

   function Corresponding_Declaration
     (Element : Protected_Body_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Declaration;
   end Corresponding_Declaration;

   procedure Set_Corresponding_Declaration
     (Element : in out Protected_Body_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Declaration := Value;
   end Set_Corresponding_Declaration;

   function Protected_Operation_Items
     (Element : Protected_Body_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Protected_Operation_Items, Include_Pragmas);
   end Protected_Operation_Items;

   procedure Set_Protected_Operation_Items
     (Element : in out Protected_Body_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Protected_Operation_Items := Primary_Declaration_Lists.List (Value);
   end Set_Protected_Operation_Items;

   function Protected_Operation_Items_List
     (Element : Protected_Body_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Protected_Operation_Items);
   end Protected_Operation_Items_List;

   function Corresponding_Body_Stub
     (Element : Protected_Body_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Body_Stub;
   end Corresponding_Body_Stub;

   procedure Set_Corresponding_Body_Stub
     (Element : in out Protected_Body_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Body_Stub := Value;
   end Set_Corresponding_Body_Stub;

   function Get_Identifier
     (Element : Protected_Body_Declaration_Node) return Asis.Element is
   begin
      return Element.Identifier;
   end Get_Identifier;

   procedure Set_Identifier
     (Element : in out Protected_Body_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Identifier := Value;
   end Set_Identifier;

   function New_Protected_Body_Declaration_Node
     (The_Context : ASIS.Context)
      return Protected_Body_Declaration_Ptr
   is
      Result : Protected_Body_Declaration_Ptr :=
       new Protected_Body_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Protected_Body_Declaration_Node;
  
   function Declaration_Kind (Element : Protected_Body_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Protected_Body_Declaration;
   end;

   function Children (Element : access Protected_Body_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Protected_Operation_Items)));
   end Children;

   function Clone
     (Element : Protected_Body_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Protected_Body_Declaration_Ptr := new Protected_Body_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Corresponding_Body_Stub := Element.Corresponding_Body_Stub;
      Result.Identifier := Element.Identifier;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Protected_Body_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Protected_Operation_Items
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Protected_Operation_Items (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Corresponding_Subunit
     (Element : Package_Body_Stub_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Subunit;
   end Corresponding_Subunit;

   procedure Set_Corresponding_Subunit
     (Element : in out Package_Body_Stub_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Subunit := Value;
   end Set_Corresponding_Subunit;

   function Corresponding_Declaration
     (Element : Package_Body_Stub_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Declaration;
   end Corresponding_Declaration;

   procedure Set_Corresponding_Declaration
     (Element : in out Package_Body_Stub_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Declaration := Value;
   end Set_Corresponding_Declaration;

   function New_Package_Body_Stub_Node
     (The_Context : ASIS.Context)
      return Package_Body_Stub_Ptr
   is
      Result : Package_Body_Stub_Ptr :=
       new Package_Body_Stub_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Package_Body_Stub_Node;
  
   function Declaration_Kind (Element : Package_Body_Stub_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Package_Body_Stub;
   end;

   function Clone
     (Element : Package_Body_Stub_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Package_Body_Stub_Ptr := new Package_Body_Stub_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Subunit := Element.Corresponding_Subunit;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Package_Body_Stub_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Task_Body_Stub_Node
     (The_Context : ASIS.Context)
      return Task_Body_Stub_Ptr
   is
      Result : Task_Body_Stub_Ptr :=
       new Task_Body_Stub_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Task_Body_Stub_Node;
  
   function Declaration_Kind (Element : Task_Body_Stub_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Task_Body_Stub;
   end;

   function Clone
     (Element : Task_Body_Stub_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Task_Body_Stub_Ptr := new Task_Body_Stub_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Subunit := Element.Corresponding_Subunit;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Task_Body_Stub_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Protected_Body_Stub_Node
     (The_Context : ASIS.Context)
      return Protected_Body_Stub_Ptr
   is
      Result : Protected_Body_Stub_Ptr :=
       new Protected_Body_Stub_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Protected_Body_Stub_Node;
  
   function Declaration_Kind (Element : Protected_Body_Stub_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Protected_Body_Stub;
   end;

   function Clone
     (Element : Protected_Body_Stub_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Protected_Body_Stub_Ptr := new Protected_Body_Stub_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Subunit := Element.Corresponding_Subunit;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Protected_Body_Stub_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Exception_Declaration_Node
     (The_Context : ASIS.Context)
      return Exception_Declaration_Ptr
   is
      Result : Exception_Declaration_Ptr :=
       new Exception_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Exception_Declaration_Node;
  
   function Declaration_Kind (Element : Exception_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return An_Exception_Declaration;
   end;

   function Clone
     (Element : Exception_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Exception_Declaration_Ptr := new Exception_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Exception_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Choice_Parameter_Specification_Node
     (The_Context : ASIS.Context)
      return Choice_Parameter_Specification_Ptr
   is
      Result : Choice_Parameter_Specification_Ptr :=
       new Choice_Parameter_Specification_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Choice_Parameter_Specification_Node;
  
   function Declaration_Kind (Element : Choice_Parameter_Specification_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Choice_Parameter_Specification;
   end;

   function Clone
     (Element : Choice_Parameter_Specification_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Choice_Parameter_Specification_Ptr := new Choice_Parameter_Specification_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Choice_Parameter_Specification_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Is_Name_Repeated
     (Element : Generic_Package_Declaration_Node) return Boolean is
   begin
      return Element.Is_Name_Repeated;
   end Is_Name_Repeated;

   procedure Set_Is_Name_Repeated
     (Element : in out Generic_Package_Declaration_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Name_Repeated := Value;
   end Set_Is_Name_Repeated;

   function Corresponding_Body
     (Element : Generic_Package_Declaration_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Body;
   end Corresponding_Body;

   procedure Set_Corresponding_Body
     (Element : in out Generic_Package_Declaration_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Body := Value;
   end Set_Corresponding_Body;

   function Is_Private_Present
     (Element : Generic_Package_Declaration_Node) return Boolean is
   begin
      return Element.Is_Private_Present;
   end Is_Private_Present;

   procedure Set_Is_Private_Present
     (Element : in out Generic_Package_Declaration_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Private_Present := Value;
   end Set_Is_Private_Present;

   function Visible_Part_Declarative_Items
     (Element : Generic_Package_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Visible_Part_Declarative_Items, Include_Pragmas);
   end Visible_Part_Declarative_Items;

   procedure Set_Visible_Part_Declarative_Items
     (Element : in out Generic_Package_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Visible_Part_Declarative_Items := Primary_Declaration_Lists.List (Value);
   end Set_Visible_Part_Declarative_Items;

   function Visible_Part_Declarative_Items_List
     (Element : Generic_Package_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Visible_Part_Declarative_Items);
   end Visible_Part_Declarative_Items_List;

   function Private_Part_Declarative_Items
     (Element : Generic_Package_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Private_Part_Declarative_Items, Include_Pragmas);
   end Private_Part_Declarative_Items;

   procedure Set_Private_Part_Declarative_Items
     (Element : in out Generic_Package_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Private_Part_Declarative_Items := Primary_Declaration_Lists.List (Value);
   end Set_Private_Part_Declarative_Items;

   function Private_Part_Declarative_Items_List
     (Element : Generic_Package_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Private_Part_Declarative_Items);
   end Private_Part_Declarative_Items_List;

   function Generic_Formal_Part
     (Element : Generic_Package_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Generic_Formal_Part, Include_Pragmas);
   end Generic_Formal_Part;

   procedure Set_Generic_Formal_Part
     (Element : in out Generic_Package_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Generic_Formal_Part := Primary_Declaration_Lists.List (Value);
   end Set_Generic_Formal_Part;

   function Generic_Formal_Part_List
     (Element : Generic_Package_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Generic_Formal_Part);
   end Generic_Formal_Part_List;

   function Specification
     (Element : Generic_Package_Declaration_Node) return Asis.Element is
   begin
      return Element.Specification;
   end Specification;

   procedure Set_Specification
     (Element : in out Generic_Package_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Specification := Value;
   end Set_Specification;

   function New_Generic_Package_Declaration_Node
     (The_Context : ASIS.Context)
      return Generic_Package_Declaration_Ptr
   is
      Result : Generic_Package_Declaration_Ptr :=
       new Generic_Package_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Generic_Package_Declaration_Node;
  
   function Declaration_Kind (Element : Generic_Package_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Generic_Package_Declaration;
   end;

   function Children (Element : access Generic_Package_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Generic_Formal_Part)),
        (True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Visible_Part_Declarative_Items)),
        (True, Asis.Element (Element.Private_Part_Declarative_Items)));
   end Children;

   function Clone
     (Element : Generic_Package_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Generic_Package_Declaration_Ptr := new Generic_Package_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      Result.Corresponding_Body := Element.Corresponding_Body;
      Result.Is_Private_Present := Element.Is_Private_Present;
      Result.Specification := Element.Specification;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Generic_Package_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Generic_Formal_Part
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Generic_Formal_Part (Source.all), Cloner, Asis.Element (Target)));
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Visible_Part_Declarative_Items
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Visible_Part_Declarative_Items (Source.all), Cloner, Asis.Element (Target)));
      Set_Private_Part_Declarative_Items
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Private_Part_Declarative_Items (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Corresponding_Declaration
     (Element : Formal_Package_Declaration_With_Box_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Declaration;
   end Corresponding_Declaration;

   procedure Set_Corresponding_Declaration
     (Element : in out Formal_Package_Declaration_With_Box_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Declaration := Value;
   end Set_Corresponding_Declaration;

   function Corresponding_Body
     (Element : Formal_Package_Declaration_With_Box_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Body;
   end Corresponding_Body;

   procedure Set_Corresponding_Body
     (Element : in out Formal_Package_Declaration_With_Box_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Body := Value;
   end Set_Corresponding_Body;

   function Generic_Unit_Name
     (Element : Formal_Package_Declaration_With_Box_Node) return Asis.Expression is
   begin
      return Element.Generic_Unit_Name;
   end Generic_Unit_Name;

   procedure Set_Generic_Unit_Name
     (Element : in out Formal_Package_Declaration_With_Box_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Generic_Unit_Name := Value;
   end Set_Generic_Unit_Name;

   function Normalized_Generic_Actual_Part
     (Element : Formal_Package_Declaration_With_Box_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Association_Lists.To_Element_List
        (Element.Normalized_Generic_Actual_Part, Include_Pragmas);
   end Normalized_Generic_Actual_Part;

   procedure Add_To_Normalized_Generic_Actual_Part
     (Element : in out Formal_Package_Declaration_With_Box_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Association_Lists.Add (Element.Normalized_Generic_Actual_Part, Item);
   end Add_To_Normalized_Generic_Actual_Part;

   function Generic_Actual
     (Element : Formal_Package_Declaration_With_Box_Node) return Asis.Expression is
   begin
      return Element.Generic_Actual;
   end Generic_Actual;

   procedure Set_Generic_Actual
     (Element : in out Formal_Package_Declaration_With_Box_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Generic_Actual := Value;
   end Set_Generic_Actual;

   function New_Formal_Package_Declaration_With_Box_Node
     (The_Context : ASIS.Context)
      return Formal_Package_Declaration_With_Box_Ptr
   is
      Result : Formal_Package_Declaration_With_Box_Ptr :=
       new Formal_Package_Declaration_With_Box_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Package_Declaration_With_Box_Node;
  
   function Declaration_Kind (Element : Formal_Package_Declaration_With_Box_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Formal_Package_Declaration_With_Box;
   end;

   function Children (Element : access Formal_Package_Declaration_With_Box_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Generic_Unit_Name'Access));
   end Children;

   function Clone
     (Element : Formal_Package_Declaration_With_Box_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Package_Declaration_With_Box_Ptr := new Formal_Package_Declaration_With_Box_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Corresponding_Body := Element.Corresponding_Body;
      null;
      Result.Generic_Actual := Element.Generic_Actual;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Package_Declaration_With_Box_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Generic_Unit_Name :=
        Copy (Cloner, Generic_Unit_Name (Source.all), Asis.Element (Target));
   end Copy;

   function Generic_Actual_Part
     (Element : Package_Instantiation_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Association_Lists.To_Element_List
        (Element.Generic_Actual_Part, Include_Pragmas);
   end Generic_Actual_Part;

   procedure Set_Generic_Actual_Part
     (Element : in out Package_Instantiation_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Generic_Actual_Part := Primary_Association_Lists.List (Value);
   end Set_Generic_Actual_Part;

   function Generic_Actual_Part_List
     (Element : Package_Instantiation_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Generic_Actual_Part);
   end Generic_Actual_Part_List;

   function New_Package_Instantiation_Node
     (The_Context : ASIS.Context)
      return Package_Instantiation_Ptr
   is
      Result : Package_Instantiation_Ptr :=
       new Package_Instantiation_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Package_Instantiation_Node;
  
   function Declaration_Kind (Element : Package_Instantiation_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Package_Instantiation;
   end;

   function Children (Element : access Package_Instantiation_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (False, Element.Generic_Unit_Name'Access),
        (True, Asis.Element (Element.Generic_Actual_Part)));
   end Children;

   function Clone
     (Element : Package_Instantiation_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Package_Instantiation_Ptr := new Package_Instantiation_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Corresponding_Body := Element.Corresponding_Body;
      null;
      Result.Generic_Actual := Element.Generic_Actual;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Package_Instantiation_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Generic_Unit_Name :=
        Copy (Cloner, Generic_Unit_Name (Source.all), Asis.Element (Target));
      Set_Generic_Actual_Part
        (Target.all,
         Primary_Association_Lists.Deep_Copy 
           (Generic_Actual_Part (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Specification
     (Element : Procedure_Instantiation_Node) return Asis.Element is
   begin
      return Element.Specification;
   end Specification;

   procedure Set_Specification
     (Element : in out Procedure_Instantiation_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Specification := Value;
   end Set_Specification;

   function Overriding_Indicator_Kind
     (Element : Procedure_Instantiation_Node) return Asis.Overriding_Indicator_Kinds is
   begin
      return Element.Overriding_Indicator_Kind;
   end Overriding_Indicator_Kind;

   procedure Set_Overriding_Indicator_Kind
     (Element : in out Procedure_Instantiation_Node;
      Value   : in     Asis.Overriding_Indicator_Kinds) is
   begin
      Element.Overriding_Indicator_Kind := Value;
   end Set_Overriding_Indicator_Kind;

   function New_Procedure_Instantiation_Node
     (The_Context : ASIS.Context)
      return Procedure_Instantiation_Ptr
   is
      Result : Procedure_Instantiation_Ptr :=
       new Procedure_Instantiation_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Procedure_Instantiation_Node;
  
   function Declaration_Kind (Element : Procedure_Instantiation_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Procedure_Instantiation;
   end;

   function Clone
     (Element : Procedure_Instantiation_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Procedure_Instantiation_Ptr := new Procedure_Instantiation_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Corresponding_Body := Element.Corresponding_Body;
      null;
      Result.Generic_Actual := Element.Generic_Actual;
      Result.Specification := Element.Specification;
      Result.Overriding_Indicator_Kind := Element.Overriding_Indicator_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Procedure_Instantiation_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Generic_Unit_Name :=
        Copy (Cloner, Generic_Unit_Name (Source.all), Asis.Element (Target));
      Set_Generic_Actual_Part
        (Target.all,
         Primary_Association_Lists.Deep_Copy 
           (Generic_Actual_Part (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Function_Instantiation_Node
     (The_Context : ASIS.Context)
      return Function_Instantiation_Ptr
   is
      Result : Function_Instantiation_Ptr :=
       new Function_Instantiation_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Function_Instantiation_Node;
  
   function Declaration_Kind (Element : Function_Instantiation_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Function_Instantiation;
   end;

   function Clone
     (Element : Function_Instantiation_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Function_Instantiation_Ptr := new Function_Instantiation_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Corresponding_Body := Element.Corresponding_Body;
      null;
      Result.Generic_Actual := Element.Generic_Actual;
      Result.Specification := Element.Specification;
      Result.Overriding_Indicator_Kind := Element.Overriding_Indicator_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Function_Instantiation_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Generic_Unit_Name :=
        Copy (Cloner, Generic_Unit_Name (Source.all), Asis.Element (Target));
      Set_Generic_Actual_Part
        (Target.all,
         Primary_Association_Lists.Deep_Copy 
           (Generic_Actual_Part (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Formal_Package_Declaration_Node
     (The_Context : ASIS.Context)
      return Formal_Package_Declaration_Ptr
   is
      Result : Formal_Package_Declaration_Ptr :=
       new Formal_Package_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Package_Declaration_Node;
  
   function Declaration_Kind (Element : Formal_Package_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Formal_Package_Declaration;
   end;

   function Clone
     (Element : Formal_Package_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Package_Declaration_Ptr := new Formal_Package_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Corresponding_Body := Element.Corresponding_Body;
      null;
      Result.Generic_Actual := Element.Generic_Actual;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Package_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Generic_Unit_Name :=
        Copy (Cloner, Generic_Unit_Name (Source.all), Asis.Element (Target));
      Set_Generic_Actual_Part
        (Target.all,
         Primary_Association_Lists.Deep_Copy 
           (Generic_Actual_Part (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Parameter_Profile
     (Element : Formal_Procedure_Declaration_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Parameter_Lists.To_Element_List
        (Element.Parameter_Profile, Include_Pragmas);
   end Parameter_Profile;

   procedure Set_Parameter_Profile
     (Element : in out Formal_Procedure_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Parameter_Profile := Primary_Parameter_Lists.List (Value);
   end Set_Parameter_Profile;

   function Parameter_Profile_List
     (Element : Formal_Procedure_Declaration_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Parameter_Profile);
   end Parameter_Profile_List;

   function Default_Kind
     (Element : Formal_Procedure_Declaration_Node) return Asis.Subprogram_Default_Kinds is
   begin
      return Element.Default_Kind;
   end Default_Kind;

   procedure Set_Default_Kind
     (Element : in out Formal_Procedure_Declaration_Node;
      Value   : in     Asis.Subprogram_Default_Kinds) is
   begin
      Element.Default_Kind := Value;
   end Set_Default_Kind;

   function Formal_Subprogram_Default
     (Element : Formal_Procedure_Declaration_Node) return Asis.Expression is
   begin
      return Element.Formal_Subprogram_Default;
   end Formal_Subprogram_Default;

   procedure Set_Formal_Subprogram_Default
     (Element : in out Formal_Procedure_Declaration_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Formal_Subprogram_Default := Value;
   end Set_Formal_Subprogram_Default;

   function Specification
     (Element : Formal_Procedure_Declaration_Node) return Asis.Element is
   begin
      return Element.Specification;
   end Specification;

   procedure Set_Specification
     (Element : in out Formal_Procedure_Declaration_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Specification := Value;
   end Set_Specification;

   function Has_Abstract
     (Element : Formal_Procedure_Declaration_Node) return Boolean is
   begin
      return Element.Has_Abstract;
   end Has_Abstract;

   procedure Set_Has_Abstract
     (Element : in out Formal_Procedure_Declaration_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Abstract := Value;
   end Set_Has_Abstract;

   function Generic_Actual
     (Element : Formal_Procedure_Declaration_Node) return Asis.Expression is
   begin
      return Element.Generic_Actual;
   end Generic_Actual;

   procedure Set_Generic_Actual
     (Element : in out Formal_Procedure_Declaration_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Generic_Actual := Value;
   end Set_Generic_Actual;

   function New_Formal_Procedure_Declaration_Node
     (The_Context : ASIS.Context)
      return Formal_Procedure_Declaration_Ptr
   is
      Result : Formal_Procedure_Declaration_Ptr :=
       new Formal_Procedure_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Procedure_Declaration_Node;
  
   function Declaration_Kind (Element : Formal_Procedure_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Formal_Procedure_Declaration;
   end;

   function Children (Element : access Formal_Procedure_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Parameter_Profile)),
        (False, Element.Formal_Subprogram_Default'Access));
   end Children;

   function Clone
     (Element : Formal_Procedure_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Procedure_Declaration_Ptr := new Formal_Procedure_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Default_Kind := Element.Default_Kind;
      Result.Specification := Element.Specification;
      Result.Has_Abstract := Element.Has_Abstract;
      Result.Generic_Actual := Element.Generic_Actual;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Procedure_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
      Target.Formal_Subprogram_Default :=
        Copy (Cloner, Formal_Subprogram_Default (Source.all), Asis.Element (Target));
   end Copy;

   function Result_Subtype
     (Element : Formal_Function_Declaration_Node) return Asis.Definition is
   begin
      return Element.Result_Subtype;
   end Result_Subtype;

   procedure Set_Result_Subtype
     (Element : in out Formal_Function_Declaration_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Result_Subtype := Value;
   end Set_Result_Subtype;

   function New_Formal_Function_Declaration_Node
     (The_Context : ASIS.Context)
      return Formal_Function_Declaration_Ptr
   is
      Result : Formal_Function_Declaration_Ptr :=
       new Formal_Function_Declaration_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Function_Declaration_Node;
  
   function Declaration_Kind (Element : Formal_Function_Declaration_Node)
      return Asis.Declaration_Kinds is
   begin
      return A_Formal_Function_Declaration;
   end;

   function Children (Element : access Formal_Function_Declaration_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Names)),
        (True, Asis.Element (Element.Parameter_Profile)),
        (False, Element.Formal_Subprogram_Default'Access),
        (False, Element.Result_Subtype'Access));
   end Children;

   function Clone
     (Element : Formal_Function_Declaration_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Function_Declaration_Ptr := new Formal_Function_Declaration_Node;
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
      Result.Declaration_Origin := Element.Declaration_Origin;
      Result.Name := Element.Name;
      null;
      null;
      Result.Place := Element.Place;
      Result.Default_Kind := Element.Default_Kind;
      Result.Specification := Element.Specification;
      Result.Has_Abstract := Element.Has_Abstract;
      Result.Generic_Actual := Element.Generic_Actual;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Function_Declaration_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
      Target.Formal_Subprogram_Default :=
        Copy (Cloner, Formal_Subprogram_Default (Source.all), Asis.Element (Target));
      Target.Result_Subtype :=
        Copy (Cloner, Result_Subtype (Source.all), Asis.Element (Target));
   end Copy;

end Asis.Gela.Elements.Decl;
