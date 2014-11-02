
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

package body Asis.Gela.Elements.Defs is

   function Corresponding_Type_Operators
     (Element : Type_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Declaration_Lists.To_Element_List
        (Element.Corresponding_Type_Operators, Include_Pragmas);
   end Corresponding_Type_Operators;

   procedure Add_To_Corresponding_Type_Operators
     (Element : in out Type_Definition_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Declaration_Lists.Add (Element.Corresponding_Type_Operators, Item);
   end Add_To_Corresponding_Type_Operators;

   function Definition_Kind (Element : Type_Definition_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Type_Definition;
   end;

   function Get_Subtype_Mark
     (Element : Subtype_Indication_Node) return Asis.Expression is
   begin
      return Element.Subtype_Mark;
   end Get_Subtype_Mark;

   procedure Set_Subtype_Mark
     (Element : in out Subtype_Indication_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Subtype_Mark := Value;
   end Set_Subtype_Mark;

   function Subtype_Constraint
     (Element : Subtype_Indication_Node) return Asis.Constraint is
   begin
      return Element.Subtype_Constraint;
   end Subtype_Constraint;

   procedure Set_Subtype_Constraint
     (Element : in out Subtype_Indication_Node;
      Value   : in     Asis.Constraint) is
   begin
      Element.Subtype_Constraint := Value;
   end Set_Subtype_Constraint;

   function Has_Null_Exclusion
     (Element : Subtype_Indication_Node) return Boolean is
   begin
      return Element.Has_Null_Exclusion;
   end Has_Null_Exclusion;

   procedure Set_Has_Null_Exclusion
     (Element : in out Subtype_Indication_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Null_Exclusion := Value;
   end Set_Has_Null_Exclusion;

   function New_Subtype_Indication_Node
     (The_Context : ASIS.Context)
      return Subtype_Indication_Ptr
   is
      Result : Subtype_Indication_Ptr :=
       new Subtype_Indication_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Subtype_Indication_Node;
  
   function Definition_Kind (Element : Subtype_Indication_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Subtype_Indication;
   end;

   function Children (Element : access Subtype_Indication_Node)
     return Traverse_List is
   begin
      return ((False, Element.Subtype_Mark'Access),
        (False, Element.Subtype_Constraint'Access));
   end Children;

   function Clone
     (Element : Subtype_Indication_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Subtype_Indication_Ptr := new Subtype_Indication_Node;
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
      Result.Has_Null_Exclusion := Element.Has_Null_Exclusion;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Subtype_Indication_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Subtype_Mark :=
        Copy (Cloner, Get_Subtype_Mark (Source.all), Asis.Element (Target));
      Target.Subtype_Constraint :=
        Copy (Cloner, Subtype_Constraint (Source.all), Asis.Element (Target));
   end Copy;

   function Definition_Kind (Element : Constraint_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Constraint;
   end;

   function Component_Subtype_Indication
     (Element : Component_Definition_Node) return Asis.Subtype_Indication is
   begin
      return Element.Component_Subtype_Indication;
   end Component_Subtype_Indication;

   procedure Set_Component_Subtype_Indication
     (Element : in out Component_Definition_Node;
      Value   : in     Asis.Subtype_Indication) is
   begin
      Element.Component_Subtype_Indication := Value;
   end Set_Component_Subtype_Indication;

   function Trait_Kind
     (Element : Component_Definition_Node) return Asis.Trait_Kinds is
   begin
      return Element.Trait_Kind;
   end Trait_Kind;

   procedure Set_Trait_Kind
     (Element : in out Component_Definition_Node;
      Value   : in     Asis.Trait_Kinds) is
   begin
      Element.Trait_Kind := Value;
   end Set_Trait_Kind;

   function New_Component_Definition_Node
     (The_Context : ASIS.Context)
      return Component_Definition_Ptr
   is
      Result : Component_Definition_Ptr :=
       new Component_Definition_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Component_Definition_Node;
  
   function Definition_Kind (Element : Component_Definition_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Component_Definition;
   end;

   function Children (Element : access Component_Definition_Node)
     return Traverse_List is
   begin
      return (1 => (False, Element.Component_Subtype_Indication'Access));
   end Children;

   function Clone
     (Element : Component_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Component_Definition_Ptr := new Component_Definition_Node;
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
      Result.Trait_Kind := Element.Trait_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Component_Definition_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Component_Subtype_Indication :=
        Copy (Cloner, Component_Subtype_Indication (Source.all), Asis.Element (Target));
   end Copy;

   function Definition_Kind (Element : Discrete_Subtype_Definition_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Discrete_Subtype_Definition;
   end;

   function Definition_Kind (Element : Discrete_Range_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Discrete_Range;
   end;

   function New_Unknown_Discriminant_Part_Node
     (The_Context : ASIS.Context)
      return Unknown_Discriminant_Part_Ptr
   is
      Result : Unknown_Discriminant_Part_Ptr :=
       new Unknown_Discriminant_Part_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Unknown_Discriminant_Part_Node;
  
   function Definition_Kind (Element : Unknown_Discriminant_Part_Node)
      return Asis.Definition_Kinds is
   begin
      return An_Unknown_Discriminant_Part;
   end;

   function Clone
     (Element : Unknown_Discriminant_Part_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Unknown_Discriminant_Part_Ptr := new Unknown_Discriminant_Part_Node;
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

   function Discriminants
     (Element : Known_Discriminant_Part_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Discriminants, Include_Pragmas);
   end Discriminants;

   procedure Set_Discriminants
     (Element : in out Known_Discriminant_Part_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Discriminants := Primary_Declaration_Lists.List (Value);
   end Set_Discriminants;

   function Discriminants_List
     (Element : Known_Discriminant_Part_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Discriminants);
   end Discriminants_List;

   function New_Known_Discriminant_Part_Node
     (The_Context : ASIS.Context)
      return Known_Discriminant_Part_Ptr
   is
      Result : Known_Discriminant_Part_Ptr :=
       new Known_Discriminant_Part_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Known_Discriminant_Part_Node;
  
   function Definition_Kind (Element : Known_Discriminant_Part_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Known_Discriminant_Part;
   end;

   function Children (Element : access Known_Discriminant_Part_Node)
     return Traverse_List is
   begin
      return (1 => (True, Asis.Element (Element.Discriminants)));
   end Children;

   function Clone
     (Element : Known_Discriminant_Part_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Known_Discriminant_Part_Ptr := new Known_Discriminant_Part_Node;
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
      Target : access Known_Discriminant_Part_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Discriminants
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Discriminants (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Record_Components
     (Element : Record_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Record_Components, Include_Pragmas);
   end Record_Components;

   procedure Set_Record_Components
     (Element : in out Record_Definition_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Record_Components := Primary_Declaration_Lists.List (Value);
   end Set_Record_Components;

   function Record_Components_List
     (Element : Record_Definition_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Record_Components);
   end Record_Components_List;

   function Implicit_Components
     (Element : Record_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Declaration_Lists.To_Element_List
        (Element.Implicit_Components, Include_Pragmas);
   end Implicit_Components;

   procedure Add_To_Implicit_Components
     (Element : in out Record_Definition_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Declaration_Lists.Add (Element.Implicit_Components, Item);
   end Add_To_Implicit_Components;

   function New_Record_Definition_Node
     (The_Context : ASIS.Context)
      return Record_Definition_Ptr
   is
      Result : Record_Definition_Ptr :=
       new Record_Definition_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Record_Definition_Node;
  
   function Definition_Kind (Element : Record_Definition_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Record_Definition;
   end;

   function Children (Element : access Record_Definition_Node)
     return Traverse_List is
   begin
      return (1 => (True, Asis.Element (Element.Record_Components)));
   end Children;

   function Clone
     (Element : Record_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Record_Definition_Ptr := new Record_Definition_Node;
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
      null;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Record_Definition_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Record_Components
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Record_Components (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Null_Record_Definition_Node
     (The_Context : ASIS.Context)
      return Null_Record_Definition_Ptr
   is
      Result : Null_Record_Definition_Ptr :=
       new Null_Record_Definition_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Null_Record_Definition_Node;
  
   function Definition_Kind (Element : Null_Record_Definition_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Null_Record_Definition;
   end;

   function Clone
     (Element : Null_Record_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Null_Record_Definition_Ptr := new Null_Record_Definition_Node;
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

   function Pragmas
     (Element : Null_Component_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Pragma_Lists.To_Element_List
        (Element.Pragmas, Include_Pragmas);
   end Pragmas;

   procedure Set_Pragmas
     (Element : in out Null_Component_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Pragmas := Primary_Pragma_Lists.List (Value);
   end Set_Pragmas;

   function Pragmas_List
     (Element : Null_Component_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Pragmas);
   end Pragmas_List;

   function New_Null_Component_Node
     (The_Context : ASIS.Context)
      return Null_Component_Ptr
   is
      Result : Null_Component_Ptr :=
       new Null_Component_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Null_Component_Node;
  
   function Definition_Kind (Element : Null_Component_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Null_Component;
   end;

   function Clone
     (Element : Null_Component_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Null_Component_Ptr := new Null_Component_Node;
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

   function Discriminant_Direct_Name
     (Element : Variant_Part_Node) return Asis.Name is
   begin
      return Element.Discriminant_Direct_Name;
   end Discriminant_Direct_Name;

   procedure Set_Discriminant_Direct_Name
     (Element : in out Variant_Part_Node;
      Value   : in     Asis.Name) is
   begin
      Element.Discriminant_Direct_Name := Value;
   end Set_Discriminant_Direct_Name;

   function Variants
     (Element : Variant_Part_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Variant_Lists.To_Element_List
        (Element.Variants, Include_Pragmas);
   end Variants;

   procedure Set_Variants
     (Element : in out Variant_Part_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Variants := Primary_Variant_Lists.List (Value);
   end Set_Variants;

   function Variants_List
     (Element : Variant_Part_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Variants);
   end Variants_List;

   function Pragmas
     (Element : Variant_Part_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Pragma_Lists.To_Element_List
        (Element.Pragmas, Include_Pragmas);
   end Pragmas;

   procedure Set_Pragmas
     (Element : in out Variant_Part_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Pragmas := Primary_Pragma_Lists.List (Value);
   end Set_Pragmas;

   function Pragmas_List
     (Element : Variant_Part_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Pragmas);
   end Pragmas_List;

   function End_Pragmas
     (Element : Variant_Part_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Pragma_Lists.To_Element_List
        (Element.End_Pragmas, Include_Pragmas);
   end End_Pragmas;

   procedure Set_End_Pragmas
     (Element : in out Variant_Part_Node;
      Value   : in     Asis.Element) is
   begin
      Element.End_Pragmas := Primary_Pragma_Lists.List (Value);
   end Set_End_Pragmas;

   function End_Pragmas_List
     (Element : Variant_Part_Node) return Asis.Element is
   begin
      return Asis.Element (Element.End_Pragmas);
   end End_Pragmas_List;

   function New_Variant_Part_Node
     (The_Context : ASIS.Context)
      return Variant_Part_Ptr
   is
      Result : Variant_Part_Ptr :=
       new Variant_Part_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Variant_Part_Node;
  
   function Definition_Kind (Element : Variant_Part_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Variant_Part;
   end;

   function Children (Element : access Variant_Part_Node)
     return Traverse_List is
   begin
      return ((False, Element.Discriminant_Direct_Name'Access),
        (True, Asis.Element (Element.Pragmas)),
        (True, Asis.Element (Element.Variants)));
   end Children;

   function Clone
     (Element : Variant_Part_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Variant_Part_Ptr := new Variant_Part_Node;
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
      Target : access Variant_Part_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Discriminant_Direct_Name :=
        Copy (Cloner, Discriminant_Direct_Name (Source.all), Asis.Element (Target));
      Set_Pragmas
        (Target.all,
         Primary_Pragma_Lists.Deep_Copy 
           (Pragmas (Source.all), Cloner, Asis.Element (Target)));
      Set_Variants
        (Target.all,
         Primary_Variant_Lists.Deep_Copy 
           (Variants (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Record_Components
     (Element : Variant_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Record_Components, Include_Pragmas);
   end Record_Components;

   procedure Set_Record_Components
     (Element : in out Variant_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Record_Components := Primary_Declaration_Lists.List (Value);
   end Set_Record_Components;

   function Record_Components_List
     (Element : Variant_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Record_Components);
   end Record_Components_List;

   function Implicit_Components
     (Element : Variant_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Declaration_Lists.To_Element_List
        (Element.Implicit_Components, Include_Pragmas);
   end Implicit_Components;

   procedure Add_To_Implicit_Components
     (Element : in out Variant_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Declaration_Lists.Add (Element.Implicit_Components, Item);
   end Add_To_Implicit_Components;

   function Variant_Choices
     (Element : Variant_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Choise_Lists.To_Element_List
        (Element.Variant_Choices, Include_Pragmas);
   end Variant_Choices;

   procedure Set_Variant_Choices
     (Element : in out Variant_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Variant_Choices := Primary_Choise_Lists.List (Value);
   end Set_Variant_Choices;

   function Variant_Choices_List
     (Element : Variant_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Variant_Choices);
   end Variant_Choices_List;

   function New_Variant_Node
     (The_Context : ASIS.Context)
      return Variant_Ptr
   is
      Result : Variant_Ptr :=
       new Variant_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Variant_Node;
  
   function Definition_Kind (Element : Variant_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Variant;
   end;

   function Children (Element : access Variant_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Variant_Choices)),
        (True, Asis.Element (Element.Record_Components)));
   end Children;

   function Clone
     (Element : Variant_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Variant_Ptr := new Variant_Node;
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
      null;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Variant_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Variant_Choices
        (Target.all,
         Primary_Choise_Lists.Deep_Copy 
           (Variant_Choices (Source.all), Cloner, Asis.Element (Target)));
      Set_Record_Components
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Record_Components (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Others_Choice_Node
     (The_Context : ASIS.Context)
      return Others_Choice_Ptr
   is
      Result : Others_Choice_Ptr :=
       new Others_Choice_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Others_Choice_Node;
  
   function Definition_Kind (Element : Others_Choice_Node)
      return Asis.Definition_Kinds is
   begin
      return An_Others_Choice;
   end;

   function Clone
     (Element : Others_Choice_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Others_Choice_Ptr := new Others_Choice_Node;
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

   function Has_Null_Exclusion
     (Element : Access_Definition_Node) return Boolean is
   begin
      return Element.Has_Null_Exclusion;
   end Has_Null_Exclusion;

   procedure Set_Has_Null_Exclusion
     (Element : in out Access_Definition_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Null_Exclusion := Value;
   end Set_Has_Null_Exclusion;

   function Definition_Kind (Element : Access_Definition_Node)
      return Asis.Definition_Kinds is
   begin
      return An_Access_Definition;
   end;

   function New_Incomplete_Type_Definition_Node
     (The_Context : ASIS.Context)
      return Incomplete_Type_Definition_Ptr
   is
      Result : Incomplete_Type_Definition_Ptr :=
       new Incomplete_Type_Definition_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Incomplete_Type_Definition_Node;
  
   function Definition_Kind (Element : Incomplete_Type_Definition_Node)
      return Asis.Definition_Kinds is
   begin
      return An_Incomplete_Type_Definition;
   end;

   function Clone
     (Element : Incomplete_Type_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Incomplete_Type_Definition_Ptr := new Incomplete_Type_Definition_Node;
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

   function Has_Tagged
     (Element : Tagged_Incomplete_Type_Definition_Node) return Boolean is
   begin
      return Element.Has_Tagged;
   end Has_Tagged;

   procedure Set_Has_Tagged
     (Element : in out Tagged_Incomplete_Type_Definition_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Tagged := Value;
   end Set_Has_Tagged;

   function New_Tagged_Incomplete_Type_Definition_Node
     (The_Context : ASIS.Context)
      return Tagged_Incomplete_Type_Definition_Ptr
   is
      Result : Tagged_Incomplete_Type_Definition_Ptr :=
       new Tagged_Incomplete_Type_Definition_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Tagged_Incomplete_Type_Definition_Node;
  
   function Definition_Kind (Element : Tagged_Incomplete_Type_Definition_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Tagged_Incomplete_Type_Definition;
   end;

   function Clone
     (Element : Tagged_Incomplete_Type_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Tagged_Incomplete_Type_Definition_Ptr := new Tagged_Incomplete_Type_Definition_Node;
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
      Result.Has_Tagged := Element.Has_Tagged;
      return Asis.Element (Result);
   end Clone;

   function Trait_Kind
     (Element : Private_Type_Definition_Node) return Asis.Trait_Kinds is
   begin
      return Element.Trait_Kind;
   end Trait_Kind;

   procedure Set_Trait_Kind
     (Element : in out Private_Type_Definition_Node;
      Value   : in     Asis.Trait_Kinds) is
   begin
      Element.Trait_Kind := Value;
   end Set_Trait_Kind;

   function Corresponding_Type_Operators
     (Element : Private_Type_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Declaration_Lists.To_Element_List
        (Element.Corresponding_Type_Operators, Include_Pragmas);
   end Corresponding_Type_Operators;

   procedure Add_To_Corresponding_Type_Operators
     (Element : in out Private_Type_Definition_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Declaration_Lists.Add (Element.Corresponding_Type_Operators, Item);
   end Add_To_Corresponding_Type_Operators;

   function Has_Limited
     (Element : Private_Type_Definition_Node) return Boolean is
   begin
      return Element.Has_Limited;
   end Has_Limited;

   procedure Set_Has_Limited
     (Element : in out Private_Type_Definition_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Limited := Value;
   end Set_Has_Limited;

   function Has_Private
     (Element : Private_Type_Definition_Node) return Boolean is
   begin
      return Element.Has_Private;
   end Has_Private;

   procedure Set_Has_Private
     (Element : in out Private_Type_Definition_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Private := Value;
   end Set_Has_Private;

   function New_Private_Type_Definition_Node
     (The_Context : ASIS.Context)
      return Private_Type_Definition_Ptr
   is
      Result : Private_Type_Definition_Ptr :=
       new Private_Type_Definition_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Private_Type_Definition_Node;
  
   function Definition_Kind (Element : Private_Type_Definition_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Private_Type_Definition;
   end;

   function Clone
     (Element : Private_Type_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Private_Type_Definition_Ptr := new Private_Type_Definition_Node;
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
      Result.Trait_Kind := Element.Trait_Kind;
      null;
      Result.Has_Limited := Element.Has_Limited;
      Result.Has_Private := Element.Has_Private;
      return Asis.Element (Result);
   end Clone;

   function Has_Abstract
     (Element : Tagged_Private_Type_Definition_Node) return Boolean is
   begin
      return Element.Has_Abstract;
   end Has_Abstract;

   procedure Set_Has_Abstract
     (Element : in out Tagged_Private_Type_Definition_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Abstract := Value;
   end Set_Has_Abstract;

   function Has_Tagged
     (Element : Tagged_Private_Type_Definition_Node) return Boolean is
   begin
      return Element.Has_Tagged;
   end Has_Tagged;

   procedure Set_Has_Tagged
     (Element : in out Tagged_Private_Type_Definition_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Tagged := Value;
   end Set_Has_Tagged;

   function New_Tagged_Private_Type_Definition_Node
     (The_Context : ASIS.Context)
      return Tagged_Private_Type_Definition_Ptr
   is
      Result : Tagged_Private_Type_Definition_Ptr :=
       new Tagged_Private_Type_Definition_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Tagged_Private_Type_Definition_Node;
  
   function Definition_Kind (Element : Tagged_Private_Type_Definition_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Tagged_Private_Type_Definition;
   end;

   function Clone
     (Element : Tagged_Private_Type_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Tagged_Private_Type_Definition_Ptr := new Tagged_Private_Type_Definition_Node;
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
      Result.Trait_Kind := Element.Trait_Kind;
      null;
      Result.Has_Limited := Element.Has_Limited;
      Result.Has_Private := Element.Has_Private;
      Result.Has_Abstract := Element.Has_Abstract;
      Result.Has_Tagged := Element.Has_Tagged;
      return Asis.Element (Result);
   end Clone;

   function Ancestor_Subtype_Indication
     (Element : Private_Extension_Definition_Node) return Asis.Subtype_Indication is
   begin
      return Element.Ancestor_Subtype_Indication;
   end Ancestor_Subtype_Indication;

   procedure Set_Ancestor_Subtype_Indication
     (Element : in out Private_Extension_Definition_Node;
      Value   : in     Asis.Subtype_Indication) is
   begin
      Element.Ancestor_Subtype_Indication := Value;
   end Set_Ancestor_Subtype_Indication;

   function Implicit_Inherited_Declarations
     (Element : Private_Extension_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Declaration_Lists.To_Element_List
        (Element.Implicit_Inherited_Declarations, Include_Pragmas);
   end Implicit_Inherited_Declarations;

   procedure Add_To_Implicit_Inherited_Declarations
     (Element : in out Private_Extension_Definition_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Declaration_Lists.Add (Element.Implicit_Inherited_Declarations, Item);
   end Add_To_Implicit_Inherited_Declarations;

   function Implicit_Inherited_Subprograms
     (Element : Private_Extension_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Declaration_Lists.To_Element_List
        (Element.Implicit_Inherited_Subprograms, Include_Pragmas);
   end Implicit_Inherited_Subprograms;

   procedure Add_To_Implicit_Inherited_Subprograms
     (Element : in out Private_Extension_Definition_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Declaration_Lists.Add (Element.Implicit_Inherited_Subprograms, Item);
   end Add_To_Implicit_Inherited_Subprograms;

   function Has_Synchronized
     (Element : Private_Extension_Definition_Node) return Boolean is
   begin
      return Element.Has_Synchronized;
   end Has_Synchronized;

   procedure Set_Has_Synchronized
     (Element : in out Private_Extension_Definition_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Synchronized := Value;
   end Set_Has_Synchronized;

   function Has_Abstract
     (Element : Private_Extension_Definition_Node) return Boolean is
   begin
      return Element.Has_Abstract;
   end Has_Abstract;

   procedure Set_Has_Abstract
     (Element : in out Private_Extension_Definition_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Abstract := Value;
   end Set_Has_Abstract;

   function New_Private_Extension_Definition_Node
     (The_Context : ASIS.Context)
      return Private_Extension_Definition_Ptr
   is
      Result : Private_Extension_Definition_Ptr :=
       new Private_Extension_Definition_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Private_Extension_Definition_Node;
  
   function Definition_Kind (Element : Private_Extension_Definition_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Private_Extension_Definition;
   end;

   function Children (Element : access Private_Extension_Definition_Node)
     return Traverse_List is
   begin
      return (1 => (False, Element.Ancestor_Subtype_Indication'Access));
   end Children;

   function Clone
     (Element : Private_Extension_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Private_Extension_Definition_Ptr := new Private_Extension_Definition_Node;
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
      Result.Trait_Kind := Element.Trait_Kind;
      null;
      Result.Has_Limited := Element.Has_Limited;
      Result.Has_Private := Element.Has_Private;
      null;
      null;
      Result.Has_Synchronized := Element.Has_Synchronized;
      Result.Has_Abstract := Element.Has_Abstract;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Private_Extension_Definition_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Ancestor_Subtype_Indication :=
        Copy (Cloner, Ancestor_Subtype_Indication (Source.all), Asis.Element (Target));
   end Copy;

   function Is_Private_Present
     (Element : Protected_Definition_Node) return Boolean is
   begin
      return Element.Is_Private_Present;
   end Is_Private_Present;

   procedure Set_Is_Private_Present
     (Element : in out Protected_Definition_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Private_Present := Value;
   end Set_Is_Private_Present;

   function Visible_Part_Items
     (Element : Protected_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Visible_Part_Items, Include_Pragmas);
   end Visible_Part_Items;

   procedure Set_Visible_Part_Items
     (Element : in out Protected_Definition_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Visible_Part_Items := Primary_Declaration_Lists.List (Value);
   end Set_Visible_Part_Items;

   function Visible_Part_Items_List
     (Element : Protected_Definition_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Visible_Part_Items);
   end Visible_Part_Items_List;

   function Private_Part_Items
     (Element : Protected_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Private_Part_Items, Include_Pragmas);
   end Private_Part_Items;

   procedure Set_Private_Part_Items
     (Element : in out Protected_Definition_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Private_Part_Items := Primary_Declaration_Lists.List (Value);
   end Set_Private_Part_Items;

   function Private_Part_Items_List
     (Element : Protected_Definition_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Private_Part_Items);
   end Private_Part_Items_List;

   function Get_Identifier
     (Element : Protected_Definition_Node) return Asis.Element is
   begin
      return Element.Identifier;
   end Get_Identifier;

   procedure Set_Identifier
     (Element : in out Protected_Definition_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Identifier := Value;
   end Set_Identifier;

   function Corresponding_Type_Operators
     (Element : Protected_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Declaration_Lists.To_Element_List
        (Element.Corresponding_Type_Operators, Include_Pragmas);
   end Corresponding_Type_Operators;

   procedure Add_To_Corresponding_Type_Operators
     (Element : in out Protected_Definition_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Declaration_Lists.Add (Element.Corresponding_Type_Operators, Item);
   end Add_To_Corresponding_Type_Operators;

   function New_Protected_Definition_Node
     (The_Context : ASIS.Context)
      return Protected_Definition_Ptr
   is
      Result : Protected_Definition_Ptr :=
       new Protected_Definition_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Protected_Definition_Node;
  
   function Definition_Kind (Element : Protected_Definition_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Protected_Definition;
   end;

   function Children (Element : access Protected_Definition_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Visible_Part_Items)),
        (True, Asis.Element (Element.Private_Part_Items)));
   end Children;

   function Clone
     (Element : Protected_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Protected_Definition_Ptr := new Protected_Definition_Node;
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
      Result.Is_Private_Present := Element.Is_Private_Present;
      Result.Identifier := Element.Identifier;
      null;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Protected_Definition_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Visible_Part_Items
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Visible_Part_Items (Source.all), Cloner, Asis.Element (Target)));
      Set_Private_Part_Items
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Private_Part_Items (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Is_Task_Definition_Present
     (Element : Task_Definition_Node) return Boolean is
   begin
      return Element.Is_Task_Definition_Present;
   end Is_Task_Definition_Present;

   procedure Set_Is_Task_Definition_Present
     (Element : in out Task_Definition_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Task_Definition_Present := Value;
   end Set_Is_Task_Definition_Present;

   function New_Task_Definition_Node
     (The_Context : ASIS.Context)
      return Task_Definition_Ptr
   is
      Result : Task_Definition_Ptr :=
       new Task_Definition_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Task_Definition_Node;
  
   function Definition_Kind (Element : Task_Definition_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Task_Definition;
   end;

   function Clone
     (Element : Task_Definition_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Task_Definition_Ptr := new Task_Definition_Node;
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
      Result.Is_Private_Present := Element.Is_Private_Present;
      Result.Identifier := Element.Identifier;
      null;
      Result.Is_Task_Definition_Present := Element.Is_Task_Definition_Present;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Task_Definition_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Visible_Part_Items
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Visible_Part_Items (Source.all), Cloner, Asis.Element (Target)));
      Set_Private_Part_Items
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Private_Part_Items (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Corresponding_Type_Operators
     (Element : Formal_Type_Definition_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Declaration_Lists.To_Element_List
        (Element.Corresponding_Type_Operators, Include_Pragmas);
   end Corresponding_Type_Operators;

   procedure Add_To_Corresponding_Type_Operators
     (Element : in out Formal_Type_Definition_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Declaration_Lists.Add (Element.Corresponding_Type_Operators, Item);
   end Add_To_Corresponding_Type_Operators;

   function Definition_Kind (Element : Formal_Type_Definition_Node)
      return Asis.Definition_Kinds is
   begin
      return A_Formal_Type_Definition;
   end;

end Asis.Gela.Elements.Defs;
