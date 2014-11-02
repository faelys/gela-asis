
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

package body Asis.Gela.Elements.Defs.Types is

   function Corresponding_Parent_Subtype
     (Element : Derived_Type_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Parent_Subtype;
   end Corresponding_Parent_Subtype;

   procedure Set_Corresponding_Parent_Subtype
     (Element : in out Derived_Type_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Parent_Subtype := Value;
   end Set_Corresponding_Parent_Subtype;

   function Corresponding_Root_Type
     (Element : Derived_Type_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Root_Type;
   end Corresponding_Root_Type;

   procedure Set_Corresponding_Root_Type
     (Element : in out Derived_Type_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Root_Type := Value;
   end Set_Corresponding_Root_Type;

   function Implicit_Inherited_Declarations
     (Element : Derived_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Declaration_Lists.To_Element_List
        (Element.Implicit_Inherited_Declarations, Include_Pragmas);
   end Implicit_Inherited_Declarations;

   procedure Add_To_Implicit_Inherited_Declarations
     (Element : in out Derived_Type_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Declaration_Lists.Add (Element.Implicit_Inherited_Declarations, Item);
   end Add_To_Implicit_Inherited_Declarations;

   function Implicit_Inherited_Subprograms
     (Element : Derived_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Declaration_Lists.To_Element_List
        (Element.Implicit_Inherited_Subprograms, Include_Pragmas);
   end Implicit_Inherited_Subprograms;

   procedure Add_To_Implicit_Inherited_Subprograms
     (Element : in out Derived_Type_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Declaration_Lists.Add (Element.Implicit_Inherited_Subprograms, Item);
   end Add_To_Implicit_Inherited_Subprograms;

   function Corresponding_Type_Structure
     (Element : Derived_Type_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Type_Structure;
   end Corresponding_Type_Structure;

   procedure Set_Corresponding_Type_Structure
     (Element : in out Derived_Type_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Type_Structure := Value;
   end Set_Corresponding_Type_Structure;

   function Trait_Kind
     (Element : Derived_Type_Node) return Asis.Trait_Kinds is
   begin
      return Element.Trait_Kind;
   end Trait_Kind;

   procedure Set_Trait_Kind
     (Element : in out Derived_Type_Node;
      Value   : in     Asis.Trait_Kinds) is
   begin
      Element.Trait_Kind := Value;
   end Set_Trait_Kind;

   function Parent_Subtype_Indication
     (Element : Derived_Type_Node) return Asis.Subtype_Indication is
   begin
      return Element.Parent_Subtype_Indication;
   end Parent_Subtype_Indication;

   procedure Set_Parent_Subtype_Indication
     (Element : in out Derived_Type_Node;
      Value   : in     Asis.Subtype_Indication) is
   begin
      Element.Parent_Subtype_Indication := Value;
   end Set_Parent_Subtype_Indication;

   function Has_Limited
     (Element : Derived_Type_Node) return Boolean is
   begin
      return Element.Has_Limited;
   end Has_Limited;

   procedure Set_Has_Limited
     (Element : in out Derived_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Limited := Value;
   end Set_Has_Limited;

   function Has_Abstract
     (Element : Derived_Type_Node) return Boolean is
   begin
      return Element.Has_Abstract;
   end Has_Abstract;

   procedure Set_Has_Abstract
     (Element : in out Derived_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Abstract := Value;
   end Set_Has_Abstract;

   function New_Derived_Type_Node
     (The_Context : ASIS.Context)
      return Derived_Type_Ptr
   is
      Result : Derived_Type_Ptr :=
       new Derived_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Derived_Type_Node;
  
   function Type_Definition_Kind (Element : Derived_Type_Node)
      return Asis.Type_Kinds is
   begin
      return A_Derived_Type_Definition;
   end;

   function Children (Element : access Derived_Type_Node)
     return Traverse_List is
   begin
      return (1 => (False, Element.Parent_Subtype_Indication'Access));
   end Children;

   function Clone
     (Element : Derived_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Derived_Type_Ptr := new Derived_Type_Node;
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
      Result.Corresponding_Parent_Subtype := Element.Corresponding_Parent_Subtype;
      Result.Corresponding_Root_Type := Element.Corresponding_Root_Type;
      null;
      null;
      Result.Corresponding_Type_Structure := Element.Corresponding_Type_Structure;
      Result.Trait_Kind := Element.Trait_Kind;
      Result.Has_Limited := Element.Has_Limited;
      Result.Has_Abstract := Element.Has_Abstract;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Derived_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Parent_Subtype_Indication :=
        Copy (Cloner, Parent_Subtype_Indication (Source.all), Asis.Element (Target));
   end Copy;

   function Progenitor_List
     (Element : Derived_Record_Extension_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Expression_Lists.To_Element_List
        (Element.Progenitor_List, Include_Pragmas);
   end Progenitor_List;

   procedure Set_Progenitor_List
     (Element : in out Derived_Record_Extension_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Progenitor_List := Primary_Expression_Lists.List (Value);
   end Set_Progenitor_List;

   function Progenitor_List_List
     (Element : Derived_Record_Extension_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Progenitor_List);
   end Progenitor_List_List;

   function Get_Record_Definition
     (Element : Derived_Record_Extension_Node) return Asis.Definition is
   begin
      return Element.Record_Definition;
   end Get_Record_Definition;

   procedure Set_Record_Definition
     (Element : in out Derived_Record_Extension_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Record_Definition := Value;
   end Set_Record_Definition;

   function New_Derived_Record_Extension_Node
     (The_Context : ASIS.Context)
      return Derived_Record_Extension_Ptr
   is
      Result : Derived_Record_Extension_Ptr :=
       new Derived_Record_Extension_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Derived_Record_Extension_Node;
  
   function Type_Definition_Kind (Element : Derived_Record_Extension_Node)
      return Asis.Type_Kinds is
   begin
      return A_Derived_Record_Extension_Definition;
   end;

   function Children (Element : access Derived_Record_Extension_Node)
     return Traverse_List is
   begin
      return ((False, Element.Parent_Subtype_Indication'Access),
        (True, Asis.Element (Element.Progenitor_List)),
        (False, Element.Record_Definition'Access));
   end Children;

   function Clone
     (Element : Derived_Record_Extension_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Derived_Record_Extension_Ptr := new Derived_Record_Extension_Node;
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
      Result.Corresponding_Parent_Subtype := Element.Corresponding_Parent_Subtype;
      Result.Corresponding_Root_Type := Element.Corresponding_Root_Type;
      null;
      null;
      Result.Corresponding_Type_Structure := Element.Corresponding_Type_Structure;
      Result.Trait_Kind := Element.Trait_Kind;
      Result.Has_Limited := Element.Has_Limited;
      Result.Has_Abstract := Element.Has_Abstract;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Derived_Record_Extension_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Parent_Subtype_Indication :=
        Copy (Cloner, Parent_Subtype_Indication (Source.all), Asis.Element (Target));
      Set_Progenitor_List
        (Target.all,
         Primary_Expression_Lists.Deep_Copy 
           (Progenitor_List (Source.all), Cloner, Asis.Element (Target)));
      Target.Record_Definition :=
        Copy (Cloner, Get_Record_Definition (Source.all), Asis.Element (Target));
   end Copy;

   function Enumeration_Literal_Declarations
     (Element : Enumeration_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Enumeration_Literal_Declarations, Include_Pragmas);
   end Enumeration_Literal_Declarations;

   procedure Set_Enumeration_Literal_Declarations
     (Element : in out Enumeration_Type_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Enumeration_Literal_Declarations := Primary_Declaration_Lists.List (Value);
   end Set_Enumeration_Literal_Declarations;

   function Enumeration_Literal_Declarations_List
     (Element : Enumeration_Type_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Enumeration_Literal_Declarations);
   end Enumeration_Literal_Declarations_List;

   function New_Enumeration_Type_Node
     (The_Context : ASIS.Context)
      return Enumeration_Type_Ptr
   is
      Result : Enumeration_Type_Ptr :=
       new Enumeration_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Enumeration_Type_Node;
  
   function Type_Definition_Kind (Element : Enumeration_Type_Node)
      return Asis.Type_Kinds is
   begin
      return An_Enumeration_Type_Definition;
   end;

   function Children (Element : access Enumeration_Type_Node)
     return Traverse_List is
   begin
      return (1 => (True, Asis.Element (Element.Enumeration_Literal_Declarations)));
   end Children;

   function Clone
     (Element : Enumeration_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Enumeration_Type_Ptr := new Enumeration_Type_Node;
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
      Target : access Enumeration_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Enumeration_Literal_Declarations
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Enumeration_Literal_Declarations (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Integer_Constraint
     (Element : Signed_Integer_Type_Node) return Asis.Range_Constraint is
   begin
      return Element.Integer_Constraint;
   end Integer_Constraint;

   procedure Set_Integer_Constraint
     (Element : in out Signed_Integer_Type_Node;
      Value   : in     Asis.Range_Constraint) is
   begin
      Element.Integer_Constraint := Value;
   end Set_Integer_Constraint;

   function New_Signed_Integer_Type_Node
     (The_Context : ASIS.Context)
      return Signed_Integer_Type_Ptr
   is
      Result : Signed_Integer_Type_Ptr :=
       new Signed_Integer_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Signed_Integer_Type_Node;
  
   function Type_Definition_Kind (Element : Signed_Integer_Type_Node)
      return Asis.Type_Kinds is
   begin
      return A_Signed_Integer_Type_Definition;
   end;

   function Children (Element : access Signed_Integer_Type_Node)
     return Traverse_List is
   begin
      return (1 => (False, Element.Integer_Constraint'Access));
   end Children;

   function Clone
     (Element : Signed_Integer_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Signed_Integer_Type_Ptr := new Signed_Integer_Type_Node;
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
      Target : access Signed_Integer_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Integer_Constraint :=
        Copy (Cloner, Integer_Constraint (Source.all), Asis.Element (Target));
   end Copy;

   function Mod_Static_Expression
     (Element : Modular_Type_Node) return Asis.Expression is
   begin
      return Element.Mod_Static_Expression;
   end Mod_Static_Expression;

   procedure Set_Mod_Static_Expression
     (Element : in out Modular_Type_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Mod_Static_Expression := Value;
   end Set_Mod_Static_Expression;

   function New_Modular_Type_Node
     (The_Context : ASIS.Context)
      return Modular_Type_Ptr
   is
      Result : Modular_Type_Ptr :=
       new Modular_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Modular_Type_Node;
  
   function Type_Definition_Kind (Element : Modular_Type_Node)
      return Asis.Type_Kinds is
   begin
      return A_Modular_Type_Definition;
   end;

   function Children (Element : access Modular_Type_Node)
     return Traverse_List is
   begin
      return (1 => (False, Element.Mod_Static_Expression'Access));
   end Children;

   function Clone
     (Element : Modular_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Modular_Type_Ptr := new Modular_Type_Node;
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
      Target : access Modular_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Mod_Static_Expression :=
        Copy (Cloner, Mod_Static_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function Root_Type_Kind
     (Element : Root_Type_Node) return Asis.Root_Type_Kinds is
   begin
      return Element.Root_Type_Kind;
   end Root_Type_Kind;

   procedure Set_Root_Type_Kind
     (Element : in out Root_Type_Node;
      Value   : in     Asis.Root_Type_Kinds) is
   begin
      Element.Root_Type_Kind := Value;
   end Set_Root_Type_Kind;

   function New_Root_Type_Node
     (The_Context : ASIS.Context)
      return Root_Type_Ptr
   is
      Result : Root_Type_Ptr :=
       new Root_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Root_Type_Node;
  
   function Type_Definition_Kind (Element : Root_Type_Node)
      return Asis.Type_Kinds is
   begin
      return A_Root_Type_Definition;
   end;

   function Clone
     (Element : Root_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Root_Type_Ptr := new Root_Type_Node;
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
      Result.Root_Type_Kind := Element.Root_Type_Kind;
      return Asis.Element (Result);
   end Clone;

   function Digits_Expression
     (Element : Floating_Point_Node) return Asis.Expression is
   begin
      return Element.Digits_Expression;
   end Digits_Expression;

   procedure Set_Digits_Expression
     (Element : in out Floating_Point_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Digits_Expression := Value;
   end Set_Digits_Expression;

   function Real_Range_Constraint
     (Element : Floating_Point_Node) return Asis.Range_Constraint is
   begin
      return Element.Real_Range_Constraint;
   end Real_Range_Constraint;

   procedure Set_Real_Range_Constraint
     (Element : in out Floating_Point_Node;
      Value   : in     Asis.Range_Constraint) is
   begin
      Element.Real_Range_Constraint := Value;
   end Set_Real_Range_Constraint;

   function New_Floating_Point_Node
     (The_Context : ASIS.Context)
      return Floating_Point_Ptr
   is
      Result : Floating_Point_Ptr :=
       new Floating_Point_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Floating_Point_Node;
  
   function Type_Definition_Kind (Element : Floating_Point_Node)
      return Asis.Type_Kinds is
   begin
      return A_Floating_Point_Definition;
   end;

   function Children (Element : access Floating_Point_Node)
     return Traverse_List is
   begin
      return ((False, Element.Digits_Expression'Access),
        (False, Element.Real_Range_Constraint'Access));
   end Children;

   function Clone
     (Element : Floating_Point_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Floating_Point_Ptr := new Floating_Point_Node;
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
      Target : access Floating_Point_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Digits_Expression :=
        Copy (Cloner, Digits_Expression (Source.all), Asis.Element (Target));
      Target.Real_Range_Constraint :=
        Copy (Cloner, Real_Range_Constraint (Source.all), Asis.Element (Target));
   end Copy;

   function Delta_Expression
     (Element : Ordinary_Fixed_Point_Node) return Asis.Expression is
   begin
      return Element.Delta_Expression;
   end Delta_Expression;

   procedure Set_Delta_Expression
     (Element : in out Ordinary_Fixed_Point_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Delta_Expression := Value;
   end Set_Delta_Expression;

   function Real_Range_Constraint
     (Element : Ordinary_Fixed_Point_Node) return Asis.Range_Constraint is
   begin
      return Element.Real_Range_Constraint;
   end Real_Range_Constraint;

   procedure Set_Real_Range_Constraint
     (Element : in out Ordinary_Fixed_Point_Node;
      Value   : in     Asis.Range_Constraint) is
   begin
      Element.Real_Range_Constraint := Value;
   end Set_Real_Range_Constraint;

   function New_Ordinary_Fixed_Point_Node
     (The_Context : ASIS.Context)
      return Ordinary_Fixed_Point_Ptr
   is
      Result : Ordinary_Fixed_Point_Ptr :=
       new Ordinary_Fixed_Point_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Ordinary_Fixed_Point_Node;
  
   function Type_Definition_Kind (Element : Ordinary_Fixed_Point_Node)
      return Asis.Type_Kinds is
   begin
      return An_Ordinary_Fixed_Point_Definition;
   end;

   function Children (Element : access Ordinary_Fixed_Point_Node)
     return Traverse_List is
   begin
      return ((False, Element.Delta_Expression'Access),
        (False, Element.Real_Range_Constraint'Access));
   end Children;

   function Clone
     (Element : Ordinary_Fixed_Point_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Ordinary_Fixed_Point_Ptr := new Ordinary_Fixed_Point_Node;
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
      Target : access Ordinary_Fixed_Point_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Delta_Expression :=
        Copy (Cloner, Delta_Expression (Source.all), Asis.Element (Target));
      Target.Real_Range_Constraint :=
        Copy (Cloner, Real_Range_Constraint (Source.all), Asis.Element (Target));
   end Copy;

   function Digits_Expression
     (Element : Decimal_Fixed_Point_Node) return Asis.Expression is
   begin
      return Element.Digits_Expression;
   end Digits_Expression;

   procedure Set_Digits_Expression
     (Element : in out Decimal_Fixed_Point_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Digits_Expression := Value;
   end Set_Digits_Expression;

   function New_Decimal_Fixed_Point_Node
     (The_Context : ASIS.Context)
      return Decimal_Fixed_Point_Ptr
   is
      Result : Decimal_Fixed_Point_Ptr :=
       new Decimal_Fixed_Point_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Decimal_Fixed_Point_Node;
  
   function Type_Definition_Kind (Element : Decimal_Fixed_Point_Node)
      return Asis.Type_Kinds is
   begin
      return A_Decimal_Fixed_Point_Definition;
   end;

   function Children (Element : access Decimal_Fixed_Point_Node)
     return Traverse_List is
   begin
      return ((False, Element.Delta_Expression'Access),
        (False, Element.Digits_Expression'Access),
        (False, Element.Real_Range_Constraint'Access));
   end Children;

   function Clone
     (Element : Decimal_Fixed_Point_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Decimal_Fixed_Point_Ptr := new Decimal_Fixed_Point_Node;
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
      Target : access Decimal_Fixed_Point_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Delta_Expression :=
        Copy (Cloner, Delta_Expression (Source.all), Asis.Element (Target));
      Target.Digits_Expression :=
        Copy (Cloner, Digits_Expression (Source.all), Asis.Element (Target));
      Target.Real_Range_Constraint :=
        Copy (Cloner, Real_Range_Constraint (Source.all), Asis.Element (Target));
   end Copy;

   function Index_Subtype_Definitions
     (Element : Unconstrained_Array_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Identifier_Lists.To_Element_List
        (Element.Index_Subtype_Definitions, Include_Pragmas);
   end Index_Subtype_Definitions;

   procedure Set_Index_Subtype_Definitions
     (Element : in out Unconstrained_Array_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Index_Subtype_Definitions := Primary_Identifier_Lists.List (Value);
   end Set_Index_Subtype_Definitions;

   function Index_Subtype_Definitions_List
     (Element : Unconstrained_Array_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Index_Subtype_Definitions);
   end Index_Subtype_Definitions_List;

   function Array_Component_Definition
     (Element : Unconstrained_Array_Node) return Asis.Component_Definition is
   begin
      return Element.Array_Component_Definition;
   end Array_Component_Definition;

   procedure Set_Array_Component_Definition
     (Element : in out Unconstrained_Array_Node;
      Value   : in     Asis.Component_Definition) is
   begin
      Element.Array_Component_Definition := Value;
   end Set_Array_Component_Definition;

   function New_Unconstrained_Array_Node
     (The_Context : ASIS.Context)
      return Unconstrained_Array_Ptr
   is
      Result : Unconstrained_Array_Ptr :=
       new Unconstrained_Array_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Unconstrained_Array_Node;
  
   function Type_Definition_Kind (Element : Unconstrained_Array_Node)
      return Asis.Type_Kinds is
   begin
      return An_Unconstrained_Array_Definition;
   end;

   function Children (Element : access Unconstrained_Array_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Index_Subtype_Definitions)),
        (False, Element.Array_Component_Definition'Access));
   end Children;

   function Clone
     (Element : Unconstrained_Array_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Unconstrained_Array_Ptr := new Unconstrained_Array_Node;
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
      Target : access Unconstrained_Array_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Index_Subtype_Definitions
        (Target.all,
         Primary_Identifier_Lists.Deep_Copy 
           (Index_Subtype_Definitions (Source.all), Cloner, Asis.Element (Target)));
      Target.Array_Component_Definition :=
        Copy (Cloner, Array_Component_Definition (Source.all), Asis.Element (Target));
   end Copy;

   function Discrete_Subtype_Definitions
     (Element : Constrained_Array_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Definition_Lists.To_Element_List
        (Element.Discrete_Subtype_Definitions, Include_Pragmas);
   end Discrete_Subtype_Definitions;

   procedure Set_Discrete_Subtype_Definitions
     (Element : in out Constrained_Array_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Discrete_Subtype_Definitions := Primary_Definition_Lists.List (Value);
   end Set_Discrete_Subtype_Definitions;

   function Discrete_Subtype_Definitions_List
     (Element : Constrained_Array_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Discrete_Subtype_Definitions);
   end Discrete_Subtype_Definitions_List;

   function Array_Component_Definition
     (Element : Constrained_Array_Node) return Asis.Component_Definition is
   begin
      return Element.Array_Component_Definition;
   end Array_Component_Definition;

   procedure Set_Array_Component_Definition
     (Element : in out Constrained_Array_Node;
      Value   : in     Asis.Component_Definition) is
   begin
      Element.Array_Component_Definition := Value;
   end Set_Array_Component_Definition;

   function New_Constrained_Array_Node
     (The_Context : ASIS.Context)
      return Constrained_Array_Ptr
   is
      Result : Constrained_Array_Ptr :=
       new Constrained_Array_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Constrained_Array_Node;
  
   function Type_Definition_Kind (Element : Constrained_Array_Node)
      return Asis.Type_Kinds is
   begin
      return A_Constrained_Array_Definition;
   end;

   function Children (Element : access Constrained_Array_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Discrete_Subtype_Definitions)),
        (False, Element.Array_Component_Definition'Access));
   end Children;

   function Clone
     (Element : Constrained_Array_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Constrained_Array_Ptr := new Constrained_Array_Node;
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
      Target : access Constrained_Array_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Discrete_Subtype_Definitions
        (Target.all,
         Primary_Definition_Lists.Deep_Copy 
           (Discrete_Subtype_Definitions (Source.all), Cloner, Asis.Element (Target)));
      Target.Array_Component_Definition :=
        Copy (Cloner, Array_Component_Definition (Source.all), Asis.Element (Target));
   end Copy;

   function Trait_Kind
     (Element : Record_Type_Node) return Asis.Trait_Kinds is
   begin
      return Element.Trait_Kind;
   end Trait_Kind;

   procedure Set_Trait_Kind
     (Element : in out Record_Type_Node;
      Value   : in     Asis.Trait_Kinds) is
   begin
      Element.Trait_Kind := Value;
   end Set_Trait_Kind;

   function Get_Record_Definition
     (Element : Record_Type_Node) return Asis.Definition is
   begin
      return Element.Record_Definition;
   end Get_Record_Definition;

   procedure Set_Record_Definition
     (Element : in out Record_Type_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Record_Definition := Value;
   end Set_Record_Definition;

   function Has_Limited
     (Element : Record_Type_Node) return Boolean is
   begin
      return Element.Has_Limited;
   end Has_Limited;

   procedure Set_Has_Limited
     (Element : in out Record_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Limited := Value;
   end Set_Has_Limited;

   function New_Record_Type_Node
     (The_Context : ASIS.Context)
      return Record_Type_Ptr
   is
      Result : Record_Type_Ptr :=
       new Record_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Record_Type_Node;
  
   function Type_Definition_Kind (Element : Record_Type_Node)
      return Asis.Type_Kinds is
   begin
      return A_Record_Type_Definition;
   end;

   function Children (Element : access Record_Type_Node)
     return Traverse_List is
   begin
      return (1 => (False, Element.Record_Definition'Access));
   end Children;

   function Clone
     (Element : Record_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Record_Type_Ptr := new Record_Type_Node;
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
      Result.Trait_Kind := Element.Trait_Kind;
      Result.Has_Limited := Element.Has_Limited;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Record_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Record_Definition :=
        Copy (Cloner, Get_Record_Definition (Source.all), Asis.Element (Target));
   end Copy;

   function Has_Abstract
     (Element : Tagged_Record_Type_Node) return Boolean is
   begin
      return Element.Has_Abstract;
   end Has_Abstract;

   procedure Set_Has_Abstract
     (Element : in out Tagged_Record_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Abstract := Value;
   end Set_Has_Abstract;

   function Has_Tagged
     (Element : Tagged_Record_Type_Node) return Boolean is
   begin
      return Element.Has_Tagged;
   end Has_Tagged;

   procedure Set_Has_Tagged
     (Element : in out Tagged_Record_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Tagged := Value;
   end Set_Has_Tagged;

   function New_Tagged_Record_Type_Node
     (The_Context : ASIS.Context)
      return Tagged_Record_Type_Ptr
   is
      Result : Tagged_Record_Type_Ptr :=
       new Tagged_Record_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Tagged_Record_Type_Node;
  
   function Type_Definition_Kind (Element : Tagged_Record_Type_Node)
      return Asis.Type_Kinds is
   begin
      return A_Tagged_Record_Type_Definition;
   end;

   function Clone
     (Element : Tagged_Record_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Tagged_Record_Type_Ptr := new Tagged_Record_Type_Node;
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
      Result.Trait_Kind := Element.Trait_Kind;
      Result.Has_Limited := Element.Has_Limited;
      Result.Has_Abstract := Element.Has_Abstract;
      Result.Has_Tagged := Element.Has_Tagged;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Tagged_Record_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Record_Definition :=
        Copy (Cloner, Get_Record_Definition (Source.all), Asis.Element (Target));
   end Copy;

   function Interface_Kind
     (Element : Interface_Type_Node) return Asis.Interface_Kinds is
   begin
      return Element.Interface_Kind;
   end Interface_Kind;

   procedure Set_Interface_Kind
     (Element : in out Interface_Type_Node;
      Value   : in     Asis.Interface_Kinds) is
   begin
      Element.Interface_Kind := Value;
   end Set_Interface_Kind;

   function Has_Limited
     (Element : Interface_Type_Node) return Boolean is
   begin
      return Element.Has_Limited;
   end Has_Limited;

   procedure Set_Has_Limited
     (Element : in out Interface_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Limited := Value;
   end Set_Has_Limited;

   function Has_Synchronized
     (Element : Interface_Type_Node) return Boolean is
   begin
      return Element.Has_Synchronized;
   end Has_Synchronized;

   procedure Set_Has_Synchronized
     (Element : in out Interface_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Synchronized := Value;
   end Set_Has_Synchronized;

   function Has_Protected
     (Element : Interface_Type_Node) return Boolean is
   begin
      return Element.Has_Protected;
   end Has_Protected;

   procedure Set_Has_Protected
     (Element : in out Interface_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Protected := Value;
   end Set_Has_Protected;

   function Has_Task
     (Element : Interface_Type_Node) return Boolean is
   begin
      return Element.Has_Task;
   end Has_Task;

   procedure Set_Has_Task
     (Element : in out Interface_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Task := Value;
   end Set_Has_Task;

   function Progenitor_List
     (Element : Interface_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Expression_Lists.To_Element_List
        (Element.Progenitor_List, Include_Pragmas);
   end Progenitor_List;

   procedure Set_Progenitor_List
     (Element : in out Interface_Type_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Progenitor_List := Primary_Expression_Lists.List (Value);
   end Set_Progenitor_List;

   function Progenitor_List_List
     (Element : Interface_Type_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Progenitor_List);
   end Progenitor_List_List;

   function Implicit_Inherited_Subprograms
     (Element : Interface_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Declaration_Lists.To_Element_List
        (Element.Implicit_Inherited_Subprograms, Include_Pragmas);
   end Implicit_Inherited_Subprograms;

   procedure Add_To_Implicit_Inherited_Subprograms
     (Element : in out Interface_Type_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Declaration_Lists.Add (Element.Implicit_Inherited_Subprograms, Item);
   end Add_To_Implicit_Inherited_Subprograms;

   function New_Interface_Type_Node
     (The_Context : ASIS.Context)
      return Interface_Type_Ptr
   is
      Result : Interface_Type_Ptr :=
       new Interface_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Interface_Type_Node;
  
   function Type_Definition_Kind (Element : Interface_Type_Node)
      return Asis.Type_Kinds is
   begin
      return An_Interface_Type_Definition;
   end;

   function Children (Element : access Interface_Type_Node)
     return Traverse_List is
   begin
      return (1 => (True, Asis.Element (Element.Progenitor_List)));
   end Children;

   function Clone
     (Element : Interface_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Interface_Type_Ptr := new Interface_Type_Node;
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
      Result.Interface_Kind := Element.Interface_Kind;
      Result.Has_Limited := Element.Has_Limited;
      Result.Has_Synchronized := Element.Has_Synchronized;
      Result.Has_Protected := Element.Has_Protected;
      Result.Has_Task := Element.Has_Task;
      null;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Interface_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Progenitor_List
        (Target.all,
         Primary_Expression_Lists.Deep_Copy 
           (Progenitor_List (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Access_Type_Kind
     (Element : Access_Type_Node) return Asis.Access_Type_Kinds is
   begin
      return Element.Access_Type_Kind;
   end Access_Type_Kind;

   procedure Set_Access_Type_Kind
     (Element : in out Access_Type_Node;
      Value   : in     Asis.Access_Type_Kinds) is
   begin
      Element.Access_Type_Kind := Value;
   end Set_Access_Type_Kind;

   function Get_Access_To_Object_Definition
     (Element : Access_Type_Node) return Asis.Subtype_Indication is
   begin
      return Element.Access_To_Object_Definition;
   end Get_Access_To_Object_Definition;

   procedure Set_Access_To_Object_Definition
     (Element : in out Access_Type_Node;
      Value   : in     Asis.Subtype_Indication) is
   begin
      Element.Access_To_Object_Definition := Value;
   end Set_Access_To_Object_Definition;

   function Access_To_Subprogram_Parameter_Profile
     (Element : Access_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Parameter_Lists.To_Element_List
        (Element.Access_To_Subprogram_Parameter_Profile, Include_Pragmas);
   end Access_To_Subprogram_Parameter_Profile;

   procedure Set_Access_To_Subprogram_Parameter_Profile
     (Element : in out Access_Type_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Access_To_Subprogram_Parameter_Profile := Primary_Parameter_Lists.List (Value);
   end Set_Access_To_Subprogram_Parameter_Profile;

   function Access_To_Subprogram_Parameter_Profile_List
     (Element : Access_Type_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Access_To_Subprogram_Parameter_Profile);
   end Access_To_Subprogram_Parameter_Profile_List;

   function Access_To_Function_Result_Subtype
     (Element : Access_Type_Node) return Asis.Definition is
   begin
      return Element.Access_To_Function_Result_Subtype;
   end Access_To_Function_Result_Subtype;

   procedure Set_Access_To_Function_Result_Subtype
     (Element : in out Access_Type_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Access_To_Function_Result_Subtype := Value;
   end Set_Access_To_Function_Result_Subtype;

   function Has_Null_Exclusion
     (Element : Access_Type_Node) return Boolean is
   begin
      return Element.Has_Null_Exclusion;
   end Has_Null_Exclusion;

   procedure Set_Has_Null_Exclusion
     (Element : in out Access_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Null_Exclusion := Value;
   end Set_Has_Null_Exclusion;

   function New_Access_Type_Node
     (The_Context : ASIS.Context)
      return Access_Type_Ptr
   is
      Result : Access_Type_Ptr :=
       new Access_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Access_Type_Node;
  
   function Type_Definition_Kind (Element : Access_Type_Node)
      return Asis.Type_Kinds is
   begin
      return An_Access_Type_Definition;
   end;

   function Children (Element : access Access_Type_Node)
     return Traverse_List is
   begin
      return ((False, Element.Access_To_Object_Definition'Access),
        (True, Asis.Element (Element.Access_To_Subprogram_Parameter_Profile)),
        (False, Element.Access_To_Function_Result_Subtype'Access));
   end Children;

   function Clone
     (Element : Access_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Access_Type_Ptr := new Access_Type_Node;
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
      Result.Access_Type_Kind := Element.Access_Type_Kind;
      Result.Has_Null_Exclusion := Element.Has_Null_Exclusion;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Access_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Access_To_Object_Definition :=
        Copy (Cloner, Get_Access_To_Object_Definition (Source.all), Asis.Element (Target));
      Set_Access_To_Subprogram_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Access_To_Subprogram_Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
      Target.Access_To_Function_Result_Subtype :=
        Copy (Cloner, Access_To_Function_Result_Subtype (Source.all), Asis.Element (Target));
   end Copy;

end Asis.Gela.Elements.Defs.Types;
