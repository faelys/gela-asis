
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

package body Asis.Gela.Elements.Defs.Formal is

   function Trait_Kind
     (Element : Formal_Private_Type_Node) return Asis.Trait_Kinds is
   begin
      return Element.Trait_Kind;
   end Trait_Kind;

   procedure Set_Trait_Kind
     (Element : in out Formal_Private_Type_Node;
      Value   : in     Asis.Trait_Kinds) is
   begin
      Element.Trait_Kind := Value;
   end Set_Trait_Kind;

   function Has_Limited
     (Element : Formal_Private_Type_Node) return Boolean is
   begin
      return Element.Has_Limited;
   end Has_Limited;

   procedure Set_Has_Limited
     (Element : in out Formal_Private_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Limited := Value;
   end Set_Has_Limited;

   function Has_Private
     (Element : Formal_Private_Type_Node) return Boolean is
   begin
      return Element.Has_Private;
   end Has_Private;

   procedure Set_Has_Private
     (Element : in out Formal_Private_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Private := Value;
   end Set_Has_Private;

   function New_Formal_Private_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Private_Type_Ptr
   is
      Result : Formal_Private_Type_Ptr :=
       new Formal_Private_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Private_Type_Node;
  
   function Formal_Type_Definition_Kind (Element : Formal_Private_Type_Node)
      return Asis.Formal_Type_Kinds is
   begin
      return A_Formal_Private_Type_Definition;
   end;

   function Clone
     (Element : Formal_Private_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Private_Type_Ptr := new Formal_Private_Type_Node;
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
      Result.Has_Private := Element.Has_Private;
      return Asis.Element (Result);
   end Clone;

   function Has_Abstract
     (Element : Formal_Tagged_Private_Type_Node) return Boolean is
   begin
      return Element.Has_Abstract;
   end Has_Abstract;

   procedure Set_Has_Abstract
     (Element : in out Formal_Tagged_Private_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Abstract := Value;
   end Set_Has_Abstract;

   function Has_Tagged
     (Element : Formal_Tagged_Private_Type_Node) return Boolean is
   begin
      return Element.Has_Tagged;
   end Has_Tagged;

   procedure Set_Has_Tagged
     (Element : in out Formal_Tagged_Private_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Tagged := Value;
   end Set_Has_Tagged;

   function New_Formal_Tagged_Private_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Tagged_Private_Type_Ptr
   is
      Result : Formal_Tagged_Private_Type_Ptr :=
       new Formal_Tagged_Private_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Tagged_Private_Type_Node;
  
   function Formal_Type_Definition_Kind (Element : Formal_Tagged_Private_Type_Node)
      return Asis.Formal_Type_Kinds is
   begin
      return A_Formal_Tagged_Private_Type_Definition;
   end;

   function Clone
     (Element : Formal_Tagged_Private_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Tagged_Private_Type_Ptr := new Formal_Tagged_Private_Type_Node;
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
      Result.Has_Private := Element.Has_Private;
      Result.Has_Abstract := Element.Has_Abstract;
      Result.Has_Tagged := Element.Has_Tagged;
      return Asis.Element (Result);
   end Clone;

   function Implicit_Inherited_Declarations
     (Element : Formal_Derived_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Declaration_Lists.To_Element_List
        (Element.Implicit_Inherited_Declarations, Include_Pragmas);
   end Implicit_Inherited_Declarations;

   procedure Add_To_Implicit_Inherited_Declarations
     (Element : in out Formal_Derived_Type_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Declaration_Lists.Add (Element.Implicit_Inherited_Declarations, Item);
   end Add_To_Implicit_Inherited_Declarations;

   function Implicit_Inherited_Subprograms
     (Element : Formal_Derived_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Declaration_Lists.To_Element_List
        (Element.Implicit_Inherited_Subprograms, Include_Pragmas);
   end Implicit_Inherited_Subprograms;

   procedure Add_To_Implicit_Inherited_Subprograms
     (Element : in out Formal_Derived_Type_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Declaration_Lists.Add (Element.Implicit_Inherited_Subprograms, Item);
   end Add_To_Implicit_Inherited_Subprograms;

   function Get_Subtype_Mark
     (Element : Formal_Derived_Type_Node) return Asis.Expression is
   begin
      return Element.Subtype_Mark;
   end Get_Subtype_Mark;

   procedure Set_Subtype_Mark
     (Element : in out Formal_Derived_Type_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Subtype_Mark := Value;
   end Set_Subtype_Mark;

   function Trait_Kind
     (Element : Formal_Derived_Type_Node) return Asis.Trait_Kinds is
   begin
      return Element.Trait_Kind;
   end Trait_Kind;

   procedure Set_Trait_Kind
     (Element : in out Formal_Derived_Type_Node;
      Value   : in     Asis.Trait_Kinds) is
   begin
      Element.Trait_Kind := Value;
   end Set_Trait_Kind;

   function Progenitor_List
     (Element : Formal_Derived_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Expression_Lists.To_Element_List
        (Element.Progenitor_List, Include_Pragmas);
   end Progenitor_List;

   procedure Set_Progenitor_List
     (Element : in out Formal_Derived_Type_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Progenitor_List := Primary_Expression_Lists.List (Value);
   end Set_Progenitor_List;

   function Progenitor_List_List
     (Element : Formal_Derived_Type_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Progenitor_List);
   end Progenitor_List_List;

   function Has_Abstract
     (Element : Formal_Derived_Type_Node) return Boolean is
   begin
      return Element.Has_Abstract;
   end Has_Abstract;

   procedure Set_Has_Abstract
     (Element : in out Formal_Derived_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Abstract := Value;
   end Set_Has_Abstract;

   function Has_Private
     (Element : Formal_Derived_Type_Node) return Boolean is
   begin
      return Element.Has_Private;
   end Has_Private;

   procedure Set_Has_Private
     (Element : in out Formal_Derived_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Private := Value;
   end Set_Has_Private;

   function Has_Limited
     (Element : Formal_Derived_Type_Node) return Boolean is
   begin
      return Element.Has_Limited;
   end Has_Limited;

   procedure Set_Has_Limited
     (Element : in out Formal_Derived_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Limited := Value;
   end Set_Has_Limited;

   function Has_Synchronized
     (Element : Formal_Derived_Type_Node) return Boolean is
   begin
      return Element.Has_Synchronized;
   end Has_Synchronized;

   procedure Set_Has_Synchronized
     (Element : in out Formal_Derived_Type_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Synchronized := Value;
   end Set_Has_Synchronized;

   function New_Formal_Derived_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Derived_Type_Ptr
   is
      Result : Formal_Derived_Type_Ptr :=
       new Formal_Derived_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Derived_Type_Node;
  
   function Formal_Type_Definition_Kind (Element : Formal_Derived_Type_Node)
      return Asis.Formal_Type_Kinds is
   begin
      return A_Formal_Derived_Type_Definition;
   end;

   function Children (Element : access Formal_Derived_Type_Node)
     return Traverse_List is
   begin
      return ((False, Element.Subtype_Mark'Access),
        (True, Asis.Element (Element.Progenitor_List)));
   end Children;

   function Clone
     (Element : Formal_Derived_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Derived_Type_Ptr := new Formal_Derived_Type_Node;
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
      null;
      null;
      Result.Trait_Kind := Element.Trait_Kind;
      Result.Has_Abstract := Element.Has_Abstract;
      Result.Has_Private := Element.Has_Private;
      Result.Has_Limited := Element.Has_Limited;
      Result.Has_Synchronized := Element.Has_Synchronized;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Derived_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Subtype_Mark :=
        Copy (Cloner, Get_Subtype_Mark (Source.all), Asis.Element (Target));
      Set_Progenitor_List
        (Target.all,
         Primary_Expression_Lists.Deep_Copy 
           (Progenitor_List (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Formal_Discrete_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Discrete_Type_Ptr
   is
      Result : Formal_Discrete_Type_Ptr :=
       new Formal_Discrete_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Discrete_Type_Node;
  
   function Formal_Type_Definition_Kind (Element : Formal_Discrete_Type_Node)
      return Asis.Formal_Type_Kinds is
   begin
      return A_Formal_Discrete_Type_Definition;
   end;

   function Clone
     (Element : Formal_Discrete_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Discrete_Type_Ptr := new Formal_Discrete_Type_Node;
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

   function New_Formal_Signed_Integer_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Signed_Integer_Type_Ptr
   is
      Result : Formal_Signed_Integer_Type_Ptr :=
       new Formal_Signed_Integer_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Signed_Integer_Type_Node;
  
   function Formal_Type_Definition_Kind (Element : Formal_Signed_Integer_Type_Node)
      return Asis.Formal_Type_Kinds is
   begin
      return A_Formal_Signed_Integer_Type_Definition;
   end;

   function Clone
     (Element : Formal_Signed_Integer_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Signed_Integer_Type_Ptr := new Formal_Signed_Integer_Type_Node;
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

   function New_Formal_Modular_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Modular_Type_Ptr
   is
      Result : Formal_Modular_Type_Ptr :=
       new Formal_Modular_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Modular_Type_Node;
  
   function Formal_Type_Definition_Kind (Element : Formal_Modular_Type_Node)
      return Asis.Formal_Type_Kinds is
   begin
      return A_Formal_Modular_Type_Definition;
   end;

   function Clone
     (Element : Formal_Modular_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Modular_Type_Ptr := new Formal_Modular_Type_Node;
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

   function New_Formal_Floating_Point_Node
     (The_Context : ASIS.Context)
      return Formal_Floating_Point_Ptr
   is
      Result : Formal_Floating_Point_Ptr :=
       new Formal_Floating_Point_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Floating_Point_Node;
  
   function Formal_Type_Definition_Kind (Element : Formal_Floating_Point_Node)
      return Asis.Formal_Type_Kinds is
   begin
      return A_Formal_Floating_Point_Definition;
   end;

   function Clone
     (Element : Formal_Floating_Point_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Floating_Point_Ptr := new Formal_Floating_Point_Node;
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

   function New_Formal_Ordinary_Fixed_Point_Node
     (The_Context : ASIS.Context)
      return Formal_Ordinary_Fixed_Point_Ptr
   is
      Result : Formal_Ordinary_Fixed_Point_Ptr :=
       new Formal_Ordinary_Fixed_Point_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Ordinary_Fixed_Point_Node;
  
   function Formal_Type_Definition_Kind (Element : Formal_Ordinary_Fixed_Point_Node)
      return Asis.Formal_Type_Kinds is
   begin
      return A_Formal_Ordinary_Fixed_Point_Definition;
   end;

   function Clone
     (Element : Formal_Ordinary_Fixed_Point_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Ordinary_Fixed_Point_Ptr := new Formal_Ordinary_Fixed_Point_Node;
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

   function New_Formal_Decimal_Fixed_Point_Node
     (The_Context : ASIS.Context)
      return Formal_Decimal_Fixed_Point_Ptr
   is
      Result : Formal_Decimal_Fixed_Point_Ptr :=
       new Formal_Decimal_Fixed_Point_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Decimal_Fixed_Point_Node;
  
   function Formal_Type_Definition_Kind (Element : Formal_Decimal_Fixed_Point_Node)
      return Asis.Formal_Type_Kinds is
   begin
      return A_Formal_Decimal_Fixed_Point_Definition;
   end;

   function Clone
     (Element : Formal_Decimal_Fixed_Point_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Decimal_Fixed_Point_Ptr := new Formal_Decimal_Fixed_Point_Node;
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

   function Index_Subtype_Definitions
     (Element : Formal_Unconstrained_Array_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Identifier_Lists.To_Element_List
        (Element.Index_Subtype_Definitions, Include_Pragmas);
   end Index_Subtype_Definitions;

   procedure Set_Index_Subtype_Definitions
     (Element : in out Formal_Unconstrained_Array_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Index_Subtype_Definitions := Primary_Identifier_Lists.List (Value);
   end Set_Index_Subtype_Definitions;

   function Index_Subtype_Definitions_List
     (Element : Formal_Unconstrained_Array_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Index_Subtype_Definitions);
   end Index_Subtype_Definitions_List;

   function Array_Component_Definition
     (Element : Formal_Unconstrained_Array_Node) return Asis.Component_Definition is
   begin
      return Element.Array_Component_Definition;
   end Array_Component_Definition;

   procedure Set_Array_Component_Definition
     (Element : in out Formal_Unconstrained_Array_Node;
      Value   : in     Asis.Component_Definition) is
   begin
      Element.Array_Component_Definition := Value;
   end Set_Array_Component_Definition;

   function Array_Definition
     (Element : Formal_Unconstrained_Array_Node) return Asis.Element is
   begin
      return Element.Array_Definition;
   end Array_Definition;

   procedure Set_Array_Definition
     (Element : in out Formal_Unconstrained_Array_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Array_Definition := Value;
   end Set_Array_Definition;

   function New_Formal_Unconstrained_Array_Node
     (The_Context : ASIS.Context)
      return Formal_Unconstrained_Array_Ptr
   is
      Result : Formal_Unconstrained_Array_Ptr :=
       new Formal_Unconstrained_Array_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Unconstrained_Array_Node;
  
   function Formal_Type_Definition_Kind (Element : Formal_Unconstrained_Array_Node)
      return Asis.Formal_Type_Kinds is
   begin
      return A_Formal_Unconstrained_Array_Definition;
   end;

   function Children (Element : access Formal_Unconstrained_Array_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Index_Subtype_Definitions)),
        (False, Element.Array_Component_Definition'Access));
   end Children;

   function Clone
     (Element : Formal_Unconstrained_Array_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Unconstrained_Array_Ptr := new Formal_Unconstrained_Array_Node;
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
      Result.Array_Definition := Element.Array_Definition;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Unconstrained_Array_Node;
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
     (Element : Formal_Constrained_Array_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Definition_Lists.To_Element_List
        (Element.Discrete_Subtype_Definitions, Include_Pragmas);
   end Discrete_Subtype_Definitions;

   procedure Set_Discrete_Subtype_Definitions
     (Element : in out Formal_Constrained_Array_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Discrete_Subtype_Definitions := Primary_Definition_Lists.List (Value);
   end Set_Discrete_Subtype_Definitions;

   function Discrete_Subtype_Definitions_List
     (Element : Formal_Constrained_Array_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Discrete_Subtype_Definitions);
   end Discrete_Subtype_Definitions_List;

   function Array_Component_Definition
     (Element : Formal_Constrained_Array_Node) return Asis.Component_Definition is
   begin
      return Element.Array_Component_Definition;
   end Array_Component_Definition;

   procedure Set_Array_Component_Definition
     (Element : in out Formal_Constrained_Array_Node;
      Value   : in     Asis.Component_Definition) is
   begin
      Element.Array_Component_Definition := Value;
   end Set_Array_Component_Definition;

   function Array_Definition
     (Element : Formal_Constrained_Array_Node) return Asis.Element is
   begin
      return Element.Array_Definition;
   end Array_Definition;

   procedure Set_Array_Definition
     (Element : in out Formal_Constrained_Array_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Array_Definition := Value;
   end Set_Array_Definition;

   function New_Formal_Constrained_Array_Node
     (The_Context : ASIS.Context)
      return Formal_Constrained_Array_Ptr
   is
      Result : Formal_Constrained_Array_Ptr :=
       new Formal_Constrained_Array_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Constrained_Array_Node;
  
   function Formal_Type_Definition_Kind (Element : Formal_Constrained_Array_Node)
      return Asis.Formal_Type_Kinds is
   begin
      return A_Formal_Constrained_Array_Definition;
   end;

   function Children (Element : access Formal_Constrained_Array_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Discrete_Subtype_Definitions)),
        (False, Element.Array_Component_Definition'Access));
   end Children;

   function Clone
     (Element : Formal_Constrained_Array_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Constrained_Array_Ptr := new Formal_Constrained_Array_Node;
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
      Result.Array_Definition := Element.Array_Definition;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Constrained_Array_Node;
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

   function Access_Type_Kind
     (Element : Formal_Access_Type_Node) return Asis.Access_Type_Kinds is
   begin
      return Element.Access_Type_Kind;
   end Access_Type_Kind;

   procedure Set_Access_Type_Kind
     (Element : in out Formal_Access_Type_Node;
      Value   : in     Asis.Access_Type_Kinds) is
   begin
      Element.Access_Type_Kind := Value;
   end Set_Access_Type_Kind;

   function Get_Access_To_Object_Definition
     (Element : Formal_Access_Type_Node) return Asis.Subtype_Indication is
   begin
      return Element.Access_To_Object_Definition;
   end Get_Access_To_Object_Definition;

   procedure Set_Access_To_Object_Definition
     (Element : in out Formal_Access_Type_Node;
      Value   : in     Asis.Subtype_Indication) is
   begin
      Element.Access_To_Object_Definition := Value;
   end Set_Access_To_Object_Definition;

   function Access_To_Subprogram_Parameter_Profile
     (Element : Formal_Access_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Parameter_Lists.To_Element_List
        (Element.Access_To_Subprogram_Parameter_Profile, Include_Pragmas);
   end Access_To_Subprogram_Parameter_Profile;

   procedure Set_Access_To_Subprogram_Parameter_Profile
     (Element : in out Formal_Access_Type_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Access_To_Subprogram_Parameter_Profile := Primary_Parameter_Lists.List (Value);
   end Set_Access_To_Subprogram_Parameter_Profile;

   function Access_To_Subprogram_Parameter_Profile_List
     (Element : Formal_Access_Type_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Access_To_Subprogram_Parameter_Profile);
   end Access_To_Subprogram_Parameter_Profile_List;

   function Access_To_Function_Result_Subtype
     (Element : Formal_Access_Type_Node) return Asis.Definition is
   begin
      return Element.Access_To_Function_Result_Subtype;
   end Access_To_Function_Result_Subtype;

   procedure Set_Access_To_Function_Result_Subtype
     (Element : in out Formal_Access_Type_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Access_To_Function_Result_Subtype := Value;
   end Set_Access_To_Function_Result_Subtype;

   function Access_Definition
     (Element : Formal_Access_Type_Node) return Asis.Element is
   begin
      return Element.Access_Definition;
   end Access_Definition;

   procedure Set_Access_Definition
     (Element : in out Formal_Access_Type_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Access_Definition := Value;
   end Set_Access_Definition;

   function New_Formal_Access_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Access_Type_Ptr
   is
      Result : Formal_Access_Type_Ptr :=
       new Formal_Access_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Access_Type_Node;
  
   function Formal_Type_Definition_Kind (Element : Formal_Access_Type_Node)
      return Asis.Formal_Type_Kinds is
   begin
      return A_Formal_Access_Type_Definition;
   end;

   function Children (Element : access Formal_Access_Type_Node)
     return Traverse_List is
   begin
      return ((False, Element.Access_To_Object_Definition'Access),
        (True, Asis.Element (Element.Access_To_Subprogram_Parameter_Profile)),
        (False, Element.Access_To_Function_Result_Subtype'Access));
   end Children;

   function Clone
     (Element : Formal_Access_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Access_Type_Ptr := new Formal_Access_Type_Node;
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
      Result.Access_Definition := Element.Access_Definition;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Access_Type_Node;
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

   function Interface_Kind
     (Element : Formal_Interface_Type_Node) return Asis.Interface_Kinds is
   begin
      return Element.Interface_Kind;
   end Interface_Kind;

   procedure Set_Interface_Kind
     (Element : in out Formal_Interface_Type_Node;
      Value   : in     Asis.Interface_Kinds) is
   begin
      Element.Interface_Kind := Value;
   end Set_Interface_Kind;

   function Progenitor_List
     (Element : Formal_Interface_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Expression_Lists.To_Element_List
        (Element.Progenitor_List, Include_Pragmas);
   end Progenitor_List;

   procedure Set_Progenitor_List
     (Element : in out Formal_Interface_Type_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Progenitor_List := Primary_Expression_Lists.List (Value);
   end Set_Progenitor_List;

   function Progenitor_List_List
     (Element : Formal_Interface_Type_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Progenitor_List);
   end Progenitor_List_List;

   function Implicit_Inherited_Subprograms
     (Element : Formal_Interface_Type_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Declaration_Lists.To_Element_List
        (Element.Implicit_Inherited_Subprograms, Include_Pragmas);
   end Implicit_Inherited_Subprograms;

   procedure Add_To_Implicit_Inherited_Subprograms
     (Element : in out Formal_Interface_Type_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Declaration_Lists.Add (Element.Implicit_Inherited_Subprograms, Item);
   end Add_To_Implicit_Inherited_Subprograms;

   function New_Formal_Interface_Type_Node
     (The_Context : ASIS.Context)
      return Formal_Interface_Type_Ptr
   is
      Result : Formal_Interface_Type_Ptr :=
       new Formal_Interface_Type_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Formal_Interface_Type_Node;
  
   function Formal_Type_Definition_Kind (Element : Formal_Interface_Type_Node)
      return Asis.Formal_Type_Kinds is
   begin
      return A_Formal_Interface_Type_Definition;
   end;

   function Children (Element : access Formal_Interface_Type_Node)
     return Traverse_List is
   begin
      return (1 => (True, Asis.Element (Element.Progenitor_List)));
   end Children;

   function Clone
     (Element : Formal_Interface_Type_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Formal_Interface_Type_Ptr := new Formal_Interface_Type_Node;
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
      null;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Formal_Interface_Type_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Progenitor_List
        (Target.all,
         Primary_Expression_Lists.Deep_Copy 
           (Progenitor_List (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

end Asis.Gela.Elements.Defs.Formal;
