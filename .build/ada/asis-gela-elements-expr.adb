
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

package body Asis.Gela.Elements.Expr is

   function New_Box_Expression_Node
     (The_Context : ASIS.Context)
      return Box_Expression_Ptr
   is
      Result : Box_Expression_Ptr :=
       new Box_Expression_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Box_Expression_Node;
  
   function Expression_Kind (Element : Box_Expression_Node)
      return Asis.Expression_Kinds is
   begin
      return A_Box_Expression;
   end;

   function Clone
     (Element : Box_Expression_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Box_Expression_Ptr := new Box_Expression_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   function Value_Image
     (Element : Base_Literal_Node) return Wide_String is
   begin
      return W.To_Wide_String (Element.Value_Image);
   end Value_Image;

   procedure Set_Value_Image
     (Element : in out Base_Literal_Node;
      Value   : in     Wide_String) is
   begin
      Element.Value_Image := W.To_Unbounded_Wide_String (Value);
   end Set_Value_Image;

   function New_Integer_Literal_Node
     (The_Context : ASIS.Context)
      return Integer_Literal_Ptr
   is
      Result : Integer_Literal_Ptr :=
       new Integer_Literal_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Integer_Literal_Node;
  
   function Expression_Kind (Element : Integer_Literal_Node)
      return Asis.Expression_Kinds is
   begin
      return An_Integer_Literal;
   end;

   function Clone
     (Element : Integer_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Integer_Literal_Ptr := new Integer_Literal_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      Result.Value_Image := Element.Value_Image;
      return Asis.Element (Result);
   end Clone;

   function New_Real_Literal_Node
     (The_Context : ASIS.Context)
      return Real_Literal_Ptr
   is
      Result : Real_Literal_Ptr :=
       new Real_Literal_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Real_Literal_Node;
  
   function Expression_Kind (Element : Real_Literal_Node)
      return Asis.Expression_Kinds is
   begin
      return A_Real_Literal;
   end;

   function Clone
     (Element : Real_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Real_Literal_Ptr := new Real_Literal_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      Result.Value_Image := Element.Value_Image;
      return Asis.Element (Result);
   end Clone;

   function New_String_Literal_Node
     (The_Context : ASIS.Context)
      return String_Literal_Ptr
   is
      Result : String_Literal_Ptr :=
       new String_Literal_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_String_Literal_Node;
  
   function Expression_Kind (Element : String_Literal_Node)
      return Asis.Expression_Kinds is
   begin
      return A_String_Literal;
   end;

   function Clone
     (Element : String_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant String_Literal_Ptr := new String_Literal_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      Result.Value_Image := Element.Value_Image;
      return Asis.Element (Result);
   end Clone;

   function Name_Image
     (Element : Base_Identifier_Node) return Wide_String is
   begin
      return W.To_Wide_String (Element.Name_Image);
   end Name_Image;

   procedure Set_Name_Image
     (Element : in out Base_Identifier_Node;
      Value   : in     Wide_String) is
   begin
      Element.Name_Image := W.To_Unbounded_Wide_String (Value);
   end Set_Name_Image;

   function Corresponding_Name_Declaration
     (Element : Base_Identifier_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Name_Declaration;
   end Corresponding_Name_Declaration;

   procedure Set_Corresponding_Name_Declaration
     (Element : in out Base_Identifier_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Name_Declaration := Value;
   end Set_Corresponding_Name_Declaration;

   function Corresponding_Name_Definition_List
     (Element : Base_Identifier_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Definition_Lists.To_Element_List
        (Element.Corresponding_Name_Definition_List, Include_Pragmas);
   end Corresponding_Name_Definition_List;

   procedure Add_To_Corresponding_Name_Definition_List
     (Element : in out Base_Identifier_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Definition_Lists.Add (Element.Corresponding_Name_Definition_List, Item);
   end Add_To_Corresponding_Name_Definition_List;

   function Corresponding_Generic_Element
     (Element : Base_Identifier_Node) return Asis.Defining_Name is
   begin
      return Element.Corresponding_Generic_Element;
   end Corresponding_Generic_Element;

   procedure Set_Corresponding_Generic_Element
     (Element : in out Base_Identifier_Node;
      Value   : in     Asis.Defining_Name) is
   begin
      Element.Corresponding_Generic_Element := Value;
   end Set_Corresponding_Generic_Element;

   function New_Identifier_Node
     (The_Context : ASIS.Context)
      return Identifier_Ptr
   is
      Result : Identifier_Ptr :=
       new Identifier_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Identifier_Node;
  
   function Expression_Kind (Element : Identifier_Node)
      return Asis.Expression_Kinds is
   begin
      return An_Identifier;
   end;

   function Clone
     (Element : Identifier_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Identifier_Ptr := new Identifier_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      Result.Name_Image := Element.Name_Image;
      Result.Corresponding_Name_Declaration := Element.Corresponding_Name_Declaration;
      null;
      Result.Corresponding_Generic_Element := Element.Corresponding_Generic_Element;
      return Asis.Element (Result);
   end Clone;

   function Operator_Kind
     (Element : Operator_Symbol_Node) return Asis.Operator_Kinds is
   begin
      return Element.Operator_Kind;
   end Operator_Kind;

   procedure Set_Operator_Kind
     (Element : in out Operator_Symbol_Node;
      Value   : in     Asis.Operator_Kinds) is
   begin
      Element.Operator_Kind := Value;
   end Set_Operator_Kind;

   function New_Operator_Symbol_Node
     (The_Context : ASIS.Context)
      return Operator_Symbol_Ptr
   is
      Result : Operator_Symbol_Ptr :=
       new Operator_Symbol_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Operator_Symbol_Node;
  
   function Expression_Kind (Element : Operator_Symbol_Node)
      return Asis.Expression_Kinds is
   begin
      return An_Operator_Symbol;
   end;

   function Clone
     (Element : Operator_Symbol_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Operator_Symbol_Ptr := new Operator_Symbol_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      Result.Name_Image := Element.Name_Image;
      Result.Corresponding_Name_Declaration := Element.Corresponding_Name_Declaration;
      null;
      Result.Corresponding_Generic_Element := Element.Corresponding_Generic_Element;
      Result.Operator_Kind := Element.Operator_Kind;
      return Asis.Element (Result);
   end Clone;

   function New_Character_Literal_Node
     (The_Context : ASIS.Context)
      return Character_Literal_Ptr
   is
      Result : Character_Literal_Ptr :=
       new Character_Literal_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Character_Literal_Node;
  
   function Expression_Kind (Element : Character_Literal_Node)
      return Asis.Expression_Kinds is
   begin
      return A_Character_Literal;
   end;

   function Clone
     (Element : Character_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Character_Literal_Ptr := new Character_Literal_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      Result.Name_Image := Element.Name_Image;
      Result.Corresponding_Name_Declaration := Element.Corresponding_Name_Declaration;
      null;
      Result.Corresponding_Generic_Element := Element.Corresponding_Generic_Element;
      return Asis.Element (Result);
   end Clone;

   function New_Enumeration_Literal_Node
     (The_Context : ASIS.Context)
      return Enumeration_Literal_Ptr
   is
      Result : Enumeration_Literal_Ptr :=
       new Enumeration_Literal_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Enumeration_Literal_Node;
  
   function Expression_Kind (Element : Enumeration_Literal_Node)
      return Asis.Expression_Kinds is
   begin
      return An_Enumeration_Literal;
   end;

   function Clone
     (Element : Enumeration_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Enumeration_Literal_Ptr := new Enumeration_Literal_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      Result.Name_Image := Element.Name_Image;
      Result.Corresponding_Name_Declaration := Element.Corresponding_Name_Declaration;
      null;
      Result.Corresponding_Generic_Element := Element.Corresponding_Generic_Element;
      return Asis.Element (Result);
   end Clone;

   function Prefix
     (Element : Explicit_Dereference_Node) return Asis.Expression is
   begin
      return Element.Prefix;
   end Prefix;

   procedure Set_Prefix
     (Element : in out Explicit_Dereference_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Prefix := Value;
   end Set_Prefix;

   function New_Explicit_Dereference_Node
     (The_Context : ASIS.Context)
      return Explicit_Dereference_Ptr
   is
      Result : Explicit_Dereference_Ptr :=
       new Explicit_Dereference_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Explicit_Dereference_Node;
  
   function Expression_Kind (Element : Explicit_Dereference_Node)
      return Asis.Expression_Kinds is
   begin
      return An_Explicit_Dereference;
   end;

   function Children (Element : access Explicit_Dereference_Node)
     return Traverse_List is
   begin
      return (1 => (False, Element.Prefix'Access));
   end Children;

   function Clone
     (Element : Explicit_Dereference_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Explicit_Dereference_Ptr := new Explicit_Dereference_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Explicit_Dereference_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Prefix :=
        Copy (Cloner, Prefix (Source.all), Asis.Element (Target));
   end Copy;

   function Prefix
     (Element : Function_Call_Node) return Asis.Expression is
   begin
      return Element.Prefix;
   end Prefix;

   procedure Set_Prefix
     (Element : in out Function_Call_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Prefix := Value;
   end Set_Prefix;

   function Is_Prefix_Call
     (Element : Function_Call_Node) return Boolean is
   begin
      return Element.Is_Prefix_Call;
   end Is_Prefix_Call;

   procedure Set_Is_Prefix_Call
     (Element : in out Function_Call_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Prefix_Call := Value;
   end Set_Is_Prefix_Call;

   function Is_Dispatching_Call
     (Element : Function_Call_Node) return Boolean is
   begin
      return Element.Is_Dispatching_Call;
   end Is_Dispatching_Call;

   procedure Set_Is_Dispatching_Call
     (Element : in out Function_Call_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Dispatching_Call := Value;
   end Set_Is_Dispatching_Call;

   function Corresponding_Called_Function
     (Element : Function_Call_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Called_Function;
   end Corresponding_Called_Function;

   procedure Set_Corresponding_Called_Function
     (Element : in out Function_Call_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Called_Function := Value;
   end Set_Corresponding_Called_Function;

   function Function_Call_Parameters
     (Element : Function_Call_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Association_Lists.To_Element_List
        (Element.Function_Call_Parameters, Include_Pragmas);
   end Function_Call_Parameters;

   procedure Set_Function_Call_Parameters
     (Element : in out Function_Call_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Function_Call_Parameters := Primary_Association_Lists.List (Value);
   end Set_Function_Call_Parameters;

   function Function_Call_Parameters_List
     (Element : Function_Call_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Function_Call_Parameters);
   end Function_Call_Parameters_List;

   function Normalized_Function_Call_Parameters
     (Element : Function_Call_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Association_Lists.To_Element_List
        (Element.Normalized_Function_Call_Parameters, Include_Pragmas);
   end Normalized_Function_Call_Parameters;

   procedure Add_To_Normalized_Function_Call_Parameters
     (Element : in out Function_Call_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Association_Lists.Add (Element.Normalized_Function_Call_Parameters, Item);
   end Add_To_Normalized_Function_Call_Parameters;

   function Is_Call_On_Dispatching_Operation
     (Element : Function_Call_Node) return Boolean is
   begin
      return Element.Is_Call_On_Dispatching_Operation;
   end Is_Call_On_Dispatching_Operation;

   procedure Set_Is_Call_On_Dispatching_Operation
     (Element : in out Function_Call_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Call_On_Dispatching_Operation := Value;
   end Set_Is_Call_On_Dispatching_Operation;

   function Record_Aggregate
     (Element : Function_Call_Node) return Asis.Element is
   begin
      return Element.Record_Aggregate;
   end Record_Aggregate;

   procedure Set_Record_Aggregate
     (Element : in out Function_Call_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Record_Aggregate := Value;
   end Set_Record_Aggregate;

   function New_Function_Call_Node
     (The_Context : ASIS.Context)
      return Function_Call_Ptr
   is
      Result : Function_Call_Ptr :=
       new Function_Call_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Function_Call_Node;
  
   function Expression_Kind (Element : Function_Call_Node)
      return Asis.Expression_Kinds is
   begin
      return A_Function_Call;
   end;

   function Children (Element : access Function_Call_Node)
     return Traverse_List is
   begin
      return ((False, Element.Prefix'Access),
        (True, Asis.Element (Element.Function_Call_Parameters)));
   end Children;

   function Clone
     (Element : Function_Call_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Function_Call_Ptr := new Function_Call_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      Result.Is_Prefix_Call := Element.Is_Prefix_Call;
      Result.Is_Dispatching_Call := Element.Is_Dispatching_Call;
      Result.Corresponding_Called_Function := Element.Corresponding_Called_Function;
      null;
      Result.Is_Call_On_Dispatching_Operation := Element.Is_Call_On_Dispatching_Operation;
      Result.Record_Aggregate := Element.Record_Aggregate;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Function_Call_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Prefix :=
        Copy (Cloner, Prefix (Source.all), Asis.Element (Target));
      Set_Function_Call_Parameters
        (Target.all,
         Primary_Association_Lists.Deep_Copy 
           (Function_Call_Parameters (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Prefix
     (Element : Indexed_Component_Node) return Asis.Expression is
   begin
      return Element.Prefix;
   end Prefix;

   procedure Set_Prefix
     (Element : in out Indexed_Component_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Prefix := Value;
   end Set_Prefix;

   function Index_Expressions
     (Element : Indexed_Component_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Expression_Lists.To_Element_List
        (Element.Index_Expressions, Include_Pragmas);
   end Index_Expressions;

   procedure Set_Index_Expressions
     (Element : in out Indexed_Component_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Index_Expressions := Primary_Expression_Lists.List (Value);
   end Set_Index_Expressions;

   function Index_Expressions_List
     (Element : Indexed_Component_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Index_Expressions);
   end Index_Expressions_List;

   function New_Indexed_Component_Node
     (The_Context : ASIS.Context)
      return Indexed_Component_Ptr
   is
      Result : Indexed_Component_Ptr :=
       new Indexed_Component_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Indexed_Component_Node;
  
   function Expression_Kind (Element : Indexed_Component_Node)
      return Asis.Expression_Kinds is
   begin
      return An_Indexed_Component;
   end;

   function Children (Element : access Indexed_Component_Node)
     return Traverse_List is
   begin
      return ((False, Element.Prefix'Access),
        (True, Asis.Element (Element.Index_Expressions)));
   end Children;

   function Clone
     (Element : Indexed_Component_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Indexed_Component_Ptr := new Indexed_Component_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Indexed_Component_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Prefix :=
        Copy (Cloner, Prefix (Source.all), Asis.Element (Target));
      Set_Index_Expressions
        (Target.all,
         Primary_Expression_Lists.Deep_Copy 
           (Index_Expressions (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Prefix
     (Element : Slice_Node) return Asis.Expression is
   begin
      return Element.Prefix;
   end Prefix;

   procedure Set_Prefix
     (Element : in out Slice_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Prefix := Value;
   end Set_Prefix;

   function Slice_Range
     (Element : Slice_Node) return Asis.Discrete_Range is
   begin
      return Element.Slice_Range;
   end Slice_Range;

   procedure Set_Slice_Range
     (Element : in out Slice_Node;
      Value   : in     Asis.Discrete_Range) is
   begin
      Element.Slice_Range := Value;
   end Set_Slice_Range;

   function New_Slice_Node
     (The_Context : ASIS.Context)
      return Slice_Ptr
   is
      Result : Slice_Ptr :=
       new Slice_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Slice_Node;
  
   function Expression_Kind (Element : Slice_Node)
      return Asis.Expression_Kinds is
   begin
      return A_Slice;
   end;

   function Children (Element : access Slice_Node)
     return Traverse_List is
   begin
      return ((False, Element.Prefix'Access),
        (False, Element.Slice_Range'Access));
   end Children;

   function Clone
     (Element : Slice_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Slice_Ptr := new Slice_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Slice_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Prefix :=
        Copy (Cloner, Prefix (Source.all), Asis.Element (Target));
      Target.Slice_Range :=
        Copy (Cloner, Slice_Range (Source.all), Asis.Element (Target));
   end Copy;

   function Prefix
     (Element : Selected_Component_Node) return Asis.Expression is
   begin
      return Element.Prefix;
   end Prefix;

   procedure Set_Prefix
     (Element : in out Selected_Component_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Prefix := Value;
   end Set_Prefix;

   function Selector
     (Element : Selected_Component_Node) return Asis.Expression is
   begin
      return Element.Selector;
   end Selector;

   procedure Set_Selector
     (Element : in out Selected_Component_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Selector := Value;
   end Set_Selector;

   function New_Selected_Component_Node
     (The_Context : ASIS.Context)
      return Selected_Component_Ptr
   is
      Result : Selected_Component_Ptr :=
       new Selected_Component_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Selected_Component_Node;
  
   function Expression_Kind (Element : Selected_Component_Node)
      return Asis.Expression_Kinds is
   begin
      return A_Selected_Component;
   end;

   function Children (Element : access Selected_Component_Node)
     return Traverse_List is
   begin
      return ((False, Element.Prefix'Access),
        (False, Element.Selector'Access));
   end Children;

   function Clone
     (Element : Selected_Component_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Selected_Component_Ptr := new Selected_Component_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Selected_Component_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Prefix :=
        Copy (Cloner, Prefix (Source.all), Asis.Element (Target));
      Target.Selector :=
        Copy (Cloner, Selector (Source.all), Asis.Element (Target));
   end Copy;

   function Prefix
     (Element : Attribute_Reference_Node) return Asis.Expression is
   begin
      return Element.Prefix;
   end Prefix;

   procedure Set_Prefix
     (Element : in out Attribute_Reference_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Prefix := Value;
   end Set_Prefix;

   function Attribute_Kind
     (Element : Attribute_Reference_Node) return Asis.Attribute_Kinds is
   begin
      return Element.Attribute_Kind;
   end Attribute_Kind;

   procedure Set_Attribute_Kind
     (Element : in out Attribute_Reference_Node;
      Value   : in     Asis.Attribute_Kinds) is
   begin
      Element.Attribute_Kind := Value;
   end Set_Attribute_Kind;

   function Attribute_Designator_Identifier
     (Element : Attribute_Reference_Node) return Asis.Expression is
   begin
      return Element.Attribute_Designator_Identifier;
   end Attribute_Designator_Identifier;

   procedure Set_Attribute_Designator_Identifier
     (Element : in out Attribute_Reference_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Attribute_Designator_Identifier := Value;
   end Set_Attribute_Designator_Identifier;

   function Attribute_Designator_Expressions
     (Element : Attribute_Reference_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Expression_Lists.To_Element_List
        (Element.Attribute_Designator_Expressions, Include_Pragmas);
   end Attribute_Designator_Expressions;

   procedure Set_Attribute_Designator_Expressions
     (Element : in out Attribute_Reference_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Attribute_Designator_Expressions := Primary_Expression_Lists.List (Value);
   end Set_Attribute_Designator_Expressions;

   function Attribute_Designator_Expressions_List
     (Element : Attribute_Reference_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Attribute_Designator_Expressions);
   end Attribute_Designator_Expressions_List;

   function New_Attribute_Reference_Node
     (The_Context : ASIS.Context)
      return Attribute_Reference_Ptr
   is
      Result : Attribute_Reference_Ptr :=
       new Attribute_Reference_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Attribute_Reference_Node;
  
   function Expression_Kind (Element : Attribute_Reference_Node)
      return Asis.Expression_Kinds is
   begin
      return An_Attribute_Reference;
   end;

   function Children (Element : access Attribute_Reference_Node)
     return Traverse_List is
   begin
      return ((False, Element.Prefix'Access),
        (False, Element.Attribute_Designator_Identifier'Access),
        (True, Asis.Element (Element.Attribute_Designator_Expressions)));
   end Children;

   function Clone
     (Element : Attribute_Reference_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Attribute_Reference_Ptr := new Attribute_Reference_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      Result.Attribute_Kind := Element.Attribute_Kind;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Attribute_Reference_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Prefix :=
        Copy (Cloner, Prefix (Source.all), Asis.Element (Target));
      Target.Attribute_Designator_Identifier :=
        Copy (Cloner, Attribute_Designator_Identifier (Source.all), Asis.Element (Target));
      Set_Attribute_Designator_Expressions
        (Target.all,
         Primary_Expression_Lists.Deep_Copy 
           (Attribute_Designator_Expressions (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Record_Component_Associations
     (Element : Base_Record_Aggregate_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Association_Lists.To_Element_List
        (Element.Record_Component_Associations, Include_Pragmas);
   end Record_Component_Associations;

   procedure Set_Record_Component_Associations
     (Element : in out Base_Record_Aggregate_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Record_Component_Associations := Primary_Association_Lists.List (Value);
   end Set_Record_Component_Associations;

   function Record_Component_Associations_List
     (Element : Base_Record_Aggregate_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Record_Component_Associations);
   end Record_Component_Associations_List;

   function Normalized_Record_Component_Associations
     (Element : Base_Record_Aggregate_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Association_Lists.To_Element_List
        (Element.Normalized_Record_Component_Associations, Include_Pragmas);
   end Normalized_Record_Component_Associations;

   procedure Add_To_Normalized_Record_Component_Associations
     (Element : in out Base_Record_Aggregate_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Association_Lists.Add (Element.Normalized_Record_Component_Associations, Item);
   end Add_To_Normalized_Record_Component_Associations;

   function Children (Element : access Base_Record_Aggregate_Node)
     return Traverse_List is
   begin
      return (1 => (True, Asis.Element (Element.Record_Component_Associations)));
   end Children;

   function New_Record_Aggregate_Node
     (The_Context : ASIS.Context)
      return Record_Aggregate_Ptr
   is
      Result : Record_Aggregate_Ptr :=
       new Record_Aggregate_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Record_Aggregate_Node;
  
   function Expression_Kind (Element : Record_Aggregate_Node)
      return Asis.Expression_Kinds is
   begin
      return A_Record_Aggregate;
   end;

   function Clone
     (Element : Record_Aggregate_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Record_Aggregate_Ptr := new Record_Aggregate_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      null;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Record_Aggregate_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Record_Component_Associations
        (Target.all,
         Primary_Association_Lists.Deep_Copy 
           (Record_Component_Associations (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Extension_Aggregate_Expression
     (Element : Extension_Aggregate_Node) return Asis.Expression is
   begin
      return Element.Extension_Aggregate_Expression;
   end Extension_Aggregate_Expression;

   procedure Set_Extension_Aggregate_Expression
     (Element : in out Extension_Aggregate_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Extension_Aggregate_Expression := Value;
   end Set_Extension_Aggregate_Expression;

   function New_Extension_Aggregate_Node
     (The_Context : ASIS.Context)
      return Extension_Aggregate_Ptr
   is
      Result : Extension_Aggregate_Ptr :=
       new Extension_Aggregate_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Extension_Aggregate_Node;
  
   function Expression_Kind (Element : Extension_Aggregate_Node)
      return Asis.Expression_Kinds is
   begin
      return An_Extension_Aggregate;
   end;

   function Children (Element : access Extension_Aggregate_Node)
     return Traverse_List is
   begin
      return ((False, Element.Extension_Aggregate_Expression'Access),
        (True, Asis.Element (Element.Record_Component_Associations)));
   end Children;

   function Clone
     (Element : Extension_Aggregate_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Extension_Aggregate_Ptr := new Extension_Aggregate_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      null;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Extension_Aggregate_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Extension_Aggregate_Expression :=
        Copy (Cloner, Extension_Aggregate_Expression (Source.all), Asis.Element (Target));
      Set_Record_Component_Associations
        (Target.all,
         Primary_Association_Lists.Deep_Copy 
           (Record_Component_Associations (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Array_Component_Associations
     (Element : Base_Array_Aggregate_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Association_Lists.To_Element_List
        (Element.Array_Component_Associations, Include_Pragmas);
   end Array_Component_Associations;

   procedure Set_Array_Component_Associations
     (Element : in out Base_Array_Aggregate_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Array_Component_Associations := Primary_Association_Lists.List (Value);
   end Set_Array_Component_Associations;

   function Array_Component_Associations_List
     (Element : Base_Array_Aggregate_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Array_Component_Associations);
   end Array_Component_Associations_List;

   function Children (Element : access Base_Array_Aggregate_Node)
     return Traverse_List is
   begin
      return (1 => (True, Asis.Element (Element.Array_Component_Associations)));
   end Children;

   function New_Positional_Array_Aggregate_Node
     (The_Context : ASIS.Context)
      return Positional_Array_Aggregate_Ptr
   is
      Result : Positional_Array_Aggregate_Ptr :=
       new Positional_Array_Aggregate_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Positional_Array_Aggregate_Node;
  
   function Expression_Kind (Element : Positional_Array_Aggregate_Node)
      return Asis.Expression_Kinds is
   begin
      return A_Positional_Array_Aggregate;
   end;

   function Clone
     (Element : Positional_Array_Aggregate_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Positional_Array_Aggregate_Ptr := new Positional_Array_Aggregate_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Positional_Array_Aggregate_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Array_Component_Associations
        (Target.all,
         Primary_Association_Lists.Deep_Copy 
           (Array_Component_Associations (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Named_Array_Aggregate_Node
     (The_Context : ASIS.Context)
      return Named_Array_Aggregate_Ptr
   is
      Result : Named_Array_Aggregate_Ptr :=
       new Named_Array_Aggregate_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Named_Array_Aggregate_Node;
  
   function Expression_Kind (Element : Named_Array_Aggregate_Node)
      return Asis.Expression_Kinds is
   begin
      return A_Named_Array_Aggregate;
   end;

   function Clone
     (Element : Named_Array_Aggregate_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Named_Array_Aggregate_Ptr := new Named_Array_Aggregate_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Named_Array_Aggregate_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Array_Component_Associations
        (Target.all,
         Primary_Association_Lists.Deep_Copy 
           (Array_Component_Associations (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Short_Circuit_Operation_Left_Expression
     (Element : Base_Short_Circuit_Node) return Asis.Expression is
   begin
      return Element.Short_Circuit_Operation_Left_Expression;
   end Short_Circuit_Operation_Left_Expression;

   procedure Set_Short_Circuit_Operation_Left_Expression
     (Element : in out Base_Short_Circuit_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Short_Circuit_Operation_Left_Expression := Value;
   end Set_Short_Circuit_Operation_Left_Expression;

   function Short_Circuit_Operation_Right_Expression
     (Element : Base_Short_Circuit_Node) return Asis.Expression is
   begin
      return Element.Short_Circuit_Operation_Right_Expression;
   end Short_Circuit_Operation_Right_Expression;

   procedure Set_Short_Circuit_Operation_Right_Expression
     (Element : in out Base_Short_Circuit_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Short_Circuit_Operation_Right_Expression := Value;
   end Set_Short_Circuit_Operation_Right_Expression;

   function Children (Element : access Base_Short_Circuit_Node)
     return Traverse_List is
   begin
      return ((False, Element.Short_Circuit_Operation_Left_Expression'Access),
        (False, Element.Short_Circuit_Operation_Right_Expression'Access));
   end Children;

   function New_And_Then_Short_Circuit_Node
     (The_Context : ASIS.Context)
      return And_Then_Short_Circuit_Ptr
   is
      Result : And_Then_Short_Circuit_Ptr :=
       new And_Then_Short_Circuit_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_And_Then_Short_Circuit_Node;
  
   function Expression_Kind (Element : And_Then_Short_Circuit_Node)
      return Asis.Expression_Kinds is
   begin
      return An_And_Then_Short_Circuit;
   end;

   function Clone
     (Element : And_Then_Short_Circuit_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant And_Then_Short_Circuit_Ptr := new And_Then_Short_Circuit_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access And_Then_Short_Circuit_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Short_Circuit_Operation_Left_Expression :=
        Copy (Cloner, Short_Circuit_Operation_Left_Expression (Source.all), Asis.Element (Target));
      Target.Short_Circuit_Operation_Right_Expression :=
        Copy (Cloner, Short_Circuit_Operation_Right_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function New_Or_Else_Short_Circuit_Node
     (The_Context : ASIS.Context)
      return Or_Else_Short_Circuit_Ptr
   is
      Result : Or_Else_Short_Circuit_Ptr :=
       new Or_Else_Short_Circuit_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Or_Else_Short_Circuit_Node;
  
   function Expression_Kind (Element : Or_Else_Short_Circuit_Node)
      return Asis.Expression_Kinds is
   begin
      return An_Or_Else_Short_Circuit;
   end;

   function Clone
     (Element : Or_Else_Short_Circuit_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Or_Else_Short_Circuit_Ptr := new Or_Else_Short_Circuit_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Or_Else_Short_Circuit_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Short_Circuit_Operation_Left_Expression :=
        Copy (Cloner, Short_Circuit_Operation_Left_Expression (Source.all), Asis.Element (Target));
      Target.Short_Circuit_Operation_Right_Expression :=
        Copy (Cloner, Short_Circuit_Operation_Right_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function Membership_Test_Expression
     (Element : In_Range_Membership_Test_Node) return Asis.Expression is
   begin
      return Element.Membership_Test_Expression;
   end Membership_Test_Expression;

   procedure Set_Membership_Test_Expression
     (Element : in out In_Range_Membership_Test_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Membership_Test_Expression := Value;
   end Set_Membership_Test_Expression;

   function Membership_Test_Range
     (Element : In_Range_Membership_Test_Node) return Asis.Range_Constraint is
   begin
      return Element.Membership_Test_Range;
   end Membership_Test_Range;

   procedure Set_Membership_Test_Range
     (Element : in out In_Range_Membership_Test_Node;
      Value   : in     Asis.Range_Constraint) is
   begin
      Element.Membership_Test_Range := Value;
   end Set_Membership_Test_Range;

   function New_In_Range_Membership_Test_Node
     (The_Context : ASIS.Context)
      return In_Range_Membership_Test_Ptr
   is
      Result : In_Range_Membership_Test_Ptr :=
       new In_Range_Membership_Test_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_In_Range_Membership_Test_Node;
  
   function Expression_Kind (Element : In_Range_Membership_Test_Node)
      return Asis.Expression_Kinds is
   begin
      return An_In_Range_Membership_Test;
   end;

   function Children (Element : access In_Range_Membership_Test_Node)
     return Traverse_List is
   begin
      return ((False, Element.Membership_Test_Expression'Access),
        (False, Element.Membership_Test_Range'Access));
   end Children;

   function Clone
     (Element : In_Range_Membership_Test_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant In_Range_Membership_Test_Ptr := new In_Range_Membership_Test_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access In_Range_Membership_Test_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Membership_Test_Expression :=
        Copy (Cloner, Membership_Test_Expression (Source.all), Asis.Element (Target));
      Target.Membership_Test_Range :=
        Copy (Cloner, Membership_Test_Range (Source.all), Asis.Element (Target));
   end Copy;

   function New_Not_In_Range_Membership_Test_Node
     (The_Context : ASIS.Context)
      return Not_In_Range_Membership_Test_Ptr
   is
      Result : Not_In_Range_Membership_Test_Ptr :=
       new Not_In_Range_Membership_Test_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Not_In_Range_Membership_Test_Node;
  
   function Expression_Kind (Element : Not_In_Range_Membership_Test_Node)
      return Asis.Expression_Kinds is
   begin
      return A_Not_In_Range_Membership_Test;
   end;

   function Clone
     (Element : Not_In_Range_Membership_Test_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Not_In_Range_Membership_Test_Ptr := new Not_In_Range_Membership_Test_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Not_In_Range_Membership_Test_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Membership_Test_Expression :=
        Copy (Cloner, Membership_Test_Expression (Source.all), Asis.Element (Target));
      Target.Membership_Test_Range :=
        Copy (Cloner, Membership_Test_Range (Source.all), Asis.Element (Target));
   end Copy;

   function Membership_Test_Expression
     (Element : In_Type_Membership_Test_Node) return Asis.Expression is
   begin
      return Element.Membership_Test_Expression;
   end Membership_Test_Expression;

   procedure Set_Membership_Test_Expression
     (Element : in out In_Type_Membership_Test_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Membership_Test_Expression := Value;
   end Set_Membership_Test_Expression;

   function Membership_Test_Subtype_Mark
     (Element : In_Type_Membership_Test_Node) return Asis.Expression is
   begin
      return Element.Membership_Test_Subtype_Mark;
   end Membership_Test_Subtype_Mark;

   procedure Set_Membership_Test_Subtype_Mark
     (Element : in out In_Type_Membership_Test_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Membership_Test_Subtype_Mark := Value;
   end Set_Membership_Test_Subtype_Mark;

   function New_In_Type_Membership_Test_Node
     (The_Context : ASIS.Context)
      return In_Type_Membership_Test_Ptr
   is
      Result : In_Type_Membership_Test_Ptr :=
       new In_Type_Membership_Test_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_In_Type_Membership_Test_Node;
  
   function Expression_Kind (Element : In_Type_Membership_Test_Node)
      return Asis.Expression_Kinds is
   begin
      return An_In_Type_Membership_Test;
   end;

   function Children (Element : access In_Type_Membership_Test_Node)
     return Traverse_List is
   begin
      return ((False, Element.Membership_Test_Expression'Access),
        (False, Element.Membership_Test_Subtype_Mark'Access));
   end Children;

   function Clone
     (Element : In_Type_Membership_Test_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant In_Type_Membership_Test_Ptr := new In_Type_Membership_Test_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access In_Type_Membership_Test_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Membership_Test_Expression :=
        Copy (Cloner, Membership_Test_Expression (Source.all), Asis.Element (Target));
      Target.Membership_Test_Subtype_Mark :=
        Copy (Cloner, Membership_Test_Subtype_Mark (Source.all), Asis.Element (Target));
   end Copy;

   function New_Not_In_Type_Membership_Test_Node
     (The_Context : ASIS.Context)
      return Not_In_Type_Membership_Test_Ptr
   is
      Result : Not_In_Type_Membership_Test_Ptr :=
       new Not_In_Type_Membership_Test_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Not_In_Type_Membership_Test_Node;
  
   function Expression_Kind (Element : Not_In_Type_Membership_Test_Node)
      return Asis.Expression_Kinds is
   begin
      return A_Not_In_Type_Membership_Test;
   end;

   function Clone
     (Element : Not_In_Type_Membership_Test_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Not_In_Type_Membership_Test_Ptr := new Not_In_Type_Membership_Test_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Not_In_Type_Membership_Test_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Membership_Test_Expression :=
        Copy (Cloner, Membership_Test_Expression (Source.all), Asis.Element (Target));
      Target.Membership_Test_Subtype_Mark :=
        Copy (Cloner, Membership_Test_Subtype_Mark (Source.all), Asis.Element (Target));
   end Copy;

   function New_Null_Literal_Node
     (The_Context : ASIS.Context)
      return Null_Literal_Ptr
   is
      Result : Null_Literal_Ptr :=
       new Null_Literal_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Null_Literal_Node;
  
   function Expression_Kind (Element : Null_Literal_Node)
      return Asis.Expression_Kinds is
   begin
      return A_Null_Literal;
   end;

   function Clone
     (Element : Null_Literal_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Null_Literal_Ptr := new Null_Literal_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   function Expression_Parenthesized
     (Element : Parenthesized_Expression_Node) return Asis.Expression is
   begin
      return Element.Expression_Parenthesized;
   end Expression_Parenthesized;

   procedure Set_Expression_Parenthesized
     (Element : in out Parenthesized_Expression_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Expression_Parenthesized := Value;
   end Set_Expression_Parenthesized;

   function New_Parenthesized_Expression_Node
     (The_Context : ASIS.Context)
      return Parenthesized_Expression_Ptr
   is
      Result : Parenthesized_Expression_Ptr :=
       new Parenthesized_Expression_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Parenthesized_Expression_Node;
  
   function Expression_Kind (Element : Parenthesized_Expression_Node)
      return Asis.Expression_Kinds is
   begin
      return A_Parenthesized_Expression;
   end;

   function Children (Element : access Parenthesized_Expression_Node)
     return Traverse_List is
   begin
      return (1 => (False, Element.Expression_Parenthesized'Access));
   end Children;

   function Clone
     (Element : Parenthesized_Expression_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Parenthesized_Expression_Ptr := new Parenthesized_Expression_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Parenthesized_Expression_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Expression_Parenthesized :=
        Copy (Cloner, Expression_Parenthesized (Source.all), Asis.Element (Target));
   end Copy;

   function Converted_Or_Qualified_Subtype_Mark
     (Element : Base_Conversion_Node) return Asis.Expression is
   begin
      return Element.Converted_Or_Qualified_Subtype_Mark;
   end Converted_Or_Qualified_Subtype_Mark;

   procedure Set_Converted_Or_Qualified_Subtype_Mark
     (Element : in out Base_Conversion_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Converted_Or_Qualified_Subtype_Mark := Value;
   end Set_Converted_Or_Qualified_Subtype_Mark;

   function Converted_Or_Qualified_Expression
     (Element : Base_Conversion_Node) return Asis.Expression is
   begin
      return Element.Converted_Or_Qualified_Expression;
   end Converted_Or_Qualified_Expression;

   procedure Set_Converted_Or_Qualified_Expression
     (Element : in out Base_Conversion_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Converted_Or_Qualified_Expression := Value;
   end Set_Converted_Or_Qualified_Expression;

   function Children (Element : access Base_Conversion_Node)
     return Traverse_List is
   begin
      return ((False, Element.Converted_Or_Qualified_Subtype_Mark'Access),
        (False, Element.Converted_Or_Qualified_Expression'Access));
   end Children;

   function New_Type_Conversion_Node
     (The_Context : ASIS.Context)
      return Type_Conversion_Ptr
   is
      Result : Type_Conversion_Ptr :=
       new Type_Conversion_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Type_Conversion_Node;
  
   function Expression_Kind (Element : Type_Conversion_Node)
      return Asis.Expression_Kinds is
   begin
      return A_Type_Conversion;
   end;

   function Clone
     (Element : Type_Conversion_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Type_Conversion_Ptr := new Type_Conversion_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Type_Conversion_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Converted_Or_Qualified_Subtype_Mark :=
        Copy (Cloner, Converted_Or_Qualified_Subtype_Mark (Source.all), Asis.Element (Target));
      Target.Converted_Or_Qualified_Expression :=
        Copy (Cloner, Converted_Or_Qualified_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function New_Qualified_Expression_Node
     (The_Context : ASIS.Context)
      return Qualified_Expression_Ptr
   is
      Result : Qualified_Expression_Ptr :=
       new Qualified_Expression_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Qualified_Expression_Node;
  
   function Expression_Kind (Element : Qualified_Expression_Node)
      return Asis.Expression_Kinds is
   begin
      return A_Qualified_Expression;
   end;

   function Clone
     (Element : Qualified_Expression_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Qualified_Expression_Ptr := new Qualified_Expression_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Qualified_Expression_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Converted_Or_Qualified_Subtype_Mark :=
        Copy (Cloner, Converted_Or_Qualified_Subtype_Mark (Source.all), Asis.Element (Target));
      Target.Converted_Or_Qualified_Expression :=
        Copy (Cloner, Converted_Or_Qualified_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function Allocator_Subtype_Indication
     (Element : Allocation_From_Subtype_Node) return Asis.Subtype_Indication is
   begin
      return Element.Allocator_Subtype_Indication;
   end Allocator_Subtype_Indication;

   procedure Set_Allocator_Subtype_Indication
     (Element : in out Allocation_From_Subtype_Node;
      Value   : in     Asis.Subtype_Indication) is
   begin
      Element.Allocator_Subtype_Indication := Value;
   end Set_Allocator_Subtype_Indication;

   function New_Allocation_From_Subtype_Node
     (The_Context : ASIS.Context)
      return Allocation_From_Subtype_Ptr
   is
      Result : Allocation_From_Subtype_Ptr :=
       new Allocation_From_Subtype_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Allocation_From_Subtype_Node;
  
   function Expression_Kind (Element : Allocation_From_Subtype_Node)
      return Asis.Expression_Kinds is
   begin
      return An_Allocation_From_Subtype;
   end;

   function Children (Element : access Allocation_From_Subtype_Node)
     return Traverse_List is
   begin
      return (1 => (False, Element.Allocator_Subtype_Indication'Access));
   end Children;

   function Clone
     (Element : Allocation_From_Subtype_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Allocation_From_Subtype_Ptr := new Allocation_From_Subtype_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Allocation_From_Subtype_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Allocator_Subtype_Indication :=
        Copy (Cloner, Allocator_Subtype_Indication (Source.all), Asis.Element (Target));
   end Copy;

   function Allocator_Qualified_Expression
     (Element : Allocation_From_Qualified_Expression_Node) return Asis.Expression is
   begin
      return Element.Allocator_Qualified_Expression;
   end Allocator_Qualified_Expression;

   procedure Set_Allocator_Qualified_Expression
     (Element : in out Allocation_From_Qualified_Expression_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Allocator_Qualified_Expression := Value;
   end Set_Allocator_Qualified_Expression;

   function New_Allocation_From_Qualified_Expression_Node
     (The_Context : ASIS.Context)
      return Allocation_From_Qualified_Expression_Ptr
   is
      Result : Allocation_From_Qualified_Expression_Ptr :=
       new Allocation_From_Qualified_Expression_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Allocation_From_Qualified_Expression_Node;
  
   function Expression_Kind (Element : Allocation_From_Qualified_Expression_Node)
      return Asis.Expression_Kinds is
   begin
      return An_Allocation_From_Qualified_Expression;
   end;

   function Children (Element : access Allocation_From_Qualified_Expression_Node)
     return Traverse_List is
   begin
      return (1 => (False, Element.Allocator_Qualified_Expression'Access));
   end Children;

   function Clone
     (Element : Allocation_From_Qualified_Expression_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Allocation_From_Qualified_Expression_Ptr := new Allocation_From_Qualified_Expression_Node;
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
      Result.Corresponding_Expression_Type := Element.Corresponding_Expression_Type;
      Result.Is_Static_Expression := Element.Is_Static_Expression;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Allocation_From_Qualified_Expression_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Allocator_Qualified_Expression :=
        Copy (Cloner, Allocator_Qualified_Expression (Source.all), Asis.Element (Target));
   end Copy;

end Asis.Gela.Elements.Expr;
