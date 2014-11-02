
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

package body Asis.Gela.Units is

   function Unit_Kind
     (Element : Any_Compilation_Unit_Node) return Asis.Unit_Kinds is
   begin
      return Element.Unit_Kind;
   end Unit_Kind;

   procedure Set_Unit_Kind
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Unit_Kinds) is
   begin
      Element.Unit_Kind := Value;
   end Set_Unit_Kind;

   function Unit_Class
     (Element : Any_Compilation_Unit_Node) return Asis.Unit_Classes is
   begin
      return Element.Unit_Class;
   end Unit_Class;

   procedure Set_Unit_Class
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Unit_Classes) is
   begin
      Element.Unit_Class := Value;
   end Set_Unit_Class;

   function Unit_Origin
     (Element : Any_Compilation_Unit_Node) return Asis.Unit_Origins is
   begin
      return Element.Unit_Origin;
   end Unit_Origin;

   procedure Set_Unit_Origin
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Unit_Origins) is
   begin
      Element.Unit_Origin := Value;
   end Set_Unit_Origin;

   function Enclosing_Context
     (Element : Any_Compilation_Unit_Node) return Asis.Context is
   begin
      return Element.Enclosing_Context;
   end Enclosing_Context;

   procedure Set_Enclosing_Context
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Context) is
   begin
      Element.Enclosing_Context := Value;
   end Set_Enclosing_Context;

   function Next_Element
     (Element : Any_Compilation_Unit_Node) return Asis.Element is
   begin
      return Element.Next_Element;
   end Next_Element;

   procedure Set_Next_Element
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Next_Element := Value;
   end Set_Next_Element;

   function Corresponding_Parent_Declaration
     (Element : Any_Compilation_Unit_Node) return Asis.Compilation_Unit is
   begin
      return Element.Corresponding_Parent_Declaration;
   end Corresponding_Parent_Declaration;

   procedure Set_Corresponding_Parent_Declaration
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Compilation_Unit) is
   begin
      Element.Corresponding_Parent_Declaration := Value;
   end Set_Corresponding_Parent_Declaration;

   function Corresponding_Children
     (Element : Any_Compilation_Unit_Node) return Asis.Compilation_Unit_List is
   begin
      return Secondary_Unit_Lists.To_Compilation_Unit_List
        (Element.Corresponding_Children);
   end Corresponding_Children;

   procedure Add_To_Corresponding_Children
     (Element : in out Any_Compilation_Unit_Node;
      Item    : in     Asis.Compilation_Unit) is
   begin
      Secondary_Unit_Lists.Add (Element.Corresponding_Children, Asis.Element (Item));
   end Add_To_Corresponding_Children;

   function Corresponding_Declaration
     (Element : Any_Compilation_Unit_Node) return Asis.Compilation_Unit is
   begin
      return Element.Corresponding_Declaration;
   end Corresponding_Declaration;

   procedure Set_Corresponding_Declaration
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Compilation_Unit) is
   begin
      Element.Corresponding_Declaration := Value;
   end Set_Corresponding_Declaration;

   function Corresponding_Body
     (Element : Any_Compilation_Unit_Node) return Asis.Compilation_Unit is
   begin
      return Element.Corresponding_Body;
   end Corresponding_Body;

   procedure Set_Corresponding_Body
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Compilation_Unit) is
   begin
      Element.Corresponding_Body := Value;
   end Set_Corresponding_Body;

   function Unit_Full_Name
     (Element : Any_Compilation_Unit_Node) return Wide_String is
   begin
      return W.To_Wide_String (Element.Unit_Full_Name);
   end Unit_Full_Name;

   procedure Set_Unit_Full_Name
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String) is
   begin
      Element.Unit_Full_Name := W.To_Unbounded_Wide_String (Value);
   end Set_Unit_Full_Name;

   function Unique_Name
     (Element : Any_Compilation_Unit_Node) return Wide_String is
   begin
      return W.To_Wide_String (Element.Unique_Name);
   end Unique_Name;

   procedure Set_Unique_Name
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String) is
   begin
      Element.Unique_Name := W.To_Unbounded_Wide_String (Value);
   end Set_Unique_Name;

   function Can_Be_Main_Program
     (Element : Any_Compilation_Unit_Node) return Boolean is
   begin
      return Element.Can_Be_Main_Program;
   end Can_Be_Main_Program;

   procedure Set_Can_Be_Main_Program
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Boolean) is
   begin
      Element.Can_Be_Main_Program := Value;
   end Set_Can_Be_Main_Program;

   function Is_Body_Required
     (Element : Any_Compilation_Unit_Node) return Boolean is
   begin
      return Element.Is_Body_Required;
   end Is_Body_Required;

   procedure Set_Is_Body_Required
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Body_Required := Value;
   end Set_Is_Body_Required;

   function Text_Name
     (Element : Any_Compilation_Unit_Node) return Wide_String is
   begin
      return W.To_Wide_String (Element.Text_Name);
   end Text_Name;

   procedure Set_Text_Name
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String) is
   begin
      Element.Text_Name := W.To_Unbounded_Wide_String (Value);
   end Set_Text_Name;

   function Text_Form
     (Element : Any_Compilation_Unit_Node) return Wide_String is
   begin
      return W.To_Wide_String (Element.Text_Form);
   end Text_Form;

   procedure Set_Text_Form
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String) is
   begin
      Element.Text_Form := W.To_Unbounded_Wide_String (Value);
   end Set_Text_Form;

   function Object_Name
     (Element : Any_Compilation_Unit_Node) return Wide_String is
   begin
      return W.To_Wide_String (Element.Object_Name);
   end Object_Name;

   procedure Set_Object_Name
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String) is
   begin
      Element.Object_Name := W.To_Unbounded_Wide_String (Value);
   end Set_Object_Name;

   function Object_Form
     (Element : Any_Compilation_Unit_Node) return Wide_String is
   begin
      return W.To_Wide_String (Element.Object_Form);
   end Object_Form;

   procedure Set_Object_Form
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String) is
   begin
      Element.Object_Form := W.To_Unbounded_Wide_String (Value);
   end Set_Object_Form;

   function Compilation_Command_Line_Options
     (Element : Any_Compilation_Unit_Node) return Wide_String is
   begin
      return W.To_Wide_String (Element.Compilation_Command_Line_Options);
   end Compilation_Command_Line_Options;

   procedure Set_Compilation_Command_Line_Options
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String) is
   begin
      Element.Compilation_Command_Line_Options := W.To_Unbounded_Wide_String (Value);
   end Set_Compilation_Command_Line_Options;

   function Corresponding_Subunit_Parent_Body
     (Element : Any_Compilation_Unit_Node) return Asis.Compilation_Unit is
   begin
      return Element.Corresponding_Subunit_Parent_Body;
   end Corresponding_Subunit_Parent_Body;

   procedure Set_Corresponding_Subunit_Parent_Body
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Compilation_Unit) is
   begin
      Element.Corresponding_Subunit_Parent_Body := Value;
   end Set_Corresponding_Subunit_Parent_Body;

   function Subunits
     (Element : Any_Compilation_Unit_Node) return Asis.Compilation_Unit_List is
   begin
      return Secondary_Unit_Lists.To_Compilation_Unit_List
        (Element.Subunits);
   end Subunits;

   procedure Add_To_Subunits
     (Element : in out Any_Compilation_Unit_Node;
      Item    : in     Asis.Compilation_Unit) is
   begin
      Secondary_Unit_Lists.Add (Element.Subunits, Asis.Element (Item));
   end Add_To_Subunits;

   function Unit_Declaration
     (Element : Any_Compilation_Unit_Node) return Asis.Element is
   begin
      return Element.Unit_Declaration;
   end Unit_Declaration;

   procedure Set_Unit_Declaration
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Unit_Declaration := Value;
   end Set_Unit_Declaration;

   function Context_Clause_Elements
     (Element : Any_Compilation_Unit_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Clause_Lists.To_Element_List
        (Element.Context_Clause_Elements, Include_Pragmas);
   end Context_Clause_Elements;

   procedure Set_Context_Clause_Elements
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Context_Clause_Elements := Primary_Clause_Lists.List (Value);
   end Set_Context_Clause_Elements;

   function Context_Clause_Elements_List
     (Element : Any_Compilation_Unit_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Context_Clause_Elements);
   end Context_Clause_Elements_List;

   function Compilation_Pragmas
     (Element : Any_Compilation_Unit_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Pragma_Lists.To_Element_List
        (Element.Compilation_Pragmas, Include_Pragmas);
   end Compilation_Pragmas;

   procedure Add_To_Compilation_Pragmas
     (Element : in out Any_Compilation_Unit_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Pragma_Lists.Add (Element.Compilation_Pragmas, Item);
   end Add_To_Compilation_Pragmas;

   function Start_Position
     (Element : Any_Compilation_Unit_Node) return Asis.Text_Position is
   begin
      return Element.Start_Position;
   end Start_Position;

   procedure Set_Start_Position
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Text_Position) is
   begin
      Element.Start_Position := Value;
   end Set_Start_Position;

   function End_Position
     (Element : Any_Compilation_Unit_Node) return Asis.Text_Position is
   begin
      return Element.End_Position;
   end End_Position;

   procedure Set_End_Position
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Text_Position) is
   begin
      Element.End_Position := Value;
   end Set_End_Position;

   function Separate_Name
     (Element : Any_Compilation_Unit_Node) return Asis.Element is
   begin
      return Element.Separate_Name;
   end Separate_Name;

   procedure Set_Separate_Name
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Separate_Name := Value;
   end Set_Separate_Name;

   function Separate_Name_Image
     (Element : Any_Compilation_Unit_Node) return Wide_String is
   begin
      return W.To_Wide_String (Element.Separate_Name_Image);
   end Separate_Name_Image;

   procedure Set_Separate_Name_Image
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String) is
   begin
      Element.Separate_Name_Image := W.To_Unbounded_Wide_String (Value);
   end Set_Separate_Name_Image;

   function Hash
     (Element : Any_Compilation_Unit_Node) return Asis.ASIS_Integer is
   begin
      return Element.Hash;
   end Hash;

   procedure Set_Hash
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.ASIS_Integer) is
   begin
      Element.Hash := Value;
   end Set_Hash;

   function Compilation
     (Element : Any_Compilation_Unit_Node) return Compilations.Compilation is
   begin
      return Element.Compilation;
   end Compilation;

   procedure Set_Compilation
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Compilations.Compilation) is
   begin
      Element.Compilation := Value;
   end Set_Compilation;

   function New_Any_Compilation_Unit_Node
     (The_Context : ASIS.Context)
      return Any_Compilation_Unit_Ptr
   is
      Result : Any_Compilation_Unit_Ptr :=
       new Any_Compilation_Unit_Node;
   begin

      return Result;
   end New_Any_Compilation_Unit_Node;
  
   function Clone
     (Element : Any_Compilation_Unit_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Any_Compilation_Unit_Ptr := new Any_Compilation_Unit_Node;
   begin
      Result.Unit_Kind := Element.Unit_Kind;
      Result.Unit_Class := Element.Unit_Class;
      Result.Unit_Origin := Element.Unit_Origin;
      Result.Enclosing_Context := Element.Enclosing_Context;
      Result.Corresponding_Parent_Declaration := Element.Corresponding_Parent_Declaration;
      null;
      Result.Corresponding_Declaration := Element.Corresponding_Declaration;
      Result.Corresponding_Body := Element.Corresponding_Body;
      Result.Unit_Full_Name := Element.Unit_Full_Name;
      Result.Unique_Name := Element.Unique_Name;
      Result.Can_Be_Main_Program := Element.Can_Be_Main_Program;
      Result.Is_Body_Required := Element.Is_Body_Required;
      Result.Text_Name := Element.Text_Name;
      Result.Text_Form := Element.Text_Form;
      Result.Object_Name := Element.Object_Name;
      Result.Object_Form := Element.Object_Form;
      Result.Compilation_Command_Line_Options := Element.Compilation_Command_Line_Options;
      Result.Corresponding_Subunit_Parent_Body := Element.Corresponding_Subunit_Parent_Body;
      null;
      Result.Unit_Declaration := Element.Unit_Declaration;
      null;
      Result.Start_Position := Element.Start_Position;
      Result.End_Position := Element.End_Position;
      Result.Separate_Name := Element.Separate_Name;
      Result.Separate_Name_Image := Element.Separate_Name_Image;
      Result.Hash := Element.Hash;
      Result.Compilation := Element.Compilation;
      return Asis.Element (Result);
   end Clone;

end Asis.Gela.Units;
