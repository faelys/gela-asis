
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

package body Asis.Gela.Elements.Clause.Rep is

   function Mod_Clause_Expression
     (Element : Record_Representation_Clause_Node) return Asis.Expression is
   begin
      return Element.Mod_Clause_Expression;
   end Mod_Clause_Expression;

   procedure Set_Mod_Clause_Expression
     (Element : in out Record_Representation_Clause_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Mod_Clause_Expression := Value;
   end Set_Mod_Clause_Expression;

   function Component_Clauses
     (Element : Record_Representation_Clause_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Clause_Lists.To_Element_List
        (Element.Component_Clauses, Include_Pragmas);
   end Component_Clauses;

   procedure Set_Component_Clauses
     (Element : in out Record_Representation_Clause_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Component_Clauses := Primary_Clause_Lists.List (Value);
   end Set_Component_Clauses;

   function Component_Clauses_List
     (Element : Record_Representation_Clause_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Component_Clauses);
   end Component_Clauses_List;

   function New_Record_Representation_Clause_Node
     (The_Context : ASIS.Context)
      return Record_Representation_Clause_Ptr
   is
      Result : Record_Representation_Clause_Ptr :=
       new Record_Representation_Clause_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Record_Representation_Clause_Node;
  
   function Representation_Clause_Kind (Element : Record_Representation_Clause_Node)
      return Asis.Representation_Clause_Kinds is
   begin
      return A_Record_Representation_Clause;
   end;

   function Children (Element : access Record_Representation_Clause_Node)
     return Traverse_List is
   begin
      return ((False, Element.Representation_Clause_Name'Access),
        (False, Element.Mod_Clause_Expression'Access),
        (True, Asis.Element (Element.Component_Clauses)));
   end Children;

   function Clone
     (Element : Record_Representation_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Record_Representation_Clause_Ptr := new Record_Representation_Clause_Node;
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
      Target : access Record_Representation_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Representation_Clause_Name :=
        Copy (Cloner, Representation_Clause_Name (Source.all), Asis.Element (Target));
      Target.Mod_Clause_Expression :=
        Copy (Cloner, Mod_Clause_Expression (Source.all), Asis.Element (Target));
      Set_Component_Clauses
        (Target.all,
         Primary_Clause_Lists.Deep_Copy 
           (Component_Clauses (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Representation_Clause_Expression
     (Element : At_Clause_Node) return Asis.Expression is
   begin
      return Element.Representation_Clause_Expression;
   end Representation_Clause_Expression;

   procedure Set_Representation_Clause_Expression
     (Element : in out At_Clause_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Representation_Clause_Expression := Value;
   end Set_Representation_Clause_Expression;

   function New_At_Clause_Node
     (The_Context : ASIS.Context)
      return At_Clause_Ptr
   is
      Result : At_Clause_Ptr :=
       new At_Clause_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_At_Clause_Node;
  
   function Representation_Clause_Kind (Element : At_Clause_Node)
      return Asis.Representation_Clause_Kinds is
   begin
      return An_At_Clause;
   end;

   function Children (Element : access At_Clause_Node)
     return Traverse_List is
   begin
      return ((False, Element.Representation_Clause_Name'Access),
        (False, Element.Representation_Clause_Expression'Access));
   end Children;

   function Clone
     (Element : At_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant At_Clause_Ptr := new At_Clause_Node;
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
      Target : access At_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Representation_Clause_Name :=
        Copy (Cloner, Representation_Clause_Name (Source.all), Asis.Element (Target));
      Target.Representation_Clause_Expression :=
        Copy (Cloner, Representation_Clause_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function New_Attribute_Definition_Clause_Node
     (The_Context : ASIS.Context)
      return Attribute_Definition_Clause_Ptr
   is
      Result : Attribute_Definition_Clause_Ptr :=
       new Attribute_Definition_Clause_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Attribute_Definition_Clause_Node;
  
   function Representation_Clause_Kind (Element : Attribute_Definition_Clause_Node)
      return Asis.Representation_Clause_Kinds is
   begin
      return An_Attribute_Definition_Clause;
   end;

   function Clone
     (Element : Attribute_Definition_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Attribute_Definition_Clause_Ptr := new Attribute_Definition_Clause_Node;
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
      Target : access Attribute_Definition_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Representation_Clause_Name :=
        Copy (Cloner, Representation_Clause_Name (Source.all), Asis.Element (Target));
      Target.Representation_Clause_Expression :=
        Copy (Cloner, Representation_Clause_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function New_Enumeration_Representation_Clause_Node
     (The_Context : ASIS.Context)
      return Enumeration_Representation_Clause_Ptr
   is
      Result : Enumeration_Representation_Clause_Ptr :=
       new Enumeration_Representation_Clause_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Enumeration_Representation_Clause_Node;
  
   function Representation_Clause_Kind (Element : Enumeration_Representation_Clause_Node)
      return Asis.Representation_Clause_Kinds is
   begin
      return An_Enumeration_Representation_Clause;
   end;

   function Clone
     (Element : Enumeration_Representation_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Enumeration_Representation_Clause_Ptr := new Enumeration_Representation_Clause_Node;
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
      Target : access Enumeration_Representation_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Representation_Clause_Name :=
        Copy (Cloner, Representation_Clause_Name (Source.all), Asis.Element (Target));
      Target.Representation_Clause_Expression :=
        Copy (Cloner, Representation_Clause_Expression (Source.all), Asis.Element (Target));
   end Copy;

end Asis.Gela.Elements.Clause.Rep;
