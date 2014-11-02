
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

package body Asis.Gela.Elements.Clause is

   function Clause_Names
     (Element : Named_Clause_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Expression_Lists.To_Element_List
        (Element.Clause_Names, Include_Pragmas);
   end Clause_Names;

   procedure Set_Clause_Names
     (Element : in out Named_Clause_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Clause_Names := Primary_Expression_Lists.List (Value);
   end Set_Clause_Names;

   function Clause_Names_List
     (Element : Named_Clause_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Clause_Names);
   end Clause_Names_List;

   function Children (Element : access Named_Clause_Node)
     return Traverse_List is
   begin
      return (1 => (True, Asis.Element (Element.Clause_Names)));
   end Children;

   function New_Use_Package_Clause_Node
     (The_Context : ASIS.Context)
      return Use_Package_Clause_Ptr
   is
      Result : Use_Package_Clause_Ptr :=
       new Use_Package_Clause_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Use_Package_Clause_Node;
  
   function Clause_Kind (Element : Use_Package_Clause_Node)
      return Asis.Clause_Kinds is
   begin
      return A_Use_Package_Clause;
   end;

   function Clone
     (Element : Use_Package_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Use_Package_Clause_Ptr := new Use_Package_Clause_Node;
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
      Target : access Use_Package_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Clause_Names
        (Target.all,
         Primary_Expression_Lists.Deep_Copy 
           (Clause_Names (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Use_Type_Clause_Node
     (The_Context : ASIS.Context)
      return Use_Type_Clause_Ptr
   is
      Result : Use_Type_Clause_Ptr :=
       new Use_Type_Clause_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Use_Type_Clause_Node;
  
   function Clause_Kind (Element : Use_Type_Clause_Node)
      return Asis.Clause_Kinds is
   begin
      return A_Use_Type_Clause;
   end;

   function Clone
     (Element : Use_Type_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Use_Type_Clause_Ptr := new Use_Type_Clause_Node;
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
      Target : access Use_Type_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Clause_Names
        (Target.all,
         Primary_Expression_Lists.Deep_Copy 
           (Clause_Names (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Has_Limited
     (Element : With_Clause_Node) return Boolean is
   begin
      return Element.Has_Limited;
   end Has_Limited;

   procedure Set_Has_Limited
     (Element : in out With_Clause_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Limited := Value;
   end Set_Has_Limited;

   function Has_Private
     (Element : With_Clause_Node) return Boolean is
   begin
      return Element.Has_Private;
   end Has_Private;

   procedure Set_Has_Private
     (Element : in out With_Clause_Node;
      Value   : in     Boolean) is
   begin
      Element.Has_Private := Value;
   end Set_Has_Private;

   function New_With_Clause_Node
     (The_Context : ASIS.Context)
      return With_Clause_Ptr
   is
      Result : With_Clause_Ptr :=
       new With_Clause_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_With_Clause_Node;
  
   function Clause_Kind (Element : With_Clause_Node)
      return Asis.Clause_Kinds is
   begin
      return A_With_Clause;
   end;

   function Clone
     (Element : With_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant With_Clause_Ptr := new With_Clause_Node;
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
      Result.Has_Limited := Element.Has_Limited;
      Result.Has_Private := Element.Has_Private;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access With_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Clause_Names
        (Target.all,
         Primary_Expression_Lists.Deep_Copy 
           (Clause_Names (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Representation_Clause_Name
     (Element : Representation_Clause_Node) return Asis.Name is
   begin
      return Element.Representation_Clause_Name;
   end Representation_Clause_Name;

   procedure Set_Representation_Clause_Name
     (Element : in out Representation_Clause_Node;
      Value   : in     Asis.Name) is
   begin
      Element.Representation_Clause_Name := Value;
   end Set_Representation_Clause_Name;

   function Clause_Kind (Element : Representation_Clause_Node)
      return Asis.Clause_Kinds is
   begin
      return A_Representation_Clause;
   end;

   function Children (Element : access Representation_Clause_Node)
     return Traverse_List is
   begin
      return (1 => (False, Element.Representation_Clause_Name'Access));
   end Children;

   function Component_Clause_Position
     (Element : Component_Clause_Node) return Asis.Expression is
   begin
      return Element.Component_Clause_Position;
   end Component_Clause_Position;

   procedure Set_Component_Clause_Position
     (Element : in out Component_Clause_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Component_Clause_Position := Value;
   end Set_Component_Clause_Position;

   function Component_Clause_Range
     (Element : Component_Clause_Node) return Asis.Discrete_Range is
   begin
      return Element.Component_Clause_Range;
   end Component_Clause_Range;

   procedure Set_Component_Clause_Range
     (Element : in out Component_Clause_Node;
      Value   : in     Asis.Discrete_Range) is
   begin
      Element.Component_Clause_Range := Value;
   end Set_Component_Clause_Range;

   function Representation_Clause_Name
     (Element : Component_Clause_Node) return Asis.Name is
   begin
      return Element.Representation_Clause_Name;
   end Representation_Clause_Name;

   procedure Set_Representation_Clause_Name
     (Element : in out Component_Clause_Node;
      Value   : in     Asis.Name) is
   begin
      Element.Representation_Clause_Name := Value;
   end Set_Representation_Clause_Name;

   function New_Component_Clause_Node
     (The_Context : ASIS.Context)
      return Component_Clause_Ptr
   is
      Result : Component_Clause_Ptr :=
       new Component_Clause_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Component_Clause_Node;
  
   function Clause_Kind (Element : Component_Clause_Node)
      return Asis.Clause_Kinds is
   begin
      return A_Component_Clause;
   end;

   function Children (Element : access Component_Clause_Node)
     return Traverse_List is
   begin
      return ((False, Element.Representation_Clause_Name'Access),
        (False, Element.Component_Clause_Position'Access),
        (False, Element.Component_Clause_Range'Access));
   end Children;

   function Clone
     (Element : Component_Clause_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Component_Clause_Ptr := new Component_Clause_Node;
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
      Target : access Component_Clause_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Representation_Clause_Name :=
        Copy (Cloner, Representation_Clause_Name (Source.all), Asis.Element (Target));
      Target.Component_Clause_Position :=
        Copy (Cloner, Component_Clause_Position (Source.all), Asis.Element (Target));
      Target.Component_Clause_Range :=
        Copy (Cloner, Component_Clause_Range (Source.all), Asis.Element (Target));
   end Copy;

end Asis.Gela.Elements.Clause;
