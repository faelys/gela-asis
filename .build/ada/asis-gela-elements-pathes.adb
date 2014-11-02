
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

package body Asis.Gela.Elements.Pathes is

   function Condition_Expression
     (Element : If_Path_Node) return Asis.Expression is
   begin
      return Element.Condition_Expression;
   end Condition_Expression;

   procedure Set_Condition_Expression
     (Element : in out If_Path_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Condition_Expression := Value;
   end Set_Condition_Expression;

   function New_If_Path_Node
     (The_Context : ASIS.Context)
      return If_Path_Ptr
   is
      Result : If_Path_Ptr :=
       new If_Path_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_If_Path_Node;
  
   function Path_Kind (Element : If_Path_Node)
      return Asis.Path_Kinds is
   begin
      return An_If_Path;
   end;

   function Children (Element : access If_Path_Node)
     return Traverse_List is
   begin
      return ((False, Element.Condition_Expression'Access),
        (True, Asis.Element (Element.Sequence_Of_Statements)));
   end Children;

   function Clone
     (Element : If_Path_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant If_Path_Ptr := new If_Path_Node;
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
      Target : access If_Path_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Condition_Expression :=
        Copy (Cloner, Condition_Expression (Source.all), Asis.Element (Target));
      Set_Sequence_Of_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Sequence_Of_Statements (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Elsif_Path_Node
     (The_Context : ASIS.Context)
      return Elsif_Path_Ptr
   is
      Result : Elsif_Path_Ptr :=
       new Elsif_Path_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Elsif_Path_Node;
  
   function Path_Kind (Element : Elsif_Path_Node)
      return Asis.Path_Kinds is
   begin
      return An_Elsif_Path;
   end;

   function Clone
     (Element : Elsif_Path_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Elsif_Path_Ptr := new Elsif_Path_Node;
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
      Target : access Elsif_Path_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Condition_Expression :=
        Copy (Cloner, Condition_Expression (Source.all), Asis.Element (Target));
      Set_Sequence_Of_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Sequence_Of_Statements (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Else_Path_Node
     (The_Context : ASIS.Context)
      return Else_Path_Ptr
   is
      Result : Else_Path_Ptr :=
       new Else_Path_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Else_Path_Node;
  
   function Path_Kind (Element : Else_Path_Node)
      return Asis.Path_Kinds is
   begin
      return An_Else_Path;
   end;

   function Clone
     (Element : Else_Path_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Else_Path_Ptr := new Else_Path_Node;
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
      Target : access Else_Path_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Sequence_Of_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Sequence_Of_Statements (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Case_Statement_Alternative_Choices
     (Element : Case_Path_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Choise_Lists.To_Element_List
        (Element.Case_Statement_Alternative_Choices, Include_Pragmas);
   end Case_Statement_Alternative_Choices;

   procedure Set_Case_Statement_Alternative_Choices
     (Element : in out Case_Path_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Case_Statement_Alternative_Choices := Primary_Choise_Lists.List (Value);
   end Set_Case_Statement_Alternative_Choices;

   function Case_Statement_Alternative_Choices_List
     (Element : Case_Path_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Case_Statement_Alternative_Choices);
   end Case_Statement_Alternative_Choices_List;

   function Pragmas
     (Element : Case_Path_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Pragma_Lists.To_Element_List
        (Element.Pragmas, Include_Pragmas);
   end Pragmas;

   procedure Set_Pragmas
     (Element : in out Case_Path_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Pragmas := Primary_Pragma_Lists.List (Value);
   end Set_Pragmas;

   function Pragmas_List
     (Element : Case_Path_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Pragmas);
   end Pragmas_List;

   function New_Case_Path_Node
     (The_Context : ASIS.Context)
      return Case_Path_Ptr
   is
      Result : Case_Path_Ptr :=
       new Case_Path_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Case_Path_Node;
  
   function Path_Kind (Element : Case_Path_Node)
      return Asis.Path_Kinds is
   begin
      return A_Case_Path;
   end;

   function Children (Element : access Case_Path_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Case_Statement_Alternative_Choices)),
        (True, Asis.Element (Element.Pragmas)),
        (True, Asis.Element (Element.Sequence_Of_Statements)));
   end Children;

   function Clone
     (Element : Case_Path_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Case_Path_Ptr := new Case_Path_Node;
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
      Target : access Case_Path_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Case_Statement_Alternative_Choices
        (Target.all,
         Primary_Choise_Lists.Deep_Copy 
           (Case_Statement_Alternative_Choices (Source.all), Cloner, Asis.Element (Target)));
      Set_Pragmas
        (Target.all,
         Primary_Pragma_Lists.Deep_Copy 
           (Pragmas (Source.all), Cloner, Asis.Element (Target)));
      Set_Sequence_Of_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Sequence_Of_Statements (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Guard
     (Element : Select_Path_Node) return Asis.Expression is
   begin
      return Element.Guard;
   end Guard;

   procedure Set_Guard
     (Element : in out Select_Path_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Guard := Value;
   end Set_Guard;

   function Pragmas
     (Element : Select_Path_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Pragma_Lists.To_Element_List
        (Element.Pragmas, Include_Pragmas);
   end Pragmas;

   procedure Set_Pragmas
     (Element : in out Select_Path_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Pragmas := Primary_Pragma_Lists.List (Value);
   end Set_Pragmas;

   function Pragmas_List
     (Element : Select_Path_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Pragmas);
   end Pragmas_List;

   function New_Select_Path_Node
     (The_Context : ASIS.Context)
      return Select_Path_Ptr
   is
      Result : Select_Path_Ptr :=
       new Select_Path_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Select_Path_Node;
  
   function Path_Kind (Element : Select_Path_Node)
      return Asis.Path_Kinds is
   begin
      return A_Select_Path;
   end;

   function Children (Element : access Select_Path_Node)
     return Traverse_List is
   begin
      return ((False, Element.Guard'Access),
        (True, Asis.Element (Element.Pragmas)),
        (True, Asis.Element (Element.Sequence_Of_Statements)));
   end Children;

   function Clone
     (Element : Select_Path_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Select_Path_Ptr := new Select_Path_Node;
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
      Target : access Select_Path_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Guard :=
        Copy (Cloner, Guard (Source.all), Asis.Element (Target));
      Set_Pragmas
        (Target.all,
         Primary_Pragma_Lists.Deep_Copy 
           (Pragmas (Source.all), Cloner, Asis.Element (Target)));
      Set_Sequence_Of_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Sequence_Of_Statements (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Or_Path_Node
     (The_Context : ASIS.Context)
      return Or_Path_Ptr
   is
      Result : Or_Path_Ptr :=
       new Or_Path_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Or_Path_Node;
  
   function Path_Kind (Element : Or_Path_Node)
      return Asis.Path_Kinds is
   begin
      return An_Or_Path;
   end;

   function Clone
     (Element : Or_Path_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Or_Path_Ptr := new Or_Path_Node;
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
      Target : access Or_Path_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Guard :=
        Copy (Cloner, Guard (Source.all), Asis.Element (Target));
      Set_Pragmas
        (Target.all,
         Primary_Pragma_Lists.Deep_Copy 
           (Pragmas (Source.all), Cloner, Asis.Element (Target)));
      Set_Sequence_Of_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Sequence_Of_Statements (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Then_Abort_Path_Node
     (The_Context : ASIS.Context)
      return Then_Abort_Path_Ptr
   is
      Result : Then_Abort_Path_Ptr :=
       new Then_Abort_Path_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Then_Abort_Path_Node;
  
   function Path_Kind (Element : Then_Abort_Path_Node)
      return Asis.Path_Kinds is
   begin
      return A_Then_Abort_Path;
   end;

   function Clone
     (Element : Then_Abort_Path_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Then_Abort_Path_Ptr := new Then_Abort_Path_Node;
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
      Target : access Then_Abort_Path_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Sequence_Of_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Sequence_Of_Statements (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

end Asis.Gela.Elements.Pathes;
