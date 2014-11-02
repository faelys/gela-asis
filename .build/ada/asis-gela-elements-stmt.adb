
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

package body Asis.Gela.Elements.Stmt is

   function Statement_Paths
     (Element : Base_Path_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Path_Lists.To_Element_List
        (Element.Statement_Paths, Include_Pragmas);
   end Statement_Paths;

   procedure Set_Statement_Paths
     (Element : in out Base_Path_Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Statement_Paths := Primary_Path_Lists.List (Value);
   end Set_Statement_Paths;

   function Statement_Paths_List
     (Element : Base_Path_Statement_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Statement_Paths);
   end Statement_Paths_List;

   function Children (Element : access Base_Path_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (True, Asis.Element (Element.Statement_Paths)));
   end Children;

   function New_If_Statement_Node
     (The_Context : ASIS.Context)
      return If_Statement_Ptr
   is
      Result : If_Statement_Ptr :=
       new If_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_If_Statement_Node;
  
   function Statement_Kind (Element : If_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return An_If_Statement;
   end;

   function Clone
     (Element : If_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant If_Statement_Ptr := new If_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access If_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Statement_Paths
        (Target.all,
         Primary_Path_Lists.Deep_Copy 
           (Statement_Paths (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Case_Expression
     (Element : Case_Statement_Node) return Asis.Expression is
   begin
      return Element.Case_Expression;
   end Case_Expression;

   procedure Set_Case_Expression
     (Element : in out Case_Statement_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Case_Expression := Value;
   end Set_Case_Expression;

   function New_Case_Statement_Node
     (The_Context : ASIS.Context)
      return Case_Statement_Ptr
   is
      Result : Case_Statement_Ptr :=
       new Case_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Case_Statement_Node;
  
   function Statement_Kind (Element : Case_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Case_Statement;
   end;

   function Children (Element : access Case_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Case_Expression'Access),
        (True, Asis.Element (Element.Statement_Paths)));
   end Children;

   function Clone
     (Element : Case_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Case_Statement_Ptr := new Case_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Case_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Case_Expression :=
        Copy (Cloner, Case_Expression (Source.all), Asis.Element (Target));
      Set_Statement_Paths
        (Target.all,
         Primary_Path_Lists.Deep_Copy 
           (Statement_Paths (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Selective_Accept_Statement_Node
     (The_Context : ASIS.Context)
      return Selective_Accept_Statement_Ptr
   is
      Result : Selective_Accept_Statement_Ptr :=
       new Selective_Accept_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Selective_Accept_Statement_Node;
  
   function Statement_Kind (Element : Selective_Accept_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Selective_Accept_Statement;
   end;

   function Clone
     (Element : Selective_Accept_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Selective_Accept_Statement_Ptr := new Selective_Accept_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Selective_Accept_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Statement_Paths
        (Target.all,
         Primary_Path_Lists.Deep_Copy 
           (Statement_Paths (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Timed_Entry_Call_Statement_Node
     (The_Context : ASIS.Context)
      return Timed_Entry_Call_Statement_Ptr
   is
      Result : Timed_Entry_Call_Statement_Ptr :=
       new Timed_Entry_Call_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Timed_Entry_Call_Statement_Node;
  
   function Statement_Kind (Element : Timed_Entry_Call_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Timed_Entry_Call_Statement;
   end;

   function Clone
     (Element : Timed_Entry_Call_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Timed_Entry_Call_Statement_Ptr := new Timed_Entry_Call_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Timed_Entry_Call_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Statement_Paths
        (Target.all,
         Primary_Path_Lists.Deep_Copy 
           (Statement_Paths (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Conditional_Entry_Call_Statement_Node
     (The_Context : ASIS.Context)
      return Conditional_Entry_Call_Statement_Ptr
   is
      Result : Conditional_Entry_Call_Statement_Ptr :=
       new Conditional_Entry_Call_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Conditional_Entry_Call_Statement_Node;
  
   function Statement_Kind (Element : Conditional_Entry_Call_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Conditional_Entry_Call_Statement;
   end;

   function Clone
     (Element : Conditional_Entry_Call_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Conditional_Entry_Call_Statement_Ptr := new Conditional_Entry_Call_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Conditional_Entry_Call_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Statement_Paths
        (Target.all,
         Primary_Path_Lists.Deep_Copy 
           (Statement_Paths (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Asynchronous_Select_Statement_Node
     (The_Context : ASIS.Context)
      return Asynchronous_Select_Statement_Ptr
   is
      Result : Asynchronous_Select_Statement_Ptr :=
       new Asynchronous_Select_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Asynchronous_Select_Statement_Node;
  
   function Statement_Kind (Element : Asynchronous_Select_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return An_Asynchronous_Select_Statement;
   end;

   function Clone
     (Element : Asynchronous_Select_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Asynchronous_Select_Statement_Ptr := new Asynchronous_Select_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Asynchronous_Select_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Statement_Paths
        (Target.all,
         Primary_Path_Lists.Deep_Copy 
           (Statement_Paths (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Null_Statement_Node
     (The_Context : ASIS.Context)
      return Null_Statement_Ptr
   is
      Result : Null_Statement_Ptr :=
       new Null_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Null_Statement_Node;
  
   function Statement_Kind (Element : Null_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Null_Statement;
   end;

   function Clone
     (Element : Null_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Null_Statement_Ptr := new Null_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Null_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Assignment_Variable_Name
     (Element : Assignment_Statement_Node) return Asis.Expression is
   begin
      return Element.Assignment_Variable_Name;
   end Assignment_Variable_Name;

   procedure Set_Assignment_Variable_Name
     (Element : in out Assignment_Statement_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Assignment_Variable_Name := Value;
   end Set_Assignment_Variable_Name;

   function Assignment_Expression
     (Element : Assignment_Statement_Node) return Asis.Expression is
   begin
      return Element.Assignment_Expression;
   end Assignment_Expression;

   procedure Set_Assignment_Expression
     (Element : in out Assignment_Statement_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Assignment_Expression := Value;
   end Set_Assignment_Expression;

   function New_Assignment_Statement_Node
     (The_Context : ASIS.Context)
      return Assignment_Statement_Ptr
   is
      Result : Assignment_Statement_Ptr :=
       new Assignment_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Assignment_Statement_Node;
  
   function Statement_Kind (Element : Assignment_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return An_Assignment_Statement;
   end;

   function Children (Element : access Assignment_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Assignment_Variable_Name'Access),
        (False, Element.Assignment_Expression'Access));
   end Children;

   function Clone
     (Element : Assignment_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Assignment_Statement_Ptr := new Assignment_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Assignment_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Assignment_Variable_Name :=
        Copy (Cloner, Assignment_Variable_Name (Source.all), Asis.Element (Target));
      Target.Assignment_Expression :=
        Copy (Cloner, Assignment_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function Statement_Identifier
     (Element : Loop_Statement_Node) return Asis.Defining_Name is
   begin
      return Element.Statement_Identifier;
   end Statement_Identifier;

   procedure Set_Statement_Identifier
     (Element : in out Loop_Statement_Node;
      Value   : in     Asis.Defining_Name) is
   begin
      Element.Statement_Identifier := Value;
   end Set_Statement_Identifier;

   function Back_Identifier
     (Element : Loop_Statement_Node) return Asis.Identifier is
   begin
      return Element.Back_Identifier;
   end Back_Identifier;

   procedure Set_Back_Identifier
     (Element : in out Loop_Statement_Node;
      Value   : in     Asis.Identifier) is
   begin
      Element.Back_Identifier := Value;
   end Set_Back_Identifier;

   function Is_Name_Repeated
     (Element : Loop_Statement_Node) return Boolean is
   begin
      return Element.Is_Name_Repeated;
   end Is_Name_Repeated;

   procedure Set_Is_Name_Repeated
     (Element : in out Loop_Statement_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Name_Repeated := Value;
   end Set_Is_Name_Repeated;

   function Loop_Statements
     (Element : Loop_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Statement_Lists.To_Element_List
        (Element.Loop_Statements, Include_Pragmas);
   end Loop_Statements;

   procedure Set_Loop_Statements
     (Element : in out Loop_Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Loop_Statements := Primary_Statement_Lists.List (Value);
   end Set_Loop_Statements;

   function Loop_Statements_List
     (Element : Loop_Statement_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Loop_Statements);
   end Loop_Statements_List;

   function New_Loop_Statement_Node
     (The_Context : ASIS.Context)
      return Loop_Statement_Ptr
   is
      Result : Loop_Statement_Ptr :=
       new Loop_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Loop_Statement_Node;
  
   function Statement_Kind (Element : Loop_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Loop_Statement;
   end;

   function Children (Element : access Loop_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Statement_Identifier'Access),
        (True, Asis.Element (Element.Loop_Statements)));
   end Children;

   function Clone
     (Element : Loop_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Loop_Statement_Ptr := new Loop_Statement_Node;
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
      Result.Place := Element.Place;
      Result.Back_Identifier := Element.Back_Identifier;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Loop_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Statement_Identifier :=
        Copy (Cloner, Statement_Identifier (Source.all), Asis.Element (Target));
      Set_Loop_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Loop_Statements (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function While_Condition
     (Element : While_Loop_Statement_Node) return Asis.Expression is
   begin
      return Element.While_Condition;
   end While_Condition;

   procedure Set_While_Condition
     (Element : in out While_Loop_Statement_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.While_Condition := Value;
   end Set_While_Condition;

   function New_While_Loop_Statement_Node
     (The_Context : ASIS.Context)
      return While_Loop_Statement_Ptr
   is
      Result : While_Loop_Statement_Ptr :=
       new While_Loop_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_While_Loop_Statement_Node;
  
   function Statement_Kind (Element : While_Loop_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_While_Loop_Statement;
   end;

   function Children (Element : access While_Loop_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Statement_Identifier'Access),
        (False, Element.While_Condition'Access),
        (True, Asis.Element (Element.Loop_Statements)));
   end Children;

   function Clone
     (Element : While_Loop_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant While_Loop_Statement_Ptr := new While_Loop_Statement_Node;
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
      Result.Place := Element.Place;
      Result.Back_Identifier := Element.Back_Identifier;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access While_Loop_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Statement_Identifier :=
        Copy (Cloner, Statement_Identifier (Source.all), Asis.Element (Target));
      Target.While_Condition :=
        Copy (Cloner, While_Condition (Source.all), Asis.Element (Target));
      Set_Loop_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Loop_Statements (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Loop_Parameter_Specification
     (Element : For_Loop_Statement_Node) return Asis.Declaration is
   begin
      return Element.Loop_Parameter_Specification;
   end Loop_Parameter_Specification;

   procedure Set_Loop_Parameter_Specification
     (Element : in out For_Loop_Statement_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Loop_Parameter_Specification := Value;
   end Set_Loop_Parameter_Specification;

   function New_For_Loop_Statement_Node
     (The_Context : ASIS.Context)
      return For_Loop_Statement_Ptr
   is
      Result : For_Loop_Statement_Ptr :=
       new For_Loop_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_For_Loop_Statement_Node;
  
   function Statement_Kind (Element : For_Loop_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_For_Loop_Statement;
   end;

   function Children (Element : access For_Loop_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Statement_Identifier'Access),
        (False, Element.Loop_Parameter_Specification'Access),
        (True, Asis.Element (Element.Loop_Statements)));
   end Children;

   function Clone
     (Element : For_Loop_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant For_Loop_Statement_Ptr := new For_Loop_Statement_Node;
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
      Result.Place := Element.Place;
      Result.Back_Identifier := Element.Back_Identifier;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access For_Loop_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Statement_Identifier :=
        Copy (Cloner, Statement_Identifier (Source.all), Asis.Element (Target));
      Target.Loop_Parameter_Specification :=
        Copy (Cloner, Loop_Parameter_Specification (Source.all), Asis.Element (Target));
      Set_Loop_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Loop_Statements (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Is_Name_Repeated
     (Element : Block_Statement_Node) return Boolean is
   begin
      return Element.Is_Name_Repeated;
   end Is_Name_Repeated;

   procedure Set_Is_Name_Repeated
     (Element : in out Block_Statement_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Name_Repeated := Value;
   end Set_Is_Name_Repeated;

   function Is_Declare_Block
     (Element : Block_Statement_Node) return Boolean is
   begin
      return Element.Is_Declare_Block;
   end Is_Declare_Block;

   procedure Set_Is_Declare_Block
     (Element : in out Block_Statement_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Declare_Block := Value;
   end Set_Is_Declare_Block;

   function Block_Declarative_Items
     (Element : Block_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Declaration_Lists.To_Element_List
        (Element.Block_Declarative_Items, Include_Pragmas);
   end Block_Declarative_Items;

   procedure Set_Block_Declarative_Items
     (Element : in out Block_Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Block_Declarative_Items := Primary_Declaration_Lists.List (Value);
   end Set_Block_Declarative_Items;

   function Block_Declarative_Items_List
     (Element : Block_Statement_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Block_Declarative_Items);
   end Block_Declarative_Items_List;

   function Block_Statements
     (Element : Block_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Statement_Lists.To_Element_List
        (Element.Block_Statements, Include_Pragmas);
   end Block_Statements;

   procedure Set_Block_Statements
     (Element : in out Block_Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Block_Statements := Primary_Statement_Lists.List (Value);
   end Set_Block_Statements;

   function Block_Statements_List
     (Element : Block_Statement_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Block_Statements);
   end Block_Statements_List;

   function Block_Exception_Handlers
     (Element : Block_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Handler_Lists.To_Element_List
        (Element.Block_Exception_Handlers, Include_Pragmas);
   end Block_Exception_Handlers;

   procedure Set_Block_Exception_Handlers
     (Element : in out Block_Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Block_Exception_Handlers := Primary_Handler_Lists.List (Value);
   end Set_Block_Exception_Handlers;

   function Block_Exception_Handlers_List
     (Element : Block_Statement_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Block_Exception_Handlers);
   end Block_Exception_Handlers_List;

   function Statement_Identifier
     (Element : Block_Statement_Node) return Asis.Defining_Name is
   begin
      return Element.Statement_Identifier;
   end Statement_Identifier;

   procedure Set_Statement_Identifier
     (Element : in out Block_Statement_Node;
      Value   : in     Asis.Defining_Name) is
   begin
      Element.Statement_Identifier := Value;
   end Set_Statement_Identifier;

   function Back_Identifier
     (Element : Block_Statement_Node) return Asis.Identifier is
   begin
      return Element.Back_Identifier;
   end Back_Identifier;

   procedure Set_Back_Identifier
     (Element : in out Block_Statement_Node;
      Value   : in     Asis.Identifier) is
   begin
      Element.Back_Identifier := Value;
   end Set_Back_Identifier;

   function Handled_Statements
     (Element : Block_Statement_Node) return Asis.Element is
   begin
      return Element.Handled_Statements;
   end Handled_Statements;

   procedure Set_Handled_Statements
     (Element : in out Block_Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Handled_Statements := Value;
   end Set_Handled_Statements;

   function New_Block_Statement_Node
     (The_Context : ASIS.Context)
      return Block_Statement_Ptr
   is
      Result : Block_Statement_Ptr :=
       new Block_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Block_Statement_Node;
  
   function Statement_Kind (Element : Block_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Block_Statement;
   end;

   function Children (Element : access Block_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Statement_Identifier'Access),
        (True, Asis.Element (Element.Block_Declarative_Items)),
        (True, Asis.Element (Element.Block_Statements)),
        (True, Asis.Element (Element.Block_Exception_Handlers)));
   end Children;

   function Clone
     (Element : Block_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Block_Statement_Ptr := new Block_Statement_Node;
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
      Result.Place := Element.Place;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      Result.Is_Declare_Block := Element.Is_Declare_Block;
      Result.Back_Identifier := Element.Back_Identifier;
      Result.Handled_Statements := Element.Handled_Statements;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Block_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Statement_Identifier :=
        Copy (Cloner, Statement_Identifier (Source.all), Asis.Element (Target));
      Set_Block_Declarative_Items
        (Target.all,
         Primary_Declaration_Lists.Deep_Copy 
           (Block_Declarative_Items (Source.all), Cloner, Asis.Element (Target)));
      Set_Block_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Block_Statements (Source.all), Cloner, Asis.Element (Target)));
      Set_Block_Exception_Handlers
        (Target.all,
         Primary_Handler_Lists.Deep_Copy 
           (Block_Exception_Handlers (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Exit_Loop_Name
     (Element : Exit_Statement_Node) return Asis.Expression is
   begin
      return Element.Exit_Loop_Name;
   end Exit_Loop_Name;

   procedure Set_Exit_Loop_Name
     (Element : in out Exit_Statement_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Exit_Loop_Name := Value;
   end Set_Exit_Loop_Name;

   function Corresponding_Loop_Exited
     (Element : Exit_Statement_Node) return Asis.Statement is
   begin
      return Element.Corresponding_Loop_Exited;
   end Corresponding_Loop_Exited;

   procedure Set_Corresponding_Loop_Exited
     (Element : in out Exit_Statement_Node;
      Value   : in     Asis.Statement) is
   begin
      Element.Corresponding_Loop_Exited := Value;
   end Set_Corresponding_Loop_Exited;

   function Exit_Condition
     (Element : Exit_Statement_Node) return Asis.Expression is
   begin
      return Element.Exit_Condition;
   end Exit_Condition;

   procedure Set_Exit_Condition
     (Element : in out Exit_Statement_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Exit_Condition := Value;
   end Set_Exit_Condition;

   function New_Exit_Statement_Node
     (The_Context : ASIS.Context)
      return Exit_Statement_Ptr
   is
      Result : Exit_Statement_Ptr :=
       new Exit_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Exit_Statement_Node;
  
   function Statement_Kind (Element : Exit_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return An_Exit_Statement;
   end;

   function Children (Element : access Exit_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Exit_Loop_Name'Access),
        (False, Element.Exit_Condition'Access));
   end Children;

   function Clone
     (Element : Exit_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Exit_Statement_Ptr := new Exit_Statement_Node;
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
      Result.Place := Element.Place;
      Result.Corresponding_Loop_Exited := Element.Corresponding_Loop_Exited;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Exit_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Exit_Loop_Name :=
        Copy (Cloner, Exit_Loop_Name (Source.all), Asis.Element (Target));
      Target.Exit_Condition :=
        Copy (Cloner, Exit_Condition (Source.all), Asis.Element (Target));
   end Copy;

   function Goto_Label
     (Element : Goto_Statement_Node) return Asis.Expression is
   begin
      return Element.Goto_Label;
   end Goto_Label;

   procedure Set_Goto_Label
     (Element : in out Goto_Statement_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Goto_Label := Value;
   end Set_Goto_Label;

   function Corresponding_Destination_Statement
     (Element : Goto_Statement_Node) return Asis.Statement is
   begin
      return Element.Corresponding_Destination_Statement;
   end Corresponding_Destination_Statement;

   procedure Set_Corresponding_Destination_Statement
     (Element : in out Goto_Statement_Node;
      Value   : in     Asis.Statement) is
   begin
      Element.Corresponding_Destination_Statement := Value;
   end Set_Corresponding_Destination_Statement;

   function New_Goto_Statement_Node
     (The_Context : ASIS.Context)
      return Goto_Statement_Ptr
   is
      Result : Goto_Statement_Ptr :=
       new Goto_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Goto_Statement_Node;
  
   function Statement_Kind (Element : Goto_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Goto_Statement;
   end;

   function Children (Element : access Goto_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Goto_Label'Access));
   end Children;

   function Clone
     (Element : Goto_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Goto_Statement_Ptr := new Goto_Statement_Node;
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
      Result.Place := Element.Place;
      Result.Corresponding_Destination_Statement := Element.Corresponding_Destination_Statement;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Goto_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Goto_Label :=
        Copy (Cloner, Goto_Label (Source.all), Asis.Element (Target));
   end Copy;

   function Called_Name
     (Element : Base_Call_Statement_Node) return Asis.Expression is
   begin
      return Element.Called_Name;
   end Called_Name;

   procedure Set_Called_Name
     (Element : in out Base_Call_Statement_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Called_Name := Value;
   end Set_Called_Name;

   function Corresponding_Called_Entity
     (Element : Base_Call_Statement_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Called_Entity;
   end Corresponding_Called_Entity;

   procedure Set_Corresponding_Called_Entity
     (Element : in out Base_Call_Statement_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Called_Entity := Value;
   end Set_Corresponding_Called_Entity;

   function Call_Statement_Parameters
     (Element : Base_Call_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Association_Lists.To_Element_List
        (Element.Call_Statement_Parameters, Include_Pragmas);
   end Call_Statement_Parameters;

   procedure Set_Call_Statement_Parameters
     (Element : in out Base_Call_Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Call_Statement_Parameters := Primary_Association_Lists.List (Value);
   end Set_Call_Statement_Parameters;

   function Call_Statement_Parameters_List
     (Element : Base_Call_Statement_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Call_Statement_Parameters);
   end Call_Statement_Parameters_List;

   function Normalized_Call_Statement_Parameters
     (Element : Base_Call_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Secondary_Association_Lists.To_Element_List
        (Element.Normalized_Call_Statement_Parameters, Include_Pragmas);
   end Normalized_Call_Statement_Parameters;

   procedure Add_To_Normalized_Call_Statement_Parameters
     (Element : in out Base_Call_Statement_Node;
      Item    : in     Asis.Element) is
   begin
      Secondary_Association_Lists.Add (Element.Normalized_Call_Statement_Parameters, Item);
   end Add_To_Normalized_Call_Statement_Parameters;

   function Children (Element : access Base_Call_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Called_Name'Access),
        (True, Asis.Element (Element.Call_Statement_Parameters)));
   end Children;

   function Is_Call_On_Dispatching_Operation
     (Element : Procedure_Call_Statement_Node) return Boolean is
   begin
      return Element.Is_Call_On_Dispatching_Operation;
   end Is_Call_On_Dispatching_Operation;

   procedure Set_Is_Call_On_Dispatching_Operation
     (Element : in out Procedure_Call_Statement_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Call_On_Dispatching_Operation := Value;
   end Set_Is_Call_On_Dispatching_Operation;

   function Is_Dispatching_Call
     (Element : Procedure_Call_Statement_Node) return Boolean is
   begin
      return Element.Is_Dispatching_Call;
   end Is_Dispatching_Call;

   procedure Set_Is_Dispatching_Call
     (Element : in out Procedure_Call_Statement_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Dispatching_Call := Value;
   end Set_Is_Dispatching_Call;

   function New_Procedure_Call_Statement_Node
     (The_Context : ASIS.Context)
      return Procedure_Call_Statement_Ptr
   is
      Result : Procedure_Call_Statement_Ptr :=
       new Procedure_Call_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Procedure_Call_Statement_Node;
  
   function Statement_Kind (Element : Procedure_Call_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Procedure_Call_Statement;
   end;

   function Clone
     (Element : Procedure_Call_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Procedure_Call_Statement_Ptr := new Procedure_Call_Statement_Node;
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
      Result.Place := Element.Place;
      Result.Corresponding_Called_Entity := Element.Corresponding_Called_Entity;
      null;
      Result.Is_Call_On_Dispatching_Operation := Element.Is_Call_On_Dispatching_Operation;
      Result.Is_Dispatching_Call := Element.Is_Dispatching_Call;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Procedure_Call_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Called_Name :=
        Copy (Cloner, Called_Name (Source.all), Asis.Element (Target));
      Set_Call_Statement_Parameters
        (Target.all,
         Primary_Association_Lists.Deep_Copy 
           (Call_Statement_Parameters (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Entry_Call_Statement_Node
     (The_Context : ASIS.Context)
      return Entry_Call_Statement_Ptr
   is
      Result : Entry_Call_Statement_Ptr :=
       new Entry_Call_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Entry_Call_Statement_Node;
  
   function Statement_Kind (Element : Entry_Call_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return An_Entry_Call_Statement;
   end;

   function Clone
     (Element : Entry_Call_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Entry_Call_Statement_Ptr := new Entry_Call_Statement_Node;
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
      Result.Place := Element.Place;
      Result.Corresponding_Called_Entity := Element.Corresponding_Called_Entity;
      null;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Entry_Call_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Called_Name :=
        Copy (Cloner, Called_Name (Source.all), Asis.Element (Target));
      Set_Call_Statement_Parameters
        (Target.all,
         Primary_Association_Lists.Deep_Copy 
           (Call_Statement_Parameters (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Return_Expression
     (Element : Simple_Return_Statement_Node) return Asis.Expression is
   begin
      return Element.Return_Expression;
   end Return_Expression;

   procedure Set_Return_Expression
     (Element : in out Simple_Return_Statement_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Return_Expression := Value;
   end Set_Return_Expression;

   function New_Simple_Return_Statement_Node
     (The_Context : ASIS.Context)
      return Simple_Return_Statement_Ptr
   is
      Result : Simple_Return_Statement_Ptr :=
       new Simple_Return_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Simple_Return_Statement_Node;
  
   function Statement_Kind (Element : Simple_Return_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Simple_Return_Statement;
   end;

   function Children (Element : access Simple_Return_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Return_Expression'Access));
   end Children;

   function Clone
     (Element : Simple_Return_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Simple_Return_Statement_Ptr := new Simple_Return_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Simple_Return_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Return_Expression :=
        Copy (Cloner, Return_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function Return_Object_Specification
     (Element : Extended_Return_Statement_Node) return Asis.Declaration is
   begin
      return Element.Return_Object_Specification;
   end Return_Object_Specification;

   procedure Set_Return_Object_Specification
     (Element : in out Extended_Return_Statement_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Return_Object_Specification := Value;
   end Set_Return_Object_Specification;

   function Extended_Return_Statements
     (Element : Extended_Return_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Statement_Lists.To_Element_List
        (Element.Extended_Return_Statements, Include_Pragmas);
   end Extended_Return_Statements;

   procedure Set_Extended_Return_Statements
     (Element : in out Extended_Return_Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Extended_Return_Statements := Primary_Statement_Lists.List (Value);
   end Set_Extended_Return_Statements;

   function Extended_Return_Statements_List
     (Element : Extended_Return_Statement_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Extended_Return_Statements);
   end Extended_Return_Statements_List;

   function Extended_Return_Exception_Handlers
     (Element : Extended_Return_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Handler_Lists.To_Element_List
        (Element.Extended_Return_Exception_Handlers, Include_Pragmas);
   end Extended_Return_Exception_Handlers;

   procedure Set_Extended_Return_Exception_Handlers
     (Element : in out Extended_Return_Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Extended_Return_Exception_Handlers := Primary_Handler_Lists.List (Value);
   end Set_Extended_Return_Exception_Handlers;

   function Extended_Return_Exception_Handlers_List
     (Element : Extended_Return_Statement_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Extended_Return_Exception_Handlers);
   end Extended_Return_Exception_Handlers_List;

   function Handled_Statements
     (Element : Extended_Return_Statement_Node) return Asis.Element is
   begin
      return Element.Handled_Statements;
   end Handled_Statements;

   procedure Set_Handled_Statements
     (Element : in out Extended_Return_Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Handled_Statements := Value;
   end Set_Handled_Statements;

   function New_Extended_Return_Statement_Node
     (The_Context : ASIS.Context)
      return Extended_Return_Statement_Ptr
   is
      Result : Extended_Return_Statement_Ptr :=
       new Extended_Return_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Extended_Return_Statement_Node;
  
   function Statement_Kind (Element : Extended_Return_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return An_Extended_Return_Statement;
   end;

   function Children (Element : access Extended_Return_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Return_Object_Specification'Access),
        (True, Asis.Element (Element.Extended_Return_Statements)),
        (True, Asis.Element (Element.Extended_Return_Exception_Handlers)));
   end Children;

   function Clone
     (Element : Extended_Return_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Extended_Return_Statement_Ptr := new Extended_Return_Statement_Node;
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
      Result.Place := Element.Place;
      Result.Handled_Statements := Element.Handled_Statements;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Extended_Return_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Return_Object_Specification :=
        Copy (Cloner, Return_Object_Specification (Source.all), Asis.Element (Target));
      Set_Extended_Return_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Extended_Return_Statements (Source.all), Cloner, Asis.Element (Target)));
      Set_Extended_Return_Exception_Handlers
        (Target.all,
         Primary_Handler_Lists.Deep_Copy 
           (Extended_Return_Exception_Handlers (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Accept_Entry_Index
     (Element : Accept_Statement_Node) return Asis.Expression is
   begin
      return Element.Accept_Entry_Index;
   end Accept_Entry_Index;

   procedure Set_Accept_Entry_Index
     (Element : in out Accept_Statement_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Accept_Entry_Index := Value;
   end Set_Accept_Entry_Index;

   function Accept_Entry_Direct_Name
     (Element : Accept_Statement_Node) return Asis.Name is
   begin
      return Element.Accept_Entry_Direct_Name;
   end Accept_Entry_Direct_Name;

   procedure Set_Accept_Entry_Direct_Name
     (Element : in out Accept_Statement_Node;
      Value   : in     Asis.Name) is
   begin
      Element.Accept_Entry_Direct_Name := Value;
   end Set_Accept_Entry_Direct_Name;

   function Accept_Parameters
     (Element : Accept_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Parameter_Lists.To_Element_List
        (Element.Accept_Parameters, Include_Pragmas);
   end Accept_Parameters;

   procedure Set_Accept_Parameters
     (Element : in out Accept_Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Accept_Parameters := Primary_Parameter_Lists.List (Value);
   end Set_Accept_Parameters;

   function Accept_Parameters_List
     (Element : Accept_Statement_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Accept_Parameters);
   end Accept_Parameters_List;

   function Accept_Body_Statements
     (Element : Accept_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Statement_Lists.To_Element_List
        (Element.Accept_Body_Statements, Include_Pragmas);
   end Accept_Body_Statements;

   procedure Set_Accept_Body_Statements
     (Element : in out Accept_Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Accept_Body_Statements := Primary_Statement_Lists.List (Value);
   end Set_Accept_Body_Statements;

   function Accept_Body_Statements_List
     (Element : Accept_Statement_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Accept_Body_Statements);
   end Accept_Body_Statements_List;

   function Accept_Body_Exception_Handlers
     (Element : Accept_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Handler_Lists.To_Element_List
        (Element.Accept_Body_Exception_Handlers, Include_Pragmas);
   end Accept_Body_Exception_Handlers;

   procedure Set_Accept_Body_Exception_Handlers
     (Element : in out Accept_Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Accept_Body_Exception_Handlers := Primary_Handler_Lists.List (Value);
   end Set_Accept_Body_Exception_Handlers;

   function Accept_Body_Exception_Handlers_List
     (Element : Accept_Statement_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Accept_Body_Exception_Handlers);
   end Accept_Body_Exception_Handlers_List;

   function Corresponding_Entry
     (Element : Accept_Statement_Node) return Asis.Declaration is
   begin
      return Element.Corresponding_Entry;
   end Corresponding_Entry;

   procedure Set_Corresponding_Entry
     (Element : in out Accept_Statement_Node;
      Value   : in     Asis.Declaration) is
   begin
      Element.Corresponding_Entry := Value;
   end Set_Corresponding_Entry;

   function Is_Name_Repeated
     (Element : Accept_Statement_Node) return Boolean is
   begin
      return Element.Is_Name_Repeated;
   end Is_Name_Repeated;

   procedure Set_Is_Name_Repeated
     (Element : in out Accept_Statement_Node;
      Value   : in     Boolean) is
   begin
      Element.Is_Name_Repeated := Value;
   end Set_Is_Name_Repeated;

   function Handled_Statements
     (Element : Accept_Statement_Node) return Asis.Element is
   begin
      return Element.Handled_Statements;
   end Handled_Statements;

   procedure Set_Handled_Statements
     (Element : in out Accept_Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Handled_Statements := Value;
   end Set_Handled_Statements;

   function New_Accept_Statement_Node
     (The_Context : ASIS.Context)
      return Accept_Statement_Ptr
   is
      Result : Accept_Statement_Ptr :=
       new Accept_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Accept_Statement_Node;
  
   function Statement_Kind (Element : Accept_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return An_Accept_Statement;
   end;

   function Children (Element : access Accept_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Accept_Entry_Direct_Name'Access),
        (False, Element.Accept_Entry_Index'Access),
        (True, Asis.Element (Element.Accept_Parameters)),
        (True, Asis.Element (Element.Accept_Body_Statements)),
        (True, Asis.Element (Element.Accept_Body_Exception_Handlers)));
   end Children;

   function Clone
     (Element : Accept_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Accept_Statement_Ptr := new Accept_Statement_Node;
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
      Result.Place := Element.Place;
      Result.Corresponding_Entry := Element.Corresponding_Entry;
      Result.Is_Name_Repeated := Element.Is_Name_Repeated;
      Result.Handled_Statements := Element.Handled_Statements;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Accept_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Accept_Entry_Direct_Name :=
        Copy (Cloner, Accept_Entry_Direct_Name (Source.all), Asis.Element (Target));
      Target.Accept_Entry_Index :=
        Copy (Cloner, Accept_Entry_Index (Source.all), Asis.Element (Target));
      Set_Accept_Parameters
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Accept_Parameters (Source.all), Cloner, Asis.Element (Target)));
      Set_Accept_Body_Statements
        (Target.all,
         Primary_Statement_Lists.Deep_Copy 
           (Accept_Body_Statements (Source.all), Cloner, Asis.Element (Target)));
      Set_Accept_Body_Exception_Handlers
        (Target.all,
         Primary_Handler_Lists.Deep_Copy 
           (Accept_Body_Exception_Handlers (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Requeue_Entry_Name
     (Element : Requeue_Statement_Node) return Asis.Name is
   begin
      return Element.Requeue_Entry_Name;
   end Requeue_Entry_Name;

   procedure Set_Requeue_Entry_Name
     (Element : in out Requeue_Statement_Node;
      Value   : in     Asis.Name) is
   begin
      Element.Requeue_Entry_Name := Value;
   end Set_Requeue_Entry_Name;

   function New_Requeue_Statement_Node
     (The_Context : ASIS.Context)
      return Requeue_Statement_Ptr
   is
      Result : Requeue_Statement_Ptr :=
       new Requeue_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Requeue_Statement_Node;
  
   function Statement_Kind (Element : Requeue_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Requeue_Statement;
   end;

   function Children (Element : access Requeue_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Requeue_Entry_Name'Access));
   end Children;

   function Clone
     (Element : Requeue_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Requeue_Statement_Ptr := new Requeue_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Requeue_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Requeue_Entry_Name :=
        Copy (Cloner, Requeue_Entry_Name (Source.all), Asis.Element (Target));
   end Copy;

   function New_Requeue_Statement_With_Abort_Node
     (The_Context : ASIS.Context)
      return Requeue_Statement_With_Abort_Ptr
   is
      Result : Requeue_Statement_With_Abort_Ptr :=
       new Requeue_Statement_With_Abort_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Requeue_Statement_With_Abort_Node;
  
   function Statement_Kind (Element : Requeue_Statement_With_Abort_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Requeue_Statement_With_Abort;
   end;

   function Clone
     (Element : Requeue_Statement_With_Abort_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Requeue_Statement_With_Abort_Ptr := new Requeue_Statement_With_Abort_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Requeue_Statement_With_Abort_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Requeue_Entry_Name :=
        Copy (Cloner, Requeue_Entry_Name (Source.all), Asis.Element (Target));
   end Copy;

   function Delay_Expression
     (Element : Delay_Until_Statement_Node) return Asis.Expression is
   begin
      return Element.Delay_Expression;
   end Delay_Expression;

   procedure Set_Delay_Expression
     (Element : in out Delay_Until_Statement_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Delay_Expression := Value;
   end Set_Delay_Expression;

   function New_Delay_Until_Statement_Node
     (The_Context : ASIS.Context)
      return Delay_Until_Statement_Ptr
   is
      Result : Delay_Until_Statement_Ptr :=
       new Delay_Until_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Delay_Until_Statement_Node;
  
   function Statement_Kind (Element : Delay_Until_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Delay_Until_Statement;
   end;

   function Children (Element : access Delay_Until_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Delay_Expression'Access));
   end Children;

   function Clone
     (Element : Delay_Until_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Delay_Until_Statement_Ptr := new Delay_Until_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Delay_Until_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Delay_Expression :=
        Copy (Cloner, Delay_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function New_Delay_Relative_Statement_Node
     (The_Context : ASIS.Context)
      return Delay_Relative_Statement_Ptr
   is
      Result : Delay_Relative_Statement_Ptr :=
       new Delay_Relative_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Delay_Relative_Statement_Node;
  
   function Statement_Kind (Element : Delay_Relative_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Delay_Relative_Statement;
   end;

   function Clone
     (Element : Delay_Relative_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Delay_Relative_Statement_Ptr := new Delay_Relative_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Delay_Relative_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Delay_Expression :=
        Copy (Cloner, Delay_Expression (Source.all), Asis.Element (Target));
   end Copy;

   function New_Terminate_Alternative_Statement_Node
     (The_Context : ASIS.Context)
      return Terminate_Alternative_Statement_Ptr
   is
      Result : Terminate_Alternative_Statement_Ptr :=
       new Terminate_Alternative_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Terminate_Alternative_Statement_Node;
  
   function Statement_Kind (Element : Terminate_Alternative_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Terminate_Alternative_Statement;
   end;

   function Clone
     (Element : Terminate_Alternative_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Terminate_Alternative_Statement_Ptr := new Terminate_Alternative_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Terminate_Alternative_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Aborted_Tasks
     (Element : Abort_Statement_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Expression_Lists.To_Element_List
        (Element.Aborted_Tasks, Include_Pragmas);
   end Aborted_Tasks;

   procedure Set_Aborted_Tasks
     (Element : in out Abort_Statement_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Aborted_Tasks := Primary_Expression_Lists.List (Value);
   end Set_Aborted_Tasks;

   function Aborted_Tasks_List
     (Element : Abort_Statement_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Aborted_Tasks);
   end Aborted_Tasks_List;

   function New_Abort_Statement_Node
     (The_Context : ASIS.Context)
      return Abort_Statement_Ptr
   is
      Result : Abort_Statement_Ptr :=
       new Abort_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Abort_Statement_Node;
  
   function Statement_Kind (Element : Abort_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return An_Abort_Statement;
   end;

   function Children (Element : access Abort_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (True, Asis.Element (Element.Aborted_Tasks)));
   end Children;

   function Clone
     (Element : Abort_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Abort_Statement_Ptr := new Abort_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Abort_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Set_Aborted_Tasks
        (Target.all,
         Primary_Expression_Lists.Deep_Copy 
           (Aborted_Tasks (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Raised_Exception
     (Element : Raise_Statement_Node) return Asis.Expression is
   begin
      return Element.Raised_Exception;
   end Raised_Exception;

   procedure Set_Raised_Exception
     (Element : in out Raise_Statement_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Raised_Exception := Value;
   end Set_Raised_Exception;

   function Raise_Statement_Message
     (Element : Raise_Statement_Node) return Asis.Expression is
   begin
      return Element.Raise_Statement_Message;
   end Raise_Statement_Message;

   procedure Set_Raise_Statement_Message
     (Element : in out Raise_Statement_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Raise_Statement_Message := Value;
   end Set_Raise_Statement_Message;

   function New_Raise_Statement_Node
     (The_Context : ASIS.Context)
      return Raise_Statement_Ptr
   is
      Result : Raise_Statement_Ptr :=
       new Raise_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Raise_Statement_Node;
  
   function Statement_Kind (Element : Raise_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Raise_Statement;
   end;

   function Children (Element : access Raise_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Raised_Exception'Access),
        (False, Element.Raise_Statement_Message'Access));
   end Children;

   function Clone
     (Element : Raise_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Raise_Statement_Ptr := new Raise_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Raise_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Raised_Exception :=
        Copy (Cloner, Raised_Exception (Source.all), Asis.Element (Target));
      Target.Raise_Statement_Message :=
        Copy (Cloner, Raise_Statement_Message (Source.all), Asis.Element (Target));
   end Copy;

   function Qualified_Expression
     (Element : Code_Statement_Node) return Asis.Expression is
   begin
      return Element.Qualified_Expression;
   end Qualified_Expression;

   procedure Set_Qualified_Expression
     (Element : in out Code_Statement_Node;
      Value   : in     Asis.Expression) is
   begin
      Element.Qualified_Expression := Value;
   end Set_Qualified_Expression;

   function New_Code_Statement_Node
     (The_Context : ASIS.Context)
      return Code_Statement_Ptr
   is
      Result : Code_Statement_Ptr :=
       new Code_Statement_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Code_Statement_Node;
  
   function Statement_Kind (Element : Code_Statement_Node)
      return Asis.Statement_Kinds is
   begin
      return A_Code_Statement;
   end;

   function Children (Element : access Code_Statement_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Label_Names)),
        (False, Element.Qualified_Expression'Access));
   end Children;

   function Clone
     (Element : Code_Statement_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Code_Statement_Ptr := new Code_Statement_Node;
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
      Result.Place := Element.Place;
      return Asis.Element (Result);
   end Clone;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Code_Statement_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Label_Names
        (Target.all,
         Primary_Defining_Name_Lists.Deep_Copy 
           (Label_Names (Source.all), Cloner, Asis.Element (Target)));
      Target.Qualified_Expression :=
        Copy (Cloner, Qualified_Expression (Source.all), Asis.Element (Target));
   end Copy;

end Asis.Gela.Elements.Stmt;
