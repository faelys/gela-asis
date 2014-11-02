
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

package body Asis.Gela.Elements.Defs.Accs is

   function Anonymous_Access_To_Object_Subtype_Mark
     (Element : Anonymous_Access_To_Variable_Node) return Asis.Name is
   begin
      return Element.Anonymous_Access_To_Object_Subtype_Mark;
   end Anonymous_Access_To_Object_Subtype_Mark;

   procedure Set_Anonymous_Access_To_Object_Subtype_Mark
     (Element : in out Anonymous_Access_To_Variable_Node;
      Value   : in     Asis.Name) is
   begin
      Element.Anonymous_Access_To_Object_Subtype_Mark := Value;
   end Set_Anonymous_Access_To_Object_Subtype_Mark;

   function New_Anonymous_Access_To_Variable_Node
     (The_Context : ASIS.Context)
      return Anonymous_Access_To_Variable_Ptr
   is
      Result : Anonymous_Access_To_Variable_Ptr :=
       new Anonymous_Access_To_Variable_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Anonymous_Access_To_Variable_Node;
  
   function Access_Definition_Kind (Element : Anonymous_Access_To_Variable_Node)
      return Asis.Access_Definition_Kinds is
   begin
      return An_Anonymous_Access_To_Variable;
   end;

   function Children (Element : access Anonymous_Access_To_Variable_Node)
     return Traverse_List is
   begin
      return (1 => (False, Element.Anonymous_Access_To_Object_Subtype_Mark'Access));
   end Children;

   function Clone
     (Element : Anonymous_Access_To_Variable_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Anonymous_Access_To_Variable_Ptr := new Anonymous_Access_To_Variable_Node;
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
      Target : access Anonymous_Access_To_Variable_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Anonymous_Access_To_Object_Subtype_Mark :=
        Copy (Cloner, Anonymous_Access_To_Object_Subtype_Mark (Source.all), Asis.Element (Target));
   end Copy;

   function New_Anonymous_Access_To_Constant_Node
     (The_Context : ASIS.Context)
      return Anonymous_Access_To_Constant_Ptr
   is
      Result : Anonymous_Access_To_Constant_Ptr :=
       new Anonymous_Access_To_Constant_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Anonymous_Access_To_Constant_Node;
  
   function Access_Definition_Kind (Element : Anonymous_Access_To_Constant_Node)
      return Asis.Access_Definition_Kinds is
   begin
      return An_Anonymous_Access_To_Constant;
   end;

   function Clone
     (Element : Anonymous_Access_To_Constant_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Anonymous_Access_To_Constant_Ptr := new Anonymous_Access_To_Constant_Node;
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
      Target : access Anonymous_Access_To_Constant_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Target.Anonymous_Access_To_Object_Subtype_Mark :=
        Copy (Cloner, Anonymous_Access_To_Object_Subtype_Mark (Source.all), Asis.Element (Target));
   end Copy;

   function Access_To_Subprogram_Parameter_Profile
     (Element : Anonymous_Access_To_Procedure_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List is
   begin
      return Primary_Parameter_Lists.To_Element_List
        (Element.Access_To_Subprogram_Parameter_Profile, Include_Pragmas);
   end Access_To_Subprogram_Parameter_Profile;

   procedure Set_Access_To_Subprogram_Parameter_Profile
     (Element : in out Anonymous_Access_To_Procedure_Node;
      Value   : in     Asis.Element) is
   begin
      Element.Access_To_Subprogram_Parameter_Profile := Primary_Parameter_Lists.List (Value);
   end Set_Access_To_Subprogram_Parameter_Profile;

   function Access_To_Subprogram_Parameter_Profile_List
     (Element : Anonymous_Access_To_Procedure_Node) return Asis.Element is
   begin
      return Asis.Element (Element.Access_To_Subprogram_Parameter_Profile);
   end Access_To_Subprogram_Parameter_Profile_List;

   function New_Anonymous_Access_To_Procedure_Node
     (The_Context : ASIS.Context)
      return Anonymous_Access_To_Procedure_Ptr
   is
      Result : Anonymous_Access_To_Procedure_Ptr :=
       new Anonymous_Access_To_Procedure_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Anonymous_Access_To_Procedure_Node;
  
   function Access_Definition_Kind (Element : Anonymous_Access_To_Procedure_Node)
      return Asis.Access_Definition_Kinds is
   begin
      return An_Anonymous_Access_To_Procedure;
   end;

   function Children (Element : access Anonymous_Access_To_Procedure_Node)
     return Traverse_List is
   begin
      return (1 => (True, Asis.Element (Element.Access_To_Subprogram_Parameter_Profile)));
   end Children;

   function Clone
     (Element : Anonymous_Access_To_Procedure_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Anonymous_Access_To_Procedure_Ptr := new Anonymous_Access_To_Procedure_Node;
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
      Target : access Anonymous_Access_To_Procedure_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Access_To_Subprogram_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Access_To_Subprogram_Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function New_Anonymous_Access_To_Protected_Procedure_Node
     (The_Context : ASIS.Context)
      return Anonymous_Access_To_Protected_Procedure_Ptr
   is
      Result : Anonymous_Access_To_Protected_Procedure_Ptr :=
       new Anonymous_Access_To_Protected_Procedure_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Anonymous_Access_To_Protected_Procedure_Node;
  
   function Access_Definition_Kind (Element : Anonymous_Access_To_Protected_Procedure_Node)
      return Asis.Access_Definition_Kinds is
   begin
      return An_Anonymous_Access_To_Protected_Procedure;
   end;

   function Clone
     (Element : Anonymous_Access_To_Protected_Procedure_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Anonymous_Access_To_Protected_Procedure_Ptr := new Anonymous_Access_To_Protected_Procedure_Node;
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
      Target : access Anonymous_Access_To_Protected_Procedure_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Access_To_Subprogram_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Access_To_Subprogram_Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
   end Copy;

   function Access_To_Function_Result_Subtype
     (Element : Anonymous_Access_To_Function_Node) return Asis.Definition is
   begin
      return Element.Access_To_Function_Result_Subtype;
   end Access_To_Function_Result_Subtype;

   procedure Set_Access_To_Function_Result_Subtype
     (Element : in out Anonymous_Access_To_Function_Node;
      Value   : in     Asis.Definition) is
   begin
      Element.Access_To_Function_Result_Subtype := Value;
   end Set_Access_To_Function_Result_Subtype;

   function New_Anonymous_Access_To_Function_Node
     (The_Context : ASIS.Context)
      return Anonymous_Access_To_Function_Ptr
   is
      Result : Anonymous_Access_To_Function_Ptr :=
       new Anonymous_Access_To_Function_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Anonymous_Access_To_Function_Node;
  
   function Access_Definition_Kind (Element : Anonymous_Access_To_Function_Node)
      return Asis.Access_Definition_Kinds is
   begin
      return An_Anonymous_Access_To_Function;
   end;

   function Children (Element : access Anonymous_Access_To_Function_Node)
     return Traverse_List is
   begin
      return ((True, Asis.Element (Element.Access_To_Subprogram_Parameter_Profile)),
        (False, Element.Access_To_Function_Result_Subtype'Access));
   end Children;

   function Clone
     (Element : Anonymous_Access_To_Function_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Anonymous_Access_To_Function_Ptr := new Anonymous_Access_To_Function_Node;
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
      Target : access Anonymous_Access_To_Function_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Access_To_Subprogram_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Access_To_Subprogram_Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
      Target.Access_To_Function_Result_Subtype :=
        Copy (Cloner, Access_To_Function_Result_Subtype (Source.all), Asis.Element (Target));
   end Copy;

   function New_Anonymous_Access_To_Protected_Function_Node
     (The_Context : ASIS.Context)
      return Anonymous_Access_To_Protected_Function_Ptr
   is
      Result : Anonymous_Access_To_Protected_Function_Ptr :=
       new Anonymous_Access_To_Protected_Function_Node;
   begin

      Set_Enclosing_Compilation_Unit
        (Result.all, Current_Unit (The_Context.all));

      return Result;
   end New_Anonymous_Access_To_Protected_Function_Node;
  
   function Access_Definition_Kind (Element : Anonymous_Access_To_Protected_Function_Node)
      return Asis.Access_Definition_Kinds is
   begin
      return An_Anonymous_Access_To_Protected_Function;
   end;

   function Clone
     (Element : Anonymous_Access_To_Protected_Function_Node;
      Parent  : Asis.Element)
     return Asis.Element
   is
      Result : constant Anonymous_Access_To_Protected_Function_Ptr := new Anonymous_Access_To_Protected_Function_Node;
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
      Target : access Anonymous_Access_To_Protected_Function_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element)
   is
   begin
      Set_Access_To_Subprogram_Parameter_Profile
        (Target.all,
         Primary_Parameter_Lists.Deep_Copy 
           (Access_To_Subprogram_Parameter_Profile (Source.all), Cloner, Asis.Element (Target)));
      Target.Access_To_Function_Result_Subtype :=
        Copy (Cloner, Access_To_Function_Result_Subtype (Source.all), Asis.Element (Target));
   end Copy;

end Asis.Gela.Elements.Defs.Accs;
