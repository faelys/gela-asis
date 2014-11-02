
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
  
with Asis.Gela.Lists;                  use Asis.Gela.Lists;
with Asis.Gela.Compilations;
  
package Asis.Gela.Units is

   -------------------------------
   -- Any_Compilation_Unit_Node --
   -------------------------------

   type Any_Compilation_Unit_Node is 
      new Compilation_Unit_Node with private;

   type Any_Compilation_Unit_Ptr is
      access all Any_Compilation_Unit_Node;
   for Any_Compilation_Unit_Ptr'Storage_Pool use Lists.Pool;

   function New_Any_Compilation_Unit_Node
     (The_Context : ASIS.Context)
      return Any_Compilation_Unit_Ptr;

   function Unit_Kind
     (Element : Any_Compilation_Unit_Node) return Asis.Unit_Kinds;

   procedure Set_Unit_Kind
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Unit_Kinds);

   function Unit_Class
     (Element : Any_Compilation_Unit_Node) return Asis.Unit_Classes;

   procedure Set_Unit_Class
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Unit_Classes);

   function Unit_Origin
     (Element : Any_Compilation_Unit_Node) return Asis.Unit_Origins;

   procedure Set_Unit_Origin
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Unit_Origins);

   function Enclosing_Context
     (Element : Any_Compilation_Unit_Node) return Asis.Context;

   procedure Set_Enclosing_Context
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Context);

   function Next_Element
     (Element : Any_Compilation_Unit_Node) return Asis.Element;

   procedure Set_Next_Element
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Element);

   function Corresponding_Parent_Declaration
     (Element : Any_Compilation_Unit_Node) return Asis.Compilation_Unit;

   procedure Set_Corresponding_Parent_Declaration
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Compilation_Unit);

   function Corresponding_Children
     (Element : Any_Compilation_Unit_Node) return Asis.Compilation_Unit_List;

   procedure Add_To_Corresponding_Children
     (Element : in out Any_Compilation_Unit_Node;
      Item    : in     Asis.Compilation_Unit);

   function Corresponding_Declaration
     (Element : Any_Compilation_Unit_Node) return Asis.Compilation_Unit;

   procedure Set_Corresponding_Declaration
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Compilation_Unit);

   function Corresponding_Body
     (Element : Any_Compilation_Unit_Node) return Asis.Compilation_Unit;

   procedure Set_Corresponding_Body
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Compilation_Unit);

   function Unit_Full_Name
     (Element : Any_Compilation_Unit_Node) return Wide_String;

   procedure Set_Unit_Full_Name
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String);

   function Unique_Name
     (Element : Any_Compilation_Unit_Node) return Wide_String;

   procedure Set_Unique_Name
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String);

   function Can_Be_Main_Program
     (Element : Any_Compilation_Unit_Node) return Boolean;

   procedure Set_Can_Be_Main_Program
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Boolean);

   function Is_Body_Required
     (Element : Any_Compilation_Unit_Node) return Boolean;

   procedure Set_Is_Body_Required
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Boolean);

   function Text_Name
     (Element : Any_Compilation_Unit_Node) return Wide_String;

   procedure Set_Text_Name
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String);

   function Text_Form
     (Element : Any_Compilation_Unit_Node) return Wide_String;

   procedure Set_Text_Form
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String);

   function Object_Name
     (Element : Any_Compilation_Unit_Node) return Wide_String;

   procedure Set_Object_Name
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String);

   function Object_Form
     (Element : Any_Compilation_Unit_Node) return Wide_String;

   procedure Set_Object_Form
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String);

   function Compilation_Command_Line_Options
     (Element : Any_Compilation_Unit_Node) return Wide_String;

   procedure Set_Compilation_Command_Line_Options
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String);

   function Corresponding_Subunit_Parent_Body
     (Element : Any_Compilation_Unit_Node) return Asis.Compilation_Unit;

   procedure Set_Corresponding_Subunit_Parent_Body
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Compilation_Unit);

   function Subunits
     (Element : Any_Compilation_Unit_Node) return Asis.Compilation_Unit_List;

   procedure Add_To_Subunits
     (Element : in out Any_Compilation_Unit_Node;
      Item    : in     Asis.Compilation_Unit);

   function Unit_Declaration
     (Element : Any_Compilation_Unit_Node) return Asis.Element;

   procedure Set_Unit_Declaration
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Element);

   function Context_Clause_Elements
     (Element : Any_Compilation_Unit_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Context_Clause_Elements
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Element);

   function Context_Clause_Elements_List
     (Element : Any_Compilation_Unit_Node) return Asis.Element;

   function Compilation_Pragmas
     (Element : Any_Compilation_Unit_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Add_To_Compilation_Pragmas
     (Element : in out Any_Compilation_Unit_Node;
      Item    : in     Asis.Element);

   function Start_Position
     (Element : Any_Compilation_Unit_Node) return Asis.Text_Position;

   procedure Set_Start_Position
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Text_Position);

   function End_Position
     (Element : Any_Compilation_Unit_Node) return Asis.Text_Position;

   procedure Set_End_Position
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Text_Position);

   function Separate_Name
     (Element : Any_Compilation_Unit_Node) return Asis.Element;

   procedure Set_Separate_Name
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.Element);

   function Separate_Name_Image
     (Element : Any_Compilation_Unit_Node) return Wide_String;

   procedure Set_Separate_Name_Image
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Wide_String);

   function Hash
     (Element : Any_Compilation_Unit_Node) return Asis.ASIS_Integer;

   procedure Set_Hash
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Asis.ASIS_Integer);

   function Compilation
     (Element : Any_Compilation_Unit_Node) return Compilations.Compilation;

   procedure Set_Compilation
     (Element : in out Any_Compilation_Unit_Node;
      Value   : in     Compilations.Compilation);

   function Clone
     (Element : Any_Compilation_Unit_Node;
      Parent  : Asis.Element)
     return Asis.Element;

private


   type Any_Compilation_Unit_Node is 
      new Compilation_Unit_Node with
      record
         Unit_Kind                      : aliased Asis.Unit_Kinds := Not_A_Unit;
         Unit_Class                     : aliased Asis.Unit_Classes;
         Unit_Origin                    : aliased Asis.Unit_Origins;
         Enclosing_Context              : aliased Asis.Context;
         Next_Element                   : aliased Asis.Element;
         Corresponding_Parent_Declaration : aliased Asis.Compilation_Unit;
         Corresponding_Children         : aliased Secondary_Unit_Lists.List_Node;
         Corresponding_Declaration      : aliased Asis.Compilation_Unit;
         Corresponding_Body             : aliased Asis.Compilation_Unit;
         Unit_Full_Name                 : aliased Unbounded_Wide_String;
         Unique_Name                    : aliased Unbounded_Wide_String;
         Can_Be_Main_Program            : aliased Boolean := False;
         Is_Body_Required               : aliased Boolean := False;
         Text_Name                      : aliased Unbounded_Wide_String;
         Text_Form                      : aliased Unbounded_Wide_String;
         Object_Name                    : aliased Unbounded_Wide_String;
         Object_Form                    : aliased Unbounded_Wide_String;
         Compilation_Command_Line_Options : aliased Unbounded_Wide_String;
         Corresponding_Subunit_Parent_Body : aliased Asis.Compilation_Unit;
         Subunits                       : aliased Secondary_Unit_Lists.List_Node;
         Unit_Declaration               : aliased Asis.Element;
         Context_Clause_Elements        : aliased Primary_Clause_Lists.List;
         Compilation_Pragmas            : aliased Secondary_Pragma_Lists.List_Node;
         Start_Position                 : aliased Asis.Text_Position;
         End_Position                   : aliased Asis.Text_Position;
         Separate_Name                  : aliased Asis.Element;
         Separate_Name_Image            : aliased Unbounded_Wide_String;
         Hash                           : aliased Asis.ASIS_Integer
           := Next_Hash;
         Compilation                    : aliased Compilations.Compilation;
      end record;

end Asis.Gela.Units;
