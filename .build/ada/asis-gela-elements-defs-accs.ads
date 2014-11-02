
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
  
package Asis.Gela.Elements.Defs.Accs is

   ---------------------------------------
   -- Anonymous_Access_To_Variable_Node --
   ---------------------------------------

   type Anonymous_Access_To_Variable_Node is 
      new Access_Definition_Node with private;

   type Anonymous_Access_To_Variable_Ptr is
      access all Anonymous_Access_To_Variable_Node;
   for Anonymous_Access_To_Variable_Ptr'Storage_Pool use Lists.Pool;

   function New_Anonymous_Access_To_Variable_Node
     (The_Context : ASIS.Context)
      return Anonymous_Access_To_Variable_Ptr;

   function Anonymous_Access_To_Object_Subtype_Mark
     (Element : Anonymous_Access_To_Variable_Node) return Asis.Name;

   procedure Set_Anonymous_Access_To_Object_Subtype_Mark
     (Element : in out Anonymous_Access_To_Variable_Node;
      Value   : in     Asis.Name);

   function Access_Definition_Kind (Element : Anonymous_Access_To_Variable_Node)
      return Asis.Access_Definition_Kinds;

   function Children (Element : access Anonymous_Access_To_Variable_Node)
     return Traverse_List;

   function Clone
     (Element : Anonymous_Access_To_Variable_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Anonymous_Access_To_Variable_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------------------
   -- Anonymous_Access_To_Constant_Node --
   ---------------------------------------

   type Anonymous_Access_To_Constant_Node is 
      new Anonymous_Access_To_Variable_Node with private;

   type Anonymous_Access_To_Constant_Ptr is
      access all Anonymous_Access_To_Constant_Node;
   for Anonymous_Access_To_Constant_Ptr'Storage_Pool use Lists.Pool;

   function New_Anonymous_Access_To_Constant_Node
     (The_Context : ASIS.Context)
      return Anonymous_Access_To_Constant_Ptr;

   function Access_Definition_Kind (Element : Anonymous_Access_To_Constant_Node)
      return Asis.Access_Definition_Kinds;

   function Clone
     (Element : Anonymous_Access_To_Constant_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Anonymous_Access_To_Constant_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ----------------------------------------
   -- Anonymous_Access_To_Procedure_Node --
   ----------------------------------------

   type Anonymous_Access_To_Procedure_Node is 
      new Access_Definition_Node with private;

   type Anonymous_Access_To_Procedure_Ptr is
      access all Anonymous_Access_To_Procedure_Node;
   for Anonymous_Access_To_Procedure_Ptr'Storage_Pool use Lists.Pool;

   function New_Anonymous_Access_To_Procedure_Node
     (The_Context : ASIS.Context)
      return Anonymous_Access_To_Procedure_Ptr;

   function Access_To_Subprogram_Parameter_Profile
     (Element : Anonymous_Access_To_Procedure_Node;
      Include_Pragmas : in Boolean := False) return Asis.Element_List;

   procedure Set_Access_To_Subprogram_Parameter_Profile
     (Element : in out Anonymous_Access_To_Procedure_Node;
      Value   : in     Asis.Element);

   function Access_To_Subprogram_Parameter_Profile_List
     (Element : Anonymous_Access_To_Procedure_Node) return Asis.Element;

   function Access_Definition_Kind (Element : Anonymous_Access_To_Procedure_Node)
      return Asis.Access_Definition_Kinds;

   function Children (Element : access Anonymous_Access_To_Procedure_Node)
     return Traverse_List;

   function Clone
     (Element : Anonymous_Access_To_Procedure_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Anonymous_Access_To_Procedure_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   --------------------------------------------------
   -- Anonymous_Access_To_Protected_Procedure_Node --
   --------------------------------------------------

   type Anonymous_Access_To_Protected_Procedure_Node is 
      new Anonymous_Access_To_Procedure_Node with private;

   type Anonymous_Access_To_Protected_Procedure_Ptr is
      access all Anonymous_Access_To_Protected_Procedure_Node;
   for Anonymous_Access_To_Protected_Procedure_Ptr'Storage_Pool use Lists.Pool;

   function New_Anonymous_Access_To_Protected_Procedure_Node
     (The_Context : ASIS.Context)
      return Anonymous_Access_To_Protected_Procedure_Ptr;

   function Access_Definition_Kind (Element : Anonymous_Access_To_Protected_Procedure_Node)
      return Asis.Access_Definition_Kinds;

   function Clone
     (Element : Anonymous_Access_To_Protected_Procedure_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Anonymous_Access_To_Protected_Procedure_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   ---------------------------------------
   -- Anonymous_Access_To_Function_Node --
   ---------------------------------------

   type Anonymous_Access_To_Function_Node is 
      new Anonymous_Access_To_Procedure_Node with private;

   type Anonymous_Access_To_Function_Ptr is
      access all Anonymous_Access_To_Function_Node;
   for Anonymous_Access_To_Function_Ptr'Storage_Pool use Lists.Pool;

   function New_Anonymous_Access_To_Function_Node
     (The_Context : ASIS.Context)
      return Anonymous_Access_To_Function_Ptr;

   function Access_To_Function_Result_Subtype
     (Element : Anonymous_Access_To_Function_Node) return Asis.Definition;

   procedure Set_Access_To_Function_Result_Subtype
     (Element : in out Anonymous_Access_To_Function_Node;
      Value   : in     Asis.Definition);

   function Access_Definition_Kind (Element : Anonymous_Access_To_Function_Node)
      return Asis.Access_Definition_Kinds;

   function Children (Element : access Anonymous_Access_To_Function_Node)
     return Traverse_List;

   function Clone
     (Element : Anonymous_Access_To_Function_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Anonymous_Access_To_Function_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

   -------------------------------------------------
   -- Anonymous_Access_To_Protected_Function_Node --
   -------------------------------------------------

   type Anonymous_Access_To_Protected_Function_Node is 
      new Anonymous_Access_To_Function_Node with private;

   type Anonymous_Access_To_Protected_Function_Ptr is
      access all Anonymous_Access_To_Protected_Function_Node;
   for Anonymous_Access_To_Protected_Function_Ptr'Storage_Pool use Lists.Pool;

   function New_Anonymous_Access_To_Protected_Function_Node
     (The_Context : ASIS.Context)
      return Anonymous_Access_To_Protected_Function_Ptr;

   function Access_Definition_Kind (Element : Anonymous_Access_To_Protected_Function_Node)
      return Asis.Access_Definition_Kinds;

   function Clone
     (Element : Anonymous_Access_To_Protected_Function_Node;
      Parent  : Asis.Element)
     return Asis.Element;

   procedure Copy
     (Source : in     Asis.Element;
      Target : access Anonymous_Access_To_Protected_Function_Node;
      Cloner : in     Cloner_Class;
      Parent : in     Asis.Element);

private


   type Anonymous_Access_To_Variable_Node is 
      new Access_Definition_Node with
      record
         Anonymous_Access_To_Object_Subtype_Mark : aliased Asis.Name;
      end record;


   type Anonymous_Access_To_Constant_Node is 
      new Anonymous_Access_To_Variable_Node with
      record
         null;
      end record;


   type Anonymous_Access_To_Procedure_Node is 
      new Access_Definition_Node with
      record
         Access_To_Subprogram_Parameter_Profile : aliased Primary_Parameter_Lists.List;
      end record;


   type Anonymous_Access_To_Protected_Procedure_Node is 
      new Anonymous_Access_To_Procedure_Node with
      record
         null;
      end record;


   type Anonymous_Access_To_Function_Node is 
      new Anonymous_Access_To_Procedure_Node with
      record
         Access_To_Function_Result_Subtype : aliased Asis.Definition;
      end record;


   type Anonymous_Access_To_Protected_Function_Node is 
      new Anonymous_Access_To_Function_Node with
      record
         null;
      end record;

end Asis.Gela.Elements.Defs.Accs;
