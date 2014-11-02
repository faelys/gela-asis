------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $
--  Purpose:
--  Helper procedures to implement compilation unit abstraction

with Asis.Gela.Compilations;

package Asis.Gela.Unit_Utils is

   procedure Set_Unit_Kind
     (Unit : Compilation_Unit);

   procedure Set_Unit_Class
     (Unit       : in Compilation_Unit;
      Is_Private : in Boolean);

   procedure Set_Unit_Origin
     (Unit : Compilation_Unit);

   procedure Set_Unit_Full_Name
     (Unit : Compilation_Unit);

   procedure Set_Unique_Name
     (Unit : Compilation_Unit);

   procedure Set_Can_Be_Main_Program
     (Unit : Compilation_Unit);

   procedure Set_Is_Body_Required
     (Unit : Compilation_Unit);

   procedure Set_Text_Name
     (Unit : Compilation_Unit);

   procedure Set_Text_Form
     (Unit : Compilation_Unit);

   procedure Set_Object_Name
     (Unit : Compilation_Unit);

   procedure Set_Object_Form
     (Unit : Compilation_Unit);

   procedure Set_Compilation_Command_Line_Options
     (Unit : Compilation_Unit);

   procedure Set_Separate_Name
     (Unit : Compilation_Unit);

   function Is_Program_Unit_Pragma
     (Kind : Pragma_Kinds) return Boolean;

   function Is_Configuration_Pragma
     (Kind : Pragma_Kinds) return Boolean;

   function Is_Compilation_Unit_Body
     (The_Unit : Asis.Compilation_Unit) return Boolean;

   procedure Add_Pragma
     (The_Unit   : Asis.Element;
      The_Pragma : Asis.Element;
      To_Clause  : Boolean := False);

   procedure Add_Subunit
     (The_Unit    : Compilation_Unit;
      The_Subunit : Compilation_Unit);

   procedure Add_Child
     (The_Unit    : Compilation_Unit;
      The_Child   : Compilation_Unit);

   procedure Set_Body
     (The_Unit    : Compilation_Unit;
      The_Body    : Compilation_Unit);

   function Make_Nonexistent_Unit
     (The_Context    : Context;
      Full_Unit_Name : Program_Text;
      Unit_Kind      : Unit_Kinds) return Compilation_Unit;

   procedure Set_Compilation
     (The_Unit    : Compilation_Unit;
      Compilation : Compilations.Compilation);

   function Compilation
     (The_Unit : Compilation_Unit) return Compilations.Compilation;

   procedure Remove_Context_Clause
     (The_Unit : Compilation_Unit;
      Element  : Asis.Element);

--   procedure Set_Full_Name
--     (The_Unit    : Compilation_Unit);

   function Make_Limited_View_Unit
     (The_Context : Context;
      Declaration : Compilation_Unit) return Compilation_Unit;

   procedure Set_Unit_Declaration
     (Unit        : Compilation_Unit;
      Declaration : Asis.Element);

end Asis.Gela.Unit_Utils;


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
