------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $
--  Purpose:
--  Scanner breaks text from source buffers into tokens.

with Asis.Gela.Scanner_Tables;
with Gela.Classificators;
with Gela.Source_Buffers;
with Gela.Character_Class_Buffers;

use Gela;

package Asis.Gela.Scanners is

   type Scanner
     (Classificator : access Classificators.Classificator'Class)
      is limited private;

   procedure Next_Token
     (Object : in out Scanner;
      Token  :    out Scanner_Tables.Token);

   procedure Token_Span
     (Object : in     Scanner;
      From   :    out Source_Buffers.Cursor;
      To     :    out Source_Buffers.Cursor);
   pragma Inline (Token_Span);

   function Token_Length (Object : Scanner) return Positive;
   pragma Inline (Token_Length);

   procedure Initialize
     (Object :    out Scanner;
      Cursor : in     Source_Buffers.Cursor);

   procedure Enter
     (Object : in out Scanner;
      State  : in     Scanner_Tables.State);
   pragma Inline (Enter);

private

   type Scanner
     (Classificator : access Classificators.Classificator'Class) is
      limited record
         Classes    : Character_Class_Buffers.Character_Class_Buffer;
         Start      : Scanner_Tables.State := Scanner_Tables.Default;
         Input      : Source_Buffers.Cursor;
         From       : Source_Buffers.Cursor;
         To         : Source_Buffers.Cursor;
         Surrogates : Natural := 0;
      end record;

end Asis.Gela.Scanners;


------------------------------------------------------------------------------
--  Copyright (c) 2008-2013, Maxim Reznik
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
