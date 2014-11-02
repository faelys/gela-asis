--------------------------------------------------------
--                E n c o d i n g s                   --
--                                                    --
-- Tools for convertion strings between Unicode and   --
-- national/vendor character sets.                    --
--                - - - - - - - - -                   --
-- Read copyright and license at the end of this file --
--------------------------------------------------------

--  This unit points to encodings linked into executable
--  Add here required encodings
--  If encoding isn't linked it will be read from text
--  file in run time (if Encodings.Maps.Runtime linked).
--  Otherwite encoding isn't available.

with Encodings.Maps.Utf_8;
--  with Encodings.Maps.Runtime;

--  with Encodings.Maps.ISO_8859_1;
--  with Encodings.Maps.ISO_8859_2;
--  with Encodings.Maps.ISO_8859_3;
--  with Encodings.Maps.ISO_8859_4;
--  with Encodings.Maps.ISO_8859_5;
--  with Encodings.Maps.ISO_8859_6;
--  with Encodings.Maps.ISO_8859_7;
--  with Encodings.Maps.ISO_8859_8;
--  with Encodings.Maps.ISO_8859_9;
--  with Encodings.Maps.ISO_8859_10;
--  with Encodings.Maps.ISO_8859_11;
--  with Encodings.Maps.ISO_8859_13;
--  with Encodings.Maps.ISO_8859_14;
--  with Encodings.Maps.ISO_8859_15;
--  with Encodings.Maps.ISO_8859_16;

--  with Encodings.Maps.CP_037;
--  with Encodings.Maps.CP_424;
--  with Encodings.Maps.CP_437;
--  with Encodings.Maps.CP_500;
--  with Encodings.Maps.CP_737;
--  with Encodings.Maps.CP_775;
--  with Encodings.Maps.CP_850;
--  with Encodings.Maps.CP_852;
--  with Encodings.Maps.CP_855;
--  with Encodings.Maps.CP_856;
--  with Encodings.Maps.CP_857;
--  with Encodings.Maps.CP_860;
--  with Encodings.Maps.CP_861;
--  with Encodings.Maps.CP_862;
--  with Encodings.Maps.CP_863;
--  with Encodings.Maps.CP_864;
--  with Encodings.Maps.CP_865;
--  with Encodings.Maps.CP_866;
--  with Encodings.Maps.CP_869;
--  with Encodings.Maps.CP_874;
--  with Encodings.Maps.CP_875;
--  with Encodings.Maps.CP_1006;
--  with Encodings.Maps.CP_1026;
--  with Encodings.Maps.CP_1250;
--  with Encodings.Maps.CP_1251;
--  with Encodings.Maps.CP_1252;
--  with Encodings.Maps.CP_1253;
--  with Encodings.Maps.CP_1254;
--  with Encodings.Maps.CP_1255;
--  with Encodings.Maps.CP_1256;
--  with Encodings.Maps.CP_1257;
--  with Encodings.Maps.CP_1258;

--  with Encodings.Maps.AtariST;
--  with Encodings.Maps.KOI8_R;

package Encodings.Maps.Linked is
end Encodings.Maps.Linked;


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
