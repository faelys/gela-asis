------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision: 209 $ $Date: 2013-11-30 21:03:24 +0200 (Сб., 30 нояб. 2013) $

package body Gela.Hash.CRC.b32 is

   Keys : constant array (CRC32 range 0 .. 255) of CRC32 :=
     (0,          1996959894, 3993919788, 2567524794, 124634137,  1886057615,
      3915621685, 2657392035, 249268274,  2044508324, 3772115230, 2547177864,
      162941995,  2125561021, 3887607047, 2428444049, 498536548,  1789927666,
      4089016648, 2227061214, 450548861,  1843258603, 4107580753, 2211677639,
      325883990,  1684777152, 4251122042, 2321926636, 335633487,  1661365465,
      4195302755, 2366115317, 997073096,  1281953886, 3579855332, 2724688242,
      1006888145, 1258607687, 3524101629, 2768942443, 901097722,  1119000684,
      3686517206, 2898065728, 853044451,  1172266101, 3705015759, 2882616665,
      651767980,  1373503546, 3369554304, 3218104598, 565507253,  1454621731,
      3485111705, 3099436303, 671266974,  1594198024, 3322730930, 2970347812,
      795835527,  1483230225, 3244367275, 3060149565, 1994146192, 31158534,
      2563907772, 4023717930, 1907459465, 112637215,  2680153253, 3904427059,
      2013776290, 251722036,  2517215374, 3775830040, 2137656763, 141376813,
      2439277719, 3865271297, 1802195444, 476864866,  2238001368, 4066508878,
      1812370925, 453092731,  2181625025, 4111451223, 1706088902, 314042704,
      2344532202, 4240017532, 1658658271, 366619977,  2362670323, 4224994405,
      1303535960, 984961486,  2747007092, 3569037538, 1256170817, 1037604311,
      2765210733, 3554079995, 1131014506, 879679996,  2909243462, 3663771856,
      1141124467, 855842277,  2852801631, 3708648649, 1342533948, 654459306,
      3188396048, 3373015174, 1466479909, 544179635,  3110523913, 3462522015,
      1591671054, 702138776,  2966460450, 3352799412, 1504918807, 783551873,
      3082640443, 3233442989, 3988292384, 2596254646, 62317068,   1957810842,
      3939845945, 2647816111, 81470997,   1943803523, 3814918930, 2489596804,
      225274430,  2053790376, 3826175755, 2466906013, 167816743,  2097651377,
      4027552580, 2265490386, 503444072,  1762050814, 4150417245, 2154129355,
      426522225,  1852507879, 4275313526, 2312317920, 282753626,  1742555852,
      4189708143, 2394877945, 397917763,  1622183637, 3604390888, 2714866558,
      953729732,  1340076626, 3518719985, 2797360999, 1068828381, 1219638859,
      3624741850, 2936675148, 906185462,  1090812512, 3747672003, 2825379669,
      829329135,  1181335161, 3412177804, 3160834842, 628085408,  1382605366,
      3423369109, 3138078467, 570562233,  1426400815, 3317316542, 2998733608,
      733239954,  1555261956, 3268935591, 3050360625, 752459403,  1541320221,
      2607071920, 3965973030, 1969922972, 40735498,   2617837225, 3943577151,
      1913087877, 83908371,   2512341634, 3803740692, 2075208622, 213261112,
      2463272603, 3855990285, 2094854071, 198958881,  2262029012, 4057260610,
      1759359992, 534414190,  2176718541, 4139329115, 1873836001, 414664567,
      2282248934, 4279200368, 1711684554, 285281116,  2405801727, 4167216745,
      1634467795, 376229701,  2685067896, 3608007406, 1308918612, 956543938,
      2808555105, 3495958263, 1231636301, 1047427035, 2932959818, 3654703836,
      1088359270, 936918000,  2847714899, 3736837829, 1202900863, 817233897,
      3183342108, 3401237130, 1404277552, 615818150,  3134207493, 3453421203,
      1423857449, 601450431,  3009837614, 3294710456, 1567103746, 711928724,
      3020668471, 3272380065, 1510334235, 755167117);

   subtype Byte is CRC32 range 0 .. 255;

   procedure Update_Hash
     (This  : in out Hasher;
      Value : in     Byte);
   pragma Inline (Update_Hash);

   ------------
   -- Update --
   ------------

   procedure Update
     (This  : in out Hasher;
      Value : in     String) is
   begin
      This.Length := This.Length + Value'Length;

      if This.Length > Maximum_Length then
         raise Maximum_Length_Error;
      end if;

      for J in Value'Range loop
         Update_Hash (This, Character'Pos (Value (J)));
      end loop;
   end Update;

   -----------------
   -- Wide_Update --
   -----------------

   procedure Wide_Update
     (This  : in out Hasher;
      Value : in     Wide_String) is
   begin
      This.Length := This.Length + 2 * Value'Length;

      if This.Length > Maximum_Length then
         raise Maximum_Length_Error;
      end if;

      for J in Value'Range loop
         Update_Hash (This, Wide_Character'Pos (Value (J)) and 16#FF#);
         Update_Hash (This, Shift_Right (Wide_Character'Pos (Value (J)), 8));
      end loop;
   end Wide_Update;

   ----------------------
   -- Wide_Wide_Update --
   ----------------------

   procedure Wide_Wide_Update
     (This  : in out Hasher;
      Value : in     Wide_Wide_String)
   is
      subtype W is Wide_Wide_Character;
   begin
      This.Length := This.Length + 4 * Value'Length;

      if This.Length > Maximum_Length then
         raise Maximum_Length_Error;
      end if;

      for J in Value'Range loop
         Update_Hash (This,              W'Pos (Value (J))      and 16#FF#);
         Update_Hash (This, Shift_Right (W'Pos (Value (J)),  8) and 16#FF#);
         Update_Hash (This, Shift_Right (W'Pos (Value (J)), 16) and 16#FF#);
         Update_Hash (This, Shift_Right (W'Pos (Value (J)), 24));
      end loop;
   end Wide_Wide_Update;

   ------------
   -- Update --
   ------------

   procedure Update
     (This  : in out Hasher;
      Value : in     Ada.Streams.Stream_Element_Array) is
   begin
      This.Length := This.Length + Value'Length;

      if This.Length > Maximum_Length then
         raise Maximum_Length_Error;
      end if;

      for J in Value'Range loop
         Update_Hash (This, CRC32 (Value (J)));
      end loop;
   end Update;

   ---------------
   -- Calculate --
   ---------------

   function Calculate (Value : in String) return CRC32 is
      H : Hasher;
   begin
      Update (H, Value);
      return Result (H);
   end Calculate;

   --------------------
   -- Wide_Calculate --
   --------------------

   function Wide_Calculate (Value : in Wide_String) return CRC32 is
      H : Hasher;
   begin
      Wide_Update (H, Value);
      return Result (H);
   end Wide_Calculate;

   ---------------
   -- Calculate --
   ---------------

   function Wide_Wide_Calculate (Value : in Wide_Wide_String) return CRC32 is
      H : Hasher;
   begin
      Wide_Wide_Update (H, Value);
      return Result (H);
   end Wide_Wide_Calculate;

   ---------------
   -- Calculate --
   ---------------

   function Calculate
     (Value : in Ada.Streams.Stream_Element_Array)
      return CRC32
   is
      H : Hasher;
   begin
      Update (H, Value);
      return Result (H);
   end Calculate;

   -------------
   -- To_Hash --
   -------------

   function To_Hash (T : in CRC32) return Hash_Type is
   begin
      return Hash_Type (T);
   end To_Hash;

   ---------------
   -- Calculate --
   ---------------

   function Calculate (Value : in String) return Hash_Type is
   begin
      return To_Hash (Calculate (Value));
   end Calculate;

   --------------------
   -- Wide_Calculate --
   --------------------

   function Wide_Calculate (Value : in Wide_String) return Hash_Type is
   begin
      return To_Hash (Wide_Calculate (Value));
   end Wide_Calculate;

   ---------------
   -- Calculate --
   ---------------

   function Wide_Wide_Calculate
     (Value : in Wide_Wide_String)
      return Hash_Type
   is
   begin
      return To_Hash (Wide_Wide_Calculate (Value));
   end Wide_Wide_Calculate;

   ---------------
   -- Calculate --
   ---------------

   function Calculate
     (Value : in Ada.Streams.Stream_Element_Array)
      return Hash_Type
   is
   begin
      return To_Hash (Calculate (Value));
   end Calculate;

   ------------
   -- Update --
   ------------

   procedure Update_Hash
     (This  : in out Hasher;
      Value : in     Byte)
   is
   begin
      This.Cm_Reg := Shift_Right (This.Cm_Reg, 8) xor
        Keys (Value xor (This.Cm_Reg and 16#0000_00FF#));
   end Update_Hash;

   ------------
   -- Result --
   ------------

   function Result (This : in Hasher) return CRC32 is
   begin
      return This.Cm_Reg xor 16#FFFFFFFF#;
   end Result;

end Gela.Hash.CRC.b32;

------------------------------------------------------------------------------
--  Copyright (c) 2006, Andry Ogorodnik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--     * this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--     * notice, this list of conditions and the following disclaimer in the
--     * documentation and/or other materials provided with the distribution.
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
