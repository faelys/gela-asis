with Gela;                     use Gela;
with Gela.Decoders;
with Gela.Encodings;
with Gela.Source_Buffers;
with Asis.Gela.Lines;

package Asis.Gela.Parser is 
   procedure Run
     (The_Context : in     Asis.Context;
      Input       : in     Source_Buffers.Source_Buffer'Class;
      Encoding    : in     Encodings.Encoding;
      Decoder     : in     Decoders.Decoder'Class;
      Line_List   : in out Lines.Vector;
      Result      :    out Asis.Element);
end Asis.Gela.Parser;
