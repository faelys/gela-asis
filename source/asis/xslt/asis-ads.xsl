<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:include href  = "common.xsl"/>
<xsl:output method="text"/>

<xsl:template match="/">

  <xsl:apply-templates
     select="//node[@name != 'Any_Compilation_Unit_Node']/attr">
    <xsl:sort select="@name"/>
  </xsl:apply-templates>

  <xsl:apply-templates select="//node[@abstract = 'y' and not(@helper)]">
    <xsl:sort select="@name"/>
  </xsl:apply-templates>

  <xsl:text>
   Nil_Element : constant Element := null;

   Nil_Element_List : constant Element_List (1 .. 0) :=
       (1 .. 0 => null);

   ----------------------
   -- Compilation_Unit --
   ----------------------

   type Compilation_Unit_Node is abstract new Element_Node with null record;
   type Compilation_Unit is access all Compilation_Unit_Node'Class;
   for Compilation_Unit'Storage_Size use 0;

</xsl:text>

  <xsl:apply-templates select="//node[@name = 'Any_Compilation_Unit_Node']/attr">
    <xsl:sort select="@name"/>
  </xsl:apply-templates>

  <xsl:text>

   Nil_Compilation_Unit : constant Compilation_Unit := null;

   Nil_Compilation_Unit_List : constant Compilation_Unit_List (1 .. 0) :=
       (1 .. 0 => null);


   function Assigned (Item : in Context) return Boolean;
   function Assigned (Item : in Compilation_Unit) return Boolean;
   function Assigned (Item : in Element) return Boolean;

   pragma Inline (Assigned);

end Asis;


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
</xsl:text>

</xsl:template>



<xsl:template match="attr[../@name != 'Any_Compilation_Unit_Node']">
  <xsl:variable name="name" select="@name"/>

  <xsl:if test="generate-id() = generate-id(//attr[@name=$name][1]) and not(@defer)">
    <xsl:apply-templates select="." mode="spec"/>

    <xsl:text>;
</xsl:text>

   </xsl:if>
</xsl:template>



<xsl:template match="attr[../@name = 'Any_Compilation_Unit_Node']">
  <xsl:variable name="name" select="@name"/>

  <xsl:if test="not(@defer)">
    <xsl:apply-templates select="." mode="spec">
      <xsl:with-param name="type">Compilation_Unit_Node</xsl:with-param>
    </xsl:apply-templates>
      is abstract;
</xsl:if>
</xsl:template>



<xsl:template match="node [@name='Discrete_Subtype_Definition_Node']">
</xsl:template>



<xsl:template match="node">
    <xsl:apply-templates select="." mode="kind-spec"/>

    <xsl:text>;
</xsl:text>
</xsl:template>

</xsl:stylesheet>
