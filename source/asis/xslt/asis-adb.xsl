<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:include href  = "common.xsl"/>

<xsl:output method="text"/>

<xsl:template match="/">

  <xsl:apply-templates select="//node[@name != 'Any_Compilation_Unit_Node']/attr">
    <xsl:sort select="@name"/>
  </xsl:apply-templates>

  <xsl:apply-templates select="//node[@abstract = 'y' and not(@helper)]">
    <xsl:sort select="@name"/>
  </xsl:apply-templates>

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
</xsl:template>
  


<xsl:template match="attr[../@name != 'Any_Compilation_Unit_Node']">
  <xsl:variable name="name" select="@name"/>

  <xsl:if test="generate-id() = generate-id(//attr[@name=$name][1]) 
                and not(@defer)">
    <xsl:apply-templates select="." mode="spec"/>
   is
   begin<xsl:choose>
    <xsl:when test="contains(@name, '_Kind')">
      return <xsl:call-template name="real-not-a-kind"/>;
</xsl:when>
<xsl:when test="starts-with (@name, 'Is_') or starts-with (@name, 'Has_')">
      return False;
</xsl:when>
<xsl:when test="@name='Declaration_Origin'">
      return Not_A_Declaration_Origin;
</xsl:when>
    <xsl:otherwise>
      Raise_Inappropriate_Element
        (Get_Context (Element), "<xsl:call-template name="attr-name"/>");
      return <xsl:choose>
   <xsl:when test="starts-with (@type, 'Primary_')">Nil_Element_List</xsl:when>
   <xsl:when test="@type = 'Asis.Component_Definition'">Nil_Element</xsl:when>
   <xsl:when test="@type = 'Asis.Constraint'">Nil_Element</xsl:when>
   <xsl:when test="@type = 'Asis.Declaration'">Nil_Element</xsl:when>
   <xsl:when test="@type = 'Asis.Defining_Name'">Nil_Element</xsl:when>
   <xsl:when test="@type = 'Asis.Definition'">Nil_Element</xsl:when>
   <xsl:when test="@type = 'Asis.Discrete_Range'">Nil_Element</xsl:when>
   <xsl:when test="@type = 'Asis.Discrete_Subtype_Definition'"
       >Nil_Element</xsl:when>
   <xsl:when test="@type = 'Asis.Element'">Nil_Element</xsl:when>
   <xsl:when test="@type = 'Asis.Expression'">Nil_Element</xsl:when>
   <xsl:when test="@type = 'Asis.Identifier'">Nil_Element</xsl:when>
   <xsl:when test="@type = 'Asis.Name'">Nil_Element</xsl:when>
   <xsl:when test="@type = 'Asis.Range_Constraint'">Nil_Element</xsl:when>
   <xsl:when test="@type = 'Asis.Subtype_Indication'">Nil_Element</xsl:when>
   <xsl:when test="@type = 'Unbounded_Wide_String'">""</xsl:when>
   <xsl:when test="@type = 'Asis.Declaration_Origins'"
       >Not_A_Declaration_Origin</xsl:when>
   <xsl:when test="@type = 'Asis.Compilation_Unit'"
       >Nil_Compilation_Unit</xsl:when>
   <xsl:when test="@type = 'Asis.Text_Position'"
       >Nil_Text_Position</xsl:when>
   <xsl:when test="@type = 'Asis.ASIS_Integer'">0</xsl:when>
   <xsl:when test="@type = 'Boolean'">False</xsl:when>
   <xsl:when test="starts-with (@type, 'Secondary_')"
       >Nil_Element_List</xsl:when>
   <xsl:when test="starts-with (@name, 'Corresponding_')"
       >Nil_Element</xsl:when>   <xsl:otherwise>
    <xsl:call-template name="attr-name"/> (Element)</xsl:otherwise>
   </xsl:choose><xsl:text>;</xsl:text>
</xsl:otherwise>
</xsl:choose>
   end <xsl:call-template name="attr-name"/>;
</xsl:if>
</xsl:template>



<xsl:template match="node [@name='Discrete_Subtype_Definition_Node']">
</xsl:template>



<xsl:template match="node">
    <xsl:apply-templates select="." mode="kind-spec"/> is
   begin
      return Not_<xsl:call-template name="real-kind-name"/>;
   end <xsl:call-template name="kind-name"/>;
</xsl:template>

</xsl:stylesheet>
