<?xml version='1.0'?>
<!--
  Copyright (c) 2006-2013, Maxim Reznik
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

     * Redistributions of source code must retain the above copyright notice,
       this list of conditions and the following disclaimer.
     * Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.
     * Neither the name of the Maxim Reznik, IE nor the names of its
       contributors may be used to endorse or promote products derived from
       this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
-->

<!DOCTYPE stylesheet [
<!ENTITY nbsp "&#160;">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="html" indent="yes" encoding="utf-8"/>

<xsl:template match="/">
<html>
<head/>
    <xsl:apply-templates select="*"/>
</html>
</xsl:template>

<xsl:template match="gramar">
  <table border='0'>
    <xsl:apply-templates select="*">
      <xsl:sort select="@name"/>
    </xsl:apply-templates>
  </table>
</xsl:template>

<xsl:template match="rule">
   <xsl:apply-templates select="*"/>
</xsl:template>

<xsl:template match="alts">
  <xsl:apply-templates select="*"/>
</xsl:template>

<xsl:template match="seq">
  <xsl:if test="position()=1">
  <tr><td>&nbsp;</td></tr>
  </xsl:if>
  <tr>
    <td>
      <xsl:if test="position()=1">
        <a name="{ancestor::rule/@name}"/>
        <xsl:value-of select="ancestor::rule/@name"/>
      </xsl:if>
    </td>
    <td>
      <xsl:if test="not(@status)">
       <xsl:attribute name="style">background:#e0e0e0</xsl:attribute>
      </xsl:if>
      <xsl:if test="position()!=1"> | &nbsp;</xsl:if>
      <xsl:apply-templates select="*"/>
    </td>
  </tr>
</xsl:template>

<xsl:template match="deleted-seq">
  <xsl:if test="position()=1">
  <tr><td>&nbsp;</td></tr>
  </xsl:if>
  <tr>
    <td>
      <xsl:if test="position()=1">
        <xsl:value-of select="ancestor::rule/@name"/>
      </xsl:if>
    </td>
    <td style="text-decoration:line-through">
        <xsl:if test="position()!=1"> | &nbsp;</xsl:if>
        <xsl:apply-templates select="*"/>
    </td>
  </tr>
</xsl:template>

<xsl:template match="ref">
  <xsl:if test="@italic">
  <i><xsl:value-of select="@italic"/></i>
  </xsl:if>
  <a href="#{@name}"><xsl:value-of select="@name"/></a>&nbsp;
</xsl:template>

<xsl:template match="opt">
 [ &nbsp;
   <xsl:apply-templates select="seq/*"/>
 ] &nbsp;
</xsl:template>

<xsl:template match="list">
 { &nbsp;
   <xsl:apply-templates select="seq/*"/>
 } &nbsp;
</xsl:template>

<xsl:template match="keyword|delim">
  <b><xsl:value-of select="@text"/></b> &nbsp;
</xsl:template>

</xsl:stylesheet>
