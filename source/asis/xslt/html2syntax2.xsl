<?xml version="1.0" encoding="utf-8"?>
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

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="text" indent="yes" encoding="utf-8"/>

<xsl:template match="/">
  <xsl:apply-templates select="*"/>
</xsl:template>

<xsl:template match="gramar">
begin_gramar
    <xsl:apply-templates select="*"/>
end_gramar
</xsl:template>


<xsl:template match="rule[count(span[@class='swiss']/text)=0]"/>

<xsl:template match="rule">
do_rule "<xsl:value-of select="span[@class='swiss'][1]/text/@value"/>" "<xsl:value-of select="label/@text"/>" 
<xsl:apply-templates select="*[position()>2]"/>
end_rule
</xsl:template>

<xsl:template match="span[count(*)=0]"/>

<xsl:template match="span">
  <xsl:if test="not((a[@href]/text)|text)">
ERROR
</xsl:if>
do_ref "<xsl:value-of select="descendant::text/@value"/>" "<xsl:if
  test="name(preceding-sibling::*[1])='i'">
  <xsl:value-of select="preceding-sibling::i[1]/text/@value"/>
</xsl:if>"
</xsl:template>

<xsl:template match="text">
  <xsl:variable name="quotion">'</xsl:variable>
  <xsl:if test='contains(@value,$quotion)'>
do_texts "<xsl:value-of select="@value"/>"
  </xsl:if>
  <xsl:if test="not(contains(@value,$quotion))">
do_texts '<xsl:value-of select="@value"/>'
  </xsl:if>
</xsl:template>

<xsl:template match="b">
do_keyword "<xsl:value-of select="text/@value"/>"
</xsl:template>

<xsl:template match="i|text()"/>

</xsl:stylesheet>
