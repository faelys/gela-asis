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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<xsl:output method="xml" indent='yes'/>

<xsl:variable name="tokens" select="document('../model/tokens.xml')"/>
<xsl:variable name="fix"    select="document('../model/fix.xml')"/>

<xsl:template match="/|*|@*">
  <xsl:copy>
   <xsl:apply-templates select="*|@*"/>
  </xsl:copy>
</xsl:template>
  
<xsl:template match="rule" >
 <xsl:variable name="rule-name" select="@name"/>
 <xsl:if test="count($tokens//token[@name=$rule-name])=0">
  <xsl:copy>
   <xsl:apply-templates select="@*"/>
   <xsl:if test="count($fix//rule[@name=$rule-name])=0">
     <xsl:apply-templates select="*"/>
   </xsl:if>
   <xsl:if test="count($fix//rule[@name=$rule-name])!=0">
     <xsl:apply-templates select="$fix//rule[@name=$rule-name]/*"/>
   </xsl:if>
  </xsl:copy>
 </xsl:if>
</xsl:template>

<xsl:template match="gramar">
  <xsl:copy>
   <xsl:apply-templates select="*|@*"/>
   <xsl:apply-templates select="$fix//rule[@added='yes']"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="deleted-seq"/>

</xsl:stylesheet>
