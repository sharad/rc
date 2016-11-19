<?xml version='1.0'?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns="http://www.w3.org/1999/xhtml"
    version="1.0">

  <xsl:import href="/usr/share/xml/docbook/stylesheet/docbook-xsl-ns/fo/docbook.xsl"/>

  <!-- <xsl:param name="html.stylesheet" select="'stalog.css'"/> -->
  <!-- <xsl:param name="admon.graphics" select="1"/> -->


  <!-- <xsl:param name="generate.toc" select="nop"/> -->

  <!-- <xsl:param name="generate.toc" select="'book toc'"/> -->



  <!-- START    -->
  <!-- from: https://lists.oasis-open.org/archives/docbook-apps/201207/msg00006.html -->
  <!-- My Table Mods -->
  <xsl:param name="met.table.font.size">0.5</xsl:param> <!--  -->
  <xsl:param name="met.table.head.font.size">0.5</xsl:param>
  <!-- Set table body font size and alignment -->
  <xsl:attribute-set name="table.properties">
    <xsl:attribute name="keep-together.within-column">auto</xsl:attribute>
    <xsl:attribute name="font-size">
      <xsl:value-of select="$body.font.master * $met.table.font.size"/>
      <xsl:text>pt</xsl:text>
    </xsl:attribute>
  </xsl:attribute-set>
  <!-- Set table header font size -->
  <xsl:template name="table.row.properties">
    <xsl:if test="ancestor::thead">
      <xsl:attribute name="font-weight">bold</xsl:attribute>
      <xsl:attribute name="color">#FFFFFF</xsl:attribute>
      <!-- White -->
      <xsl:attribute name="background-color">#000000</xsl:attribute>
      <!-- Black -->
      <xsl:attribute name="font-size">
        <xsl:value-of
            select="$body.font.master * $met.table.head.font.size" />
        <xsl:text>pt</xsl:text>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <!-- END -->



</xsl:stylesheet>
