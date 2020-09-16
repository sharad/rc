<xsl:stylesheet
    version="1.0"
    xmlns:exsl="http://exslt.org/common"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:m="http://www.merunetworks.com/info/0.0"
    xmlns:mn="http://www.merunetworks.com/namespace/melf"
    xmlns:db="http://docbook.org/ns/docbook"
    xmlns:xl="http://www.w3.org/1999/xlink"
    xmlns:meru="http://www.merunetworks.com/namespace/"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- <xsl:import href="util.xsl" /> -->

  <xsl:param name="debug">1</xsl:param>
  <xsl:param name="stylesheet"/>

  <xsl:output method="xml" indent="yes"/>
  <xsl:output
      omit-xml-declaration="yes"
      method="xml"
      media-type="application/xml"
      indent="yes"
      doctype-public="-//OASIS//DTD DocBook XML V4.5//EN"/>

  <xsl:variable name='newline'><xsl:text>&#xa;</xsl:text></xsl:variable>
  <!-- <xsl:variable name='missingval'><xsl:text>N/A</xsl:text></xsl:variable> -->
  <!-- <xsl:variable name='missingval'><xsl:text disable-output-escaping="yes">&#160;</xsl:text></xsl:variable> -->
  <xsl:variable name='missingval'><xsl:text disable-output-escaping="yes">N/A</xsl:text></xsl:variable>
  <xsl:variable name="indent" select="0"/>



  <xsl:template match="@*|*|processing-instruction()|comment()">
    <!-- <xsl:copy> -->
    <!--   <xsl:apply-templates select="*|@*|text()|processing-instruction()|comment()"/> -->
    <!-- </xsl:copy> -->
    <xsl:apply-templates select="*"/>
  </xsl:template>

  <!-- <xsl:template match="mn:instance|mn:entry"> -->

  <!--   <xsl:copy> -->
  <!--     <xsl:apply-templates select="@*|node()"/> -->
  <!--     <!-\- <mn:trigger><xsl:value-of select="mn:description/mn:long"/></mn:trigger> -\-> -->
  <!--   </xsl:copy> -->
  <!--   <!-\- <xsl:apply-templates/> -\-> -->
  <!-- </xsl:template> -->



  <xsl:template match="html:table">
    <xsl:element name="table">
      <xsl:for-each select="html:tr">
        <xsl:element name="entry">
          <xsl:for-each select="html:td">
            <xsl:variable name="pos">
              <xsl:value-of select="position()"/>
            </xsl:variable>
            <!-- <xsl:element name="position"> -->
            <!--   <xsl:value-of select="$pos"/> -->
            <!--   <xsl:value-of select="../../html:tr[1]/html:th[ position() = $pos ]"/> -->
            <!-- </xsl:element> -->
            <xsl:element name="{translate(../../html:tr[1]/html:th[ position() = $pos ],'  &#x9;&#xa;&#xd;', '')}">
              <xsl:value-of select="."/>
            </xsl:element>
          </xsl:for-each>
        </xsl:element>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>

  <xsl:template match="/">
    <xsl:element name="tables">
      <xsl:apply-templates select="*"/>
    </xsl:element>
  </xsl:template>


</xsl:stylesheet>
