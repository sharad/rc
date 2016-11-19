<xsl:stylesheet
    version="1.0"
    xmlns:exsl="http://exslt.org/common"
    xmlns:db="http://docbook.org/ns/docbook"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:m="http://www.merunetworks.com/info/0.0"
    xmlns:mn="http://www.merunetworks.com/namespace/melf"
    xmlns:xl="http://www.w3.org/1999/xlink"
    xmlns:meru="http://www.merunetworks.com/namespace/"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- <xsl:import href="util.xsl" /> -->

  <xsl:param name="debug">1</xsl:param>
  <xsl:param name="stylesheet"/>

  <xsl:output method="text" omit-xml-declaration="yes" indent="no"/>

  <xsl:variable name='space'><xsl:text>&#032;</xsl:text></xsl:variable>
  <xsl:variable name='newline'><xsl:text>&#xa;</xsl:text></xsl:variable>
  <xsl:variable name='doublequote'><xsl:text>&quot;</xsl:text></xsl:variable>
  <xsl:variable name='missingval'><xsl:text disable-output-escaping="yes">N/A</xsl:text></xsl:variable>
  <xsl:variable name="indent" select="0"/>
  <xsl:variable name="smallcase" select="'abcdefghijklmnopqrstuvwxyz'" />
  <xsl:variable name="uppercase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />
  <xsl:variable name="cppinvalidchars" select="')(][:&gt;&lt;-*\%!@$&amp;='" />
  <xsl:variable name="cppvalidchars" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_'" />

  <xsl:template match="/|*">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="text()|@*">
    <xsl:value-of select="."/>
  </xsl:template>

  <xsl:template match="/|*" mode="member">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="text()|@*">
    <xsl:value-of select="."/>
  </xsl:template>

  <!-- <xsl:template match="processing-instruction()|comment()"/> -->

  <!-- <xsl:template match="@*|*|node()|text()|comment()|processing-instruction()"> -->
  <xsl:template match="@*|*|node()|text()|comment()">
    <!-- <xsl:copy> -->
    <!--   <xsl:apply-templates select="@*"/> -->
    <!--   <xsl:apply-templates/> -->
    <!-- </xsl:copy> -->
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="mn:entry/comment()">
    <xsl:value-of select="$newline"/>
    <xsl:value-of select="."/>
    <xsl:value-of select="$newline"/>
  </xsl:template>


  <!-- ==================================================== -->


  <xsl:template match="mn:*">
    <xsl:apply-templates/>
  </xsl:template>





  <xsl:template match="mn:entry">

    <xsl:value-of select="$newline"/>
    <xsl:apply-templates select="comment()"/>
    <xsl:value-of select="$newline"/>
    <xsl:value-of select="$newline"/>

  </xsl:template> <!-- <xsl:template match="mn:entry"> -->


  <!-- <xsl:template match="mn:removecomment/comment()"> -->
  <!-- </xsl:template> -->

</xsl:stylesheet>
