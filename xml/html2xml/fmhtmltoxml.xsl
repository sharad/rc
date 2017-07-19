<xsl:stylesheet
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:exslt="http://exslt.org/common"
    xmlns:tf="transformation"
    xmlns:myfn="http://whatever"
    xmlns:functx="http://www.functx.com"
    xmlns:str="http://exslt.org/strings"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:data="http://example.com/data"
    xmlns:dyn="http://exslt.org/dynamic">

  <!-- <xsl:import href="util.xsl" /> -->

  <xsl:param name="debug">1</xsl:param>
  <xsl:param name="stylesheet"/>

  <xsl:output name="xmlv" method="xml" indent="yes"/>
  <!-- <xsl:output name="textv" method="text"/> -->


  <!-- http://bytes.com/topic/xml/answers/774052-removing-attribute-element-xsl -->
  <!-- <xsl:import href="../OLD/xmlresume/xsl/themes/plain/themes/professional/utils/dyn/dyn.xsl"/> -->

  <xsl:param name="include"/>
  <xsl:param name="op"/>
  <xsl:param name="outdir">output</xsl:param>
  <xsl:param name="infile">in</xsl:param>

  <xsl:variable name='newline'><xsl:text>&#xa;</xsl:text></xsl:variable>

  <xsl:template match="/|*">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="text()|@*">
    <xsl:value-of select="."/>
  </xsl:template>

  <!-- <xsl:template match="processing-instruction()|comment()"/> -->

  <!-- <xsl:template match="@*|*|node()|text()|comment()|processing-instruction()"> -->
  <xsl:template match="@*|*|node()|text()|comment()">
    <xsl:copy>
      <!-- <xsl:apply-templates select="@*"/> -->
      <!-- <xsl:apply-templates/> -->
      <xsl:apply-templates select="@* | node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="processing-instruction('xml-stylesheet')" mode="o">
    <xsl:element name="link">
      <!-- <xsl:apply-templates select="@*"/> -->
      <xsl:attribute name="href">
        <xsl:value-of select="href"/>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>


  <xsl:template match="/">
    <xsl:if test="$stylesheet">
      <xsl:processing-instruction name="xml-stylesheet">
        href="<xsl:value-of select="$stylesheet"/>" type="text/xsl"
      </xsl:processing-instruction>
      <xsl:value-of select="$newline"/>
    </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>


  <xsl:template match="*[@class]">
    <!-- Must see http://stackoverflow.com/a/17189003 -->
    <xsl:element name="{@class}">


      <!-- <xsl:copy-of select="./*|./text()"/> -->

      <!-- <xsl:copy> -->
      <!--   <xsl:apply-templates select="@*"/> -->
      <!--   <xsl:apply-templates select="./*|./text()"/> -->
      <!-- </xsl:copy> -->

      <!-- <xsl:copy> -->
      <!--   <xsl:apply-templates select="./*"/> -->
      <!-- </xsl:copy> -->

      <!-- <xsl:apply-templates select="@*"/> -->
      <!-- <xsl:copy-of select="./*"/> -->


      <!-- Above will work if elements don't have any attribute, but not otherwise -->

      <xsl:copy-of select="@*" />
      <xsl:apply-templates select="*|text()" />



    </xsl:element>
  </xsl:template>



  <xsl:template match="*[@class]" mode="fortest">
    <xsl:copy>

      <xsl:copy-of select="@*" />
      <xsl:apply-templates select="*|text()" />

    </xsl:copy>
  </xsl:template>


</xsl:stylesheet>




