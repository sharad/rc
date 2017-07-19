<xsl:stylesheet
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:exslt="http://exslt.org/common"
    xmlns:myfn="http://whatever"
    xmlns:tf="transformation"
    xmlns:functx="http://www.functx.com"
    xmlns:str="http://exslt.org/strings"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:data="http://example.com/data"
    xmlns:dyn="http://exslt.org/dynamic">

  <!-- <xsl:import href="util.xsl" /> -->

  <xsl:param name="debug">1</xsl:param>

  <xsl:output name="xmlv" method="xml" indent="yes"/>
  <!-- <xsl:output name="textv" method="text"/> -->


  <!-- http://bytes.com/topic/xml/answers/774052-removing-attribute-element-xsl -->
  <!-- <xsl:import href="../OLD/xmlresume/xsl/themes/plain/themes/professional/utils/dyn/dyn.xsl"/> -->

  <xsl:param name="include"/>
  <xsl:param name="op"/>
  <xsl:param name="outdir">output</xsl:param>
  <xsl:param name="infile">in</xsl:param>


  <xsl:template match="/|*">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="text()|@*">
    <xsl:value-of select="."/>
  </xsl:template>
  <xsl:template match="processing-instruction()|comment()"/>


  <!-- </xsl:template> -->
  <!-- <xsl:template match="*"/> -->



  <!-- Copy input -->
  <!-- <xsl:template match="@*|node()" mode="asitis"/> -->

  <xsl:template match="@*|*|node()|text()|comment()|processing-instruction()">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="@*|*|node()|text()|comment()|processing-instruction()" mode="x">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="mnH2" priority="0" mode="x">
    <xsl:variable name="test" select="count(following-sibling::mnChapterTitle|following-sibling::mnH1|following-sibling::mnH2)"/>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:element name="tf:heading">
        <xsl:apply-templates select="*|node()|text()|comment()|processing-instruction()"/>
      </xsl:element>
      <xsl:apply-templates
          select="following-sibling::*[count(following-sibling::mnChapterTitle|following-sibling::mnH1|following-sibling::mnH2) = $test]"
          mode="x"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="mnH1" priority="0" mode="x">
    <xsl:variable name="test" select="count(following-sibling::mnChapterTitle|following-sibling::mnH1|following-sibling::mnH2)"/>
    <xsl:variable name="test1" select="count(following-sibling::mnChapterTitle|following-sibling::mnH1)"/>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:element name="tf:heading">
        <xsl:apply-templates select="*|node()|text()|comment()|processing-instruction()"/>
      </xsl:element>
      <xsl:apply-templates
          select="following-sibling::*[count(following-sibling::mnChapterTitle|following-sibling::mnH1|following-sibling::mnH2) = $test]"
          mode="x"/>
      <xsl:apply-templates
          select="following-sibling::mnH2[count(following-sibling::mnChapterTitle|following-sibling::mnH1) = $test1]"
          mode="x"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="mnChapterTitle" priority="0">
    <xsl:variable name="test1" select="count(following-sibling::mnChapterTitle)"/>
    <xsl:variable name="test2" select="count(following-sibling::mnChapterTitle|following-sibling::mnH1)"/>
    <xsl:variable name="test" select="count(following-sibling::mnChapterTitle|following-sibling::mnH1|following-sibling::mnH2)"/>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:element name="tf:heading">
        <xsl:apply-templates select="*|node()|text()|comment()|processing-instruction()"/>
      </xsl:element>
      <xsl:apply-templates
          select="following-sibling::*[count(following-sibling::mnChapterTitle|following-sibling::mnH1|following-sibling::mnH2) = $test]"
          mode="x"/>
      <xsl:apply-templates
          select="following-sibling::mnH1[count(following-sibling::mnChapterTitle) = $test1]
                  |following-sibling::mnH2[count(following-sibling::mnChapterTitle|following-sibling::mnH1) = $test2]"
          mode="x"/>
    </xsl:copy>
  </xsl:template>


  <!-- <xsl:template match="mnChapterTitle/following-sibling::*"/> -->
  <!-- <xsl:template match="mnH1/following-sibling::*"/> -->
  <!-- <xsl:template match="mnH2/following-sibling::*"/> -->

  <xsl:template match="*">
    <xsl:if test="count(preceding-sibling::mnChapterTitle|preceding-sibling::mnH1|preceding-sibling::mnH2) = 0">
      <xsl:copy>
        <xsl:apply-templates select="@*"/>
        <xsl:apply-templates/>
      </xsl:copy>
    </xsl:if>
  </xsl:template>

  <xsl:template match="mnH1">
    <xsl:if test="count(preceding-sibling::mnChapterTitle) = 0">
      <xsl:apply-templates select="." mode="x"/>
    </xsl:if>
  </xsl:template>

  <xsl:template match="mnH2">
    <xsl:if test="count(preceding-sibling::mnChapterTitle|preceding-sibling::mnH1) = 0">
      <xsl:apply-templates select="." mode="x"/>
    </xsl:if>
  </xsl:template>

  <!-- <xsl:template match="/"> -->
  <!--   <xsl:copy> -->
  <!--     <xsl:apply-templates select="@*"/> -->
  <!--     <xsl:apply-templates select="//mnChapterTitle[1]/preceding-sibling::*" mode="pre"/> -->
  <!--     <xsl:apply-templates select="//mnChapterTitle"/> -->
  <!--   </xsl:copy> -->
  <!-- </xsl:template> -->


</xsl:stylesheet>

