<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:html="http://www.w3.org/1999/xhtml">

  <xsl:output method="text"
              indent="no"/>

  <xsl:variable name='newline'><xsl:text>&#xa;</xsl:text></xsl:variable>
  <xsl:variable name="smallcase" select="'abcdefghijklmnopqrstuvwxyz'" /> 
  <xsl:variable name="uppercase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" /> 
  <!-- https://stackoverflow.com/questions/4727816/xslt-sort-by-multiple-items -->
  <!-- https://stackoverflow.com/a/4728054 -->


  <xsl:template match=" @* | node() | processing-instruction() | comment() ">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template name="tag">
    <xsl:param name="name" />
    <xsl:value-of select="translate($name, $smallcase, $uppercase)"/>
    <xsl:text>:</xsl:text>
  </xsl:template>

  <xsl:template match=" html:entry/* ">
    <xsl:if test="normalize-space()">
      <xsl:text>  :</xsl:text>
      <xsl:value-of select="name()"/>
      <xsl:text>:    </xsl:text>
      <xsl:value-of select="."/>
      <xsl:value-of select="$newline"/>
    </xsl:if>
  </xsl:template>
  <xsl:template match=" html:entry/html:ID | html:entry/html:Summary "/>
  <xsl:template match="html:entry">
    <xsl:text>*** mantis </xsl:text>
    <xsl:value-of select="html:ID"/>
    <xsl:text> - </xsl:text>
    <xsl:value-of select="translate(html:Summary, $newline, ' ')"/>

    <xsl:text>      :</xsl:text>
    <xsl:call-template name="tag">
      <xsl:with-param name="name" select="html:Priority"/> 
    </xsl:call-template>
    <xsl:call-template name="tag">
      <xsl:with-param name="name" select="html:Serverity"/> 
    </xsl:call-template>

    <xsl:value-of select="$newline"/>
    <xsl:text>  :PROPERTIES:</xsl:text>
    <xsl:value-of select="$newline"/>
    <xsl:apply-templates/>
    <xsl:text>  :END:</xsl:text>
    <xsl:value-of select="$newline"/>
  </xsl:template>
  <xsl:template match="html:table">
    <xsl:text>** Bugs:</xsl:text>
    <xsl:value-of select="$newline"/>
    <xsl:value-of select="$newline"/>
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="/">
    <xsl:value-of select="$newline"/>
    <xsl:value-of select="$newline"/>
    <xsl:value-of select="$newline"/>
    <xsl:text>* Listing:</xsl:text>
    <xsl:value-of select="$newline"/>
    <xsl:value-of select="$newline"/>
    <xsl:apply-templates/>
  </xsl:template>
  
</xsl:stylesheet>
