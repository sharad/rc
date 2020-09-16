<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:html="http://www.w3.org/1999/xhtml">
  <xsl:output method="xml" indent="yes"/>
  <!-- https://stackoverflow.com/questions/4727816/xslt-sort-by-multiple-items -->
  <!-- https://stackoverflow.com/a/4728054 -->
    <xsl:template match="@*|node()">
        <xsl:copy>
          <xsl:apply-templates select="@*|node()" />
        </xsl:copy>
    </xsl:template>
    <xsl:template match="html:table">
      <xsl:copy>
        <xsl:apply-templates>
          <xsl:sort select="html:Assigned" order="ascending"/>
          <xsl:sort select="html:Priority"/>
          <xsl:sort select="html:Serverity"/>
          <xsl:sort select="html:Status"/>
        </xsl:apply-templates>
      </xsl:copy>
    </xsl:template>
    
    <xsl:template match="html:entry">
    </xsl:template>
    <xsl:template match="html:entry[html:Assigned = 'Sharad_Pratap']">
      <xsl:apply-templates select="."/>
    </xsl:template>
</xsl:stylesheet>
