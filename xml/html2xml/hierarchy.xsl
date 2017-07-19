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

  <xsl:param name="stylesheetofoutput"/>
  <xsl:variable name='newline'><xsl:text>&#xa;</xsl:text></xsl:variable>

  <xsl:template match="/">
    <xsl:element name="xsl:stylesheet">
      <xsl:attribute
          namespace="xmlns"
          name="tf">transformation</xsl:attribute>
      <xsl:attribute
          namespace="xmlns"
          name="xsl">http://www.w3.org/1999/XSL/Transform</xsl:attribute>
      <xsl:attribute name="version">1.0</xsl:attribute>


      <xsl:comment>
        <xsl:element name="xsl:import">
          <xsl:attribute name="href">util.xsl</xsl:attribute>
        </xsl:element>
        <xsl:element name="xsl:namespace-alias">
          <xsl:attribute name="result-prefix">xsl</xsl:attribute>
          <xsl:attribute name="stylesheet-prefix">metaxsl</xsl:attribute>
        </xsl:element>
      </xsl:comment>

      <xsl:copy>
        <xsl:element name="xsl:param"><xsl:attribute name="name">debug</xsl:attribute>1</xsl:element>
        <xsl:element name="xsl:output">
          <xsl:attribute name="name">xmlv</xsl:attribute>
          <xsl:attribute name="method">xml</xsl:attribute>
          <xsl:attribute name="indent">yes</xsl:attribute>
        </xsl:element>

        <xsl:comment>
          http://bytes.com/topic/xml/answers/774052-removing-attribute-element-xsl:
          <xsl:element name="xsl:import">
            <xsl:attribute name="href">../OLD/xmlresume/xsl:/themes/plain/themes/professional/utils/dyn/dyn.xsl:</xsl:attribute>
          </xsl:element>
          <xsl:element name="xsl:param">
            <xsl:attribute name="include"/>
          </xsl:element>
          <xsl:element name="xsl:param">
            <xsl:attribute name="op"/>
          </xsl:element>
          <xsl:element name="xsl:param">
            <xsl:attribute name="outdir">output</xsl:attribute>
          </xsl:element>
          <xsl:element name="xsl:param">
            <xsl:attribute name="infile">in</xsl:attribute>
          </xsl:element>
        </xsl:comment>

        <xsl:element name="xsl:template">
          <xsl:attribute name="match">/|*</xsl:attribute>
          <xsl:element name="xsl:apply-templates"/>
        </xsl:element>
        <xsl:element name="xsl:template">
          <xsl:attribute name="match">text()|@*</xsl:attribute>
          <xsl:element name="xsl:value-of">
            <xsl:attribute name="select">.</xsl:attribute>
          </xsl:element>
        </xsl:element>
        <xsl:element name="xsl:template">
          <xsl:attribute name="match">@*</xsl:attribute>
        </xsl:element>
        <xsl:element name="xsl:template">
          <xsl:attribute name="mode">x</xsl:attribute>
          <xsl:attribute name="match">@*</xsl:attribute>
        </xsl:element>
        <xsl:element name="xsl:template">
          <xsl:attribute name="match">processing-instruction()|comment()</xsl:attribute>
        </xsl:element>

        <xsl:comment>
          Copy input
        </xsl:comment>

      <xsl:element name="xsl:template">
          <xsl:attribute name="match">*|node()|text()|comment()|processing-instruction()</xsl:attribute>
          <xsl:element name="xsl:copy">

            <xsl:comment>
              Other form will work if elements don't have any attribute, but not otherwise
              Must see http://stackoverflow.com/a/17189003
            </xsl:comment>


            <xsl:element name="xsl:copy-of">
              <xsl:attribute name="select">@*</xsl:attribute>
            </xsl:element>
            <xsl:element name="xsl:apply-templates">
              <xsl:attribute name="select">@* | node() |text()</xsl:attribute>
            </xsl:element>
          </xsl:element>
      </xsl:element>

      <xsl:element name="xsl:template">
          <xsl:attribute name="match">*|node()|text()|comment()|processing-instruction()</xsl:attribute>
          <xsl:attribute name="mode">x</xsl:attribute>
          <xsl:element name="xsl:copy">

            <xsl:comment>
              Other form will work if elements don't have any attribute, but not otherwise
              Must see http://stackoverflow.com/a/17189003
            </xsl:comment>

            <xsl:element name="xsl:copy-of">
              <xsl:attribute name="select">@*</xsl:attribute>
            </xsl:element>
            <xsl:element name="xsl:apply-templates">
              <xsl:attribute name="select">@* | node() |text()</xsl:attribute>
            </xsl:element>
          </xsl:element>
      </xsl:element>

      <xsl:comment>
        Remove previous inline XSL stylesheet
      </xsl:comment>



      <xsl:comment>
        Not working
      </xsl:comment>
      <xsl:element name="xsl:template">
        <xsl:attribute name="match">processing-instruction('xml-stylesheet')[@type='text/xsl']</xsl:attribute>
      </xsl:element>
      <xsl:comment>
        Not working
      </xsl:comment>

      <xsl:element name="xsl:template">
        <xsl:attribute name="match">processing-instruction('xml-stylesheet')</xsl:attribute>
      </xsl:element>




      <xsl:comment>
        Remove previous inline XSL stylesheet
      </xsl:comment>

      <xsl:comment>
        Start: Inline XSL stylesheet
      </xsl:comment>

      <xsl:element name="xsl:template">
        <xsl:attribute name="match">/</xsl:attribute>
        <xsl:value-of select="$newline"/>
        <xsl:choose>
          <xsl:when test="$stylesheetofoutput">
            <xsl:element name="xsl:processing-instruction">
              <xsl:attribute name="name">xml-stylesheet</xsl:attribute>
              <xsl:text>
                href="</xsl:text><xsl:value-of select="$stylesheetofoutput"/><xsl:text>" type="text/xsl"
              </xsl:text>
            </xsl:element>
            <xsl:value-of select="$newline"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:element name="xsl:param">
              <xsl:attribute name="name">stylesheet</xsl:attribute>
            </xsl:element>
            <xsl:value-of select="$newline"/>
            <xsl:element name="xsl:if">
              <xsl:attribute name="test">$stylesheet</xsl:attribute>
              <xsl:element name="xsl:processing-instruction">
                <xsl:attribute name="name">xml-stylesheet</xsl:attribute>
                <xsl:text>
                  href="</xsl:text><xsl:element name="xsl:value-of"><xsl:attribute name="select">$stylesheet</xsl:attribute></xsl:element><xsl:text>" type="text/xsl"
                </xsl:text>
              </xsl:element>
              <xsl:value-of select="$newline"/>
            </xsl:element>
            <xsl:value-of select="$newline"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:element name="xsl:apply-templates"/>
      </xsl:element>

      <xsl:comment>
        Start: Inline XSL stylesheet
      </xsl:comment>

      <xsl:apply-templates/>

    </xsl:copy>
    </xsl:element>


  </xsl:template>


  <xsl:template match="hier">
    <xsl:comment>
      Main Function
    </xsl:comment>

    <xsl:element name="xsl:template">
      <xsl:attribute name="match">*|node()|text()|comment()|processing-instruction()</xsl:attribute>
      <xsl:element name="xsl:if">
        <xsl:attribute name="test">count(<xsl:for-each select="*"><xsl:if test="count(preceding-sibling::*) != 0">|</xsl:if>preceding-sibling::<xsl:value-of select="local-name()"/></xsl:for-each>) = 0</xsl:attribute>

        <xsl:comment>
          Other form will work if elements don't have any attribute, but not otherwise
          Must see http://stackoverflow.com/a/17189003
        </xsl:comment>

        <xsl:element name="xsl:copy">
          <xsl:element name="xsl:copy-of">
            <xsl:attribute name="select">@*</xsl:attribute>
          </xsl:element>
          <xsl:element name="xsl:apply-templates">
            <xsl:attribute name="select">@* | node() |text()</xsl:attribute>
          </xsl:element>
        </xsl:element>

        <!-- <xsl:element name="xsl:copy"> -->
        <!--   <xsl:element name="xsl:apply-templates"/> -->
        <!-- </xsl:element> -->


      </xsl:element>
    </xsl:element>

    <xsl:apply-templates/>
  </xsl:template>


  <xsl:template match="hier/*">

    <xsl:element name="xsl:template">
      <xsl:attribute name="match"><xsl:value-of select="local-name()"/>/text()|<xsl:value-of select="local-name()"/>/node()|<xsl:value-of select="local-name()"/>/*|<xsl:value-of select="local-name()"/>/comment()|<xsl:value-of select="local-name()"/>/processing-instruction()</xsl:attribute>
    </xsl:element>

      <xsl:element name="xsl:template">
        <xsl:attribute name="match"><xsl:value-of select="local-name()"/></xsl:attribute>
        <xsl:choose>
          <xsl:when test="count(preceding-sibling::*) != 0">
            <xsl:element name="xsl:if">
              <xsl:attribute name="test">count(<xsl:for-each select="preceding-sibling::*"><xsl:if test="count(preceding-sibling::*) != 0">|</xsl:if>preceding-sibling::<xsl:value-of select="local-name()"/></xsl:for-each>) = 0</xsl:attribute>
              <xsl:element name="xsl:apply-templates">
                <xsl:attribute name="select">.</xsl:attribute>
                <xsl:attribute name="mode">x</xsl:attribute>
              </xsl:element>
            </xsl:element>
          </xsl:when>
          <xsl:otherwise>
            <xsl:element name="xsl:apply-templates">
              <xsl:attribute name="select">.</xsl:attribute>
              <xsl:attribute name="mode">x</xsl:attribute>
            </xsl:element>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:element>

      <xsl:element name="xsl:template">
          <xsl:attribute name="match"><xsl:value-of select="local-name()"/></xsl:attribute>
          <xsl:attribute name="priority">0</xsl:attribute>
          <xsl:attribute name="mode">x</xsl:attribute>
          <xsl:for-each select=".|following-sibling::*">
            <xsl:element name="xsl:variable">
              <xsl:attribute name="name"><xsl:for-each select=".|preceding-sibling::*"><xsl:if test="count(preceding-sibling::*) != 0">-</xsl:if><xsl:value-of select="local-name()"/></xsl:for-each></xsl:attribute>
              <xsl:attribute name="select">count(<xsl:for-each select=".|preceding-sibling::*"><xsl:if test="count(preceding-sibling::*) != 0">|</xsl:if>following-sibling::<xsl:value-of select="local-name()"/></xsl:for-each>)</xsl:attribute>
            </xsl:element>
          </xsl:for-each>

          <xsl:element name="xsl:copy">

            <xsl:element name="xsl:apply-templates">
                <xsl:attribute name="select">@*</xsl:attribute>
            </xsl:element>

            <xsl:element name="xsl:element">
              <xsl:attribute name="name">tf:heading</xsl:attribute>
              <xsl:element name="xsl:attribute"><xsl:attribute name="name">parent</xsl:attribute><xsl:value-of select="local-name()"/></xsl:element>
              <xsl:element name="xsl:apply-templates">
                <xsl:attribute name="select">*|node()|text()|comment()|processing-instruction()</xsl:attribute>
                <xsl:attribute name="mode">x</xsl:attribute>
              </xsl:element>
            </xsl:element>

            <xsl:element name="xsl:apply-templates">
              <xsl:attribute name="select">following-sibling::*[count(<xsl:for-each select="../*"><xsl:if test="count(preceding-sibling::*) != 0">|</xsl:if>following-sibling::<xsl:value-of select="local-name()"/></xsl:for-each>) = $<xsl:for-each select="../*"><xsl:if test="count(preceding-sibling::*) != 0">-</xsl:if><xsl:value-of select="local-name()"/></xsl:for-each>]</xsl:attribute>
              <xsl:attribute name="mode">x</xsl:attribute>
            </xsl:element>

            <xsl:if test="count(following-sibling::*) != 0">
              <xsl:element name="xsl:apply-templates">
                <xsl:attribute name="select"><xsl:for-each select="following-sibling::*">following-sibling::<xsl:value-of select="local-name()"/>[count(<xsl:for-each select="preceding-sibling::*"><xsl:if test="count(preceding-sibling::*) != 0">|</xsl:if>following-sibling::<xsl:value-of select="local-name()"/></xsl:for-each>) = $<xsl:for-each select="preceding-sibling::*"><xsl:if test="count(preceding-sibling::*) != 0">-</xsl:if><xsl:value-of select="local-name()"/></xsl:for-each>] <xsl:if test="count(following-sibling::*) != 0">|</xsl:if></xsl:for-each></xsl:attribute>
                <xsl:attribute name="mode">x</xsl:attribute>
              </xsl:element>
            </xsl:if>


            <xsl:element name="xsl:apply-templates">
              <xsl:attribute name="select">@*</xsl:attribute>
            </xsl:element>
            <xsl:element name="xsl:apply-templates"/>
          </xsl:element>
      </xsl:element>


  </xsl:template>
</xsl:stylesheet>

