<xsl:stylesheet
    version="1.0"
    xmlns:exsl="http://exslt.org/common"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:m="http://www.merunetworks.com/info/0.0"
    xmlns:ig="http://www.merunetworks.com/namespace/ignore"
    xmlns:mn="http://www.merunetworks.com/namespace/melf"
    xmlns:db="http://docbook.org/ns/docbook"
    xmlns:xl="http://www.w3.org/1999/xlink"
    xmlns:meru="http://www.merunetworks.com/namespace/"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- <xsl:import href="util.xsl" /> -->

  <xsl:param name="debug">0</xsl:param>
  <xsl:param name="stylesheet"/>

  <xsl:param name="noeg">0</xsl:param>

  <xsl:output method="xml" indent="yes"/>
  <!-- <xsl:output -->
  <!--     omit-xml-declaration="yes" -->
  <!--     method="xml" -->
  <!--     media-type="application/xml" -->
  <!--     indent="yes" -->
  <!--     doctype-public="-//OASIS//DTD DocBook XML V4.5//EN"/> -->

  <xsl:variable name='newline'><xsl:text>&#xa;</xsl:text></xsl:variable>
  <!-- <xsl:variable name='missingval'><xsl:text>N/A</xsl:text></xsl:variable> -->
  <!-- <xsl:variable name='missingval'><xsl:text disable-output-escaping="yes">&#160;</xsl:text></xsl:variable> -->
  <xsl:variable name='missingval'><xsl:text disable-output-escaping="yes">N/A</xsl:text></xsl:variable>
  <xsl:variable name="indent" select="0"/>

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
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  <!-- <xsl:apply-templates/> -->
  </xsl:template>

  <!-- ==================================================== -->


  <xsl:param name="html.stylesheet">stalog.css</xsl:param>

  <xsl:template match="db:screen">
    <xsl:if test="$noeg != 1">
      <xsl:copy>
        <xsl:apply-templates select="@*"/>
        <xsl:apply-templates/>
      </xsl:copy>
    </xsl:if>
  </xsl:template>

  <xsl:template match="mn:*">
    <!-- <xsl:apply-templates/> -->
  </xsl:template>

  <xsl:template match="mn:entry">
    <xsl:apply-templates select="mn:instance"/>
  </xsl:template>

  <!-- <xsl:template match="mn:*/text()"> -->
  <!-- </xsl:template> -->

  <!-- <xsl:template match="mn:format/text()|"> -->
  <!--   <xsl:copy> -->
  <!--     <xsl:apply-templates select="@*"/> -->
  <!--     <xsl:apply-templates/> -->
  <!--   </xsl:copy> -->
  <!-- </xsl:template> -->

  <xsl:template match="mn:format">
    <xsl:param name="params"/>

    <!-- mn:params/mn:funparams/mn:param[ mn:name/text() = 'event time' ]/mn:value -->
    <xsl:choose>
      <xsl:when test="exsl:node-set($params)/mn:params/mn:funparams/mn:param[mn:name/text()='event time']/mn:value">
        <xsl:value-of
            select="exsl:node-set($params)/mn:params/mn:funparams/mn:param[mn:name/text()='event time']/mn:value"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of
            select="../mn:params/mn:funparams/mn:param[mn:name/text()='event time']/mn:eg"/>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:text> | </xsl:text>

    <!-- mn:params/mn:funparams/mn:param[ mn:name/text() = 'stamac' ]/mn:value -->
    <xsl:choose>
      <xsl:when test="exsl:node-set($params)/mn:params/mn:funparams/mn:param[mn:name/text()='stamac']/mn:value">
        <xsl:value-of
            select="exsl:node-set($params)/mn:params/mn:funparams/mn:param[mn:name/text()='stamac']/mn:value"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of
            select="../mn:params/mn:funparams/mn:param[mn:name/text()='stamac']/mn:eg"/>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:text> | </xsl:text>

    <!-- mn:params/mn:funparams/mn:param[ mn:name/text() = 'event' ]/mn:description -->
    <xsl:choose>
      <xsl:when test="exsl:node-set($params)/mn:params/mn:funparams/mn:param[mn:name/text()='event']/mn:description">
        <xsl:value-of
            select="exsl:node-set($params)/mn:params/mn:funparams/mn:param[mn:name/text()='event']/mn:description"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of
            select="../mn:params/mn:funparams/mn:param[mn:name/text()='event']/mn:description"/>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:text>	 | </xsl:text>

    <xsl:apply-templates>
      <xsl:with-param  name="params" select="$params"/>
    </xsl:apply-templates>

  </xsl:template><!-- <xsl:template match="mn:format"> -->

  <xsl:template match="mn:format//mn:fmtparam">
    <xsl:param name="params"/>

    <!-- <xsl:message> -->
    <!--   <xsl:copy-of select="$params"/> -->
    <!-- </xsl:message> -->

    <xsl:choose>
      <xsl:when
          test="exsl:node-set($params)/mn:params/mn:fmtparams/mn:param[mn:name/text()=current()/@name]/mn:value
                and
                exsl:node-set($params)/mn:params/mn:fmtparams/mn:param[mn:name/text()=current()/@name]/mn:value != 'TBNL'">
        <xsl:value-of
            select="exsl:node-set($params)/mn:params/mn:fmtparams/mn:param[mn:name/text()=current()/@name]/mn:value"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of
            select="../../mn:params/mn:fmtparams/mn:param[mn:name/text()=current()/@name]/mn:eg"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template><!-- <xsl:template match="mn:format/mn:fmtparam"> -->


  <xsl:template match="mn:instance">
    <xsl:variable name="entry">
      <xsl:choose>
        <xsl:when test="local-name(..) = 'entry'">
          <xsl:copy-of select=".."/>
        </xsl:when>
        <xsl:when test="//mn:entry[@uid=current()/@uidref]">
          <xsl:copy-of select="//mn:entry[@uid=current()/@uidref]"/>
        </xsl:when>
        <xsl:when test="//mn:entry[@oldid=current()/@oldidref]">
          <xsl:copy-of select="//mn:entry[@oldid=current()/@oldidref]"/>
        </xsl:when>
        <xsl:when test="//mn:entry[@id=current()/@entry]">
          <xsl:copy-of select="//mn:entry[@id=current()/@entry]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy-of select="//mn:entry[@entryid=current()/@entryref]"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <db:row>

      <db:entry>
        <!-- <db:screen> -->
        <xsl:apply-templates select="exsl:node-set($entry)/mn:entry/mn:format">
          <xsl:with-param  name="params">
            <xsl:copy-of select="mn:params"/>
          </xsl:with-param>
        </xsl:apply-templates>
        <!-- </db:screen> -->
      </db:entry>

      <!-- Long Description -->
      <db:entry>
        <xsl:choose>
          <xsl:when test="mn:description/mn:long">
            <xsl:value-of select="mn:description/mn:long"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="mn:description/mn:short"/>
          </xsl:otherwise>
        </xsl:choose>
      </db:entry>

      <!-- Trigger -->
      <db:entry>
        <xsl:choose>
          <xsl:when test="mn:trigger">
            <xsl:value-of select="mn:trigger"/>
          </xsl:when>
          <xsl:when test="mn:description/mn:long">
            <xsl:value-of select="mn:description/mn:long"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="mn:description/mn:short"/>
          </xsl:otherwise>
        </xsl:choose>
      </db:entry>

      <db:entry><xsl:value-of select="mn:action"/></db:entry>

      <!-- <db:entry>  -->               <!-- category -->
        <!-- <xsl:value-of select="exsl:node-set($entry)/mn:entry/mn:description/mn:short"/> -->
      <!-- </db:entry> -->
    </db:row>
  </xsl:template>

  <xsl:template match="mn:example">
    <xsl:variable name="entry">
      <xsl:choose>
        <xsl:when test="local-name(..) = 'entry'">
          <xsl:copy-of select=".."/>
        </xsl:when>
        <xsl:when test="//mn:entry[@uid=current()/@uidref]">
          <xsl:copy-of select="//mn:entry[@uid=current()/@uidref]"/>
        </xsl:when>
        <xsl:when test="//mn:entry[@oldid=current()/@oldidref]">
          <xsl:copy-of select="//mn:entry[@oldid=current()/@oldidref]"/>
        </xsl:when>
        <xsl:when test="//mn:entry[@id=current()/@entry]">
          <xsl:copy-of select="//mn:entry[@id=current()/@entry]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy-of select="//mn:entry[@entryid=current()/@entryref]"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:apply-templates select="exsl:node-set($entry)/mn:entry/mn:format">
      <xsl:with-param  name="params">
        <xsl:copy-of select="mn:params"/>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="mn:removecomment/comment()">
  </xsl:template>

  <xsl:template
      match="db:screen/text()[preceding-sibling::ig:ignore[1]][following-sibling::mn:*[1]]">
    <xsl:if test="$debug != 0">
      <xsl:message>matched in screen <xsl:value-of select="string-length(.)"/></xsl:message>
      <xsl:message>matched in screen <xsl:value-of select="string-length(normalize-space(.))"/></xsl:message>
      <xsl:message><xsl:value-of select="'&#xA;'"/></xsl:message>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="string-length(normalize-space(.)) = 0 and string-length(.) &gt; 1">
        <!-- <xsl:value-of select="normalize-space(.)"/> -->
        <xsl:value-of select="'&#xA;'"/>
        <xsl:message>matching</xsl:message>
        <!-- <xsl:value-of select="translate(.,'&#xA;&#xD; ', '&#xA;')"/> -->
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="ig:ignore">
  </xsl:template>


</xsl:stylesheet>
