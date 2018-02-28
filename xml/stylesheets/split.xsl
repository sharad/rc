<xsl:stylesheet
    version="1.0"
    xmlns:exsl="http://exslt.org/common"
    extension-element-prefixes="exsl"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:xi="http://www.w3.org/2003/XInclude"
    xmlns:m="http://www.merunetworks.com/info/0.0"
    xmlns:ig="http://www.merunetworks.com/namespace/ignore"
    xmlns:mn="http://www.merunetworks.com/namespace/melf"
    xmlns:db="http://docbook.org/ns/docbook"
    xmlns:xl="http://www.w3.org/1999/xlink"
    xmlns:meru="http://www.merunetworks.com/namespace/"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- <xsl:import href="util.xsl" /> -->

  <xsl:param name="debug" select="0"/>

  <!-- do not make it 1 -->
  <xsl:param name="regenuid" select="0"/>

  <xsl:param name="stylesheet"/>

  <xsl:param name="melf" select="melf-tmp.xml"/>
  <xsl:param name="stalog" select="stalog-melf-tmp.xml"/>
  <xsl:variable name='space'><xsl:text>&#032;</xsl:text></xsl:variable>
  <xsl:variable name='newline'><xsl:text>&#xa;</xsl:text></xsl:variable>
  <xsl:variable name='doublequote'><xsl:text>&quot;</xsl:text></xsl:variable>
  <xsl:variable name='missingval'><xsl:text disable-output-escaping="yes">N/A</xsl:text></xsl:variable>
  <xsl:variable name="indent" select="0"/>
  <xsl:variable name="smallcase" select="'abcdefghijklmnopqrstuvwxyz'" />
  <xsl:variable name="uppercase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />
  <xsl:variable name="cppinvalidchars" select="')(][:&gt;&lt;-*\%!@$&amp;='" />
  <!-- <xsl:variable name="cppvalidchars" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_ '" /> -->
  <xsl:variable name="cppvalidchars" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_][&lt;&gt;)(&amp; '" />


  <xsl:variable name="counter_instance" select="0"/>
  <xsl:variable name="counter_example" select="0"/>


  <xsl:output method="xml" indent="yes"/>
  <!-- <xsl:output -->
  <!--     omit-xml-declaration="yes" -->
  <!--     method="xml" -->
  <!--     media-type="application/xml" -->
  <!--     indent="yes" -->
  <!--     doctype-public="-//OASIS//DTD DocBook XML V4.5//EN"/> -->




  <xsl:template match="@*|*|processing-instruction()|comment()">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()|processing-instruction()|comment()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="@*|*|processing-instruction()" mode="nocomment">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()|processing-instruction()"/>
    </xsl:copy>
  </xsl:template>






  <xsl:template match="@*|*|mn:*|processing-instruction()|comment()" mode="identity">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()|processing-instruction()|comment()" mode="identity"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="@*|node()" mode="ref">
    <xsl:copy>
      <xsl:apply-templates select="@*" mode="ref"/>
      <xsl:apply-templates select="node()" mode="ref"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="mn:entry" mode="ref">
    <xsl:copy>
      <!-- <xsl:attribute name="uidref"> -->
      <!--   <xsl:value-of select="concat('entry_', count(preceding::mn:entry))"/> -->
      <!-- </xsl:attribute> -->
      <xsl:apply-templates mode="ref"/>
    </xsl:copy>
    <!-- <xsl:copy-of select="."/> -->
  </xsl:template>

  <xsl:template match="db:*|db:*/text()" mode="melf">
    <xsl:apply-templates mode="melf"/>
  </xsl:template>

  <xsl:template match="mn:*|mn:*/@*" mode="melf">
    <xsl:copy>
      <xsl:apply-templates select="@*" mode="melf"/>
      <xsl:apply-templates select="mn:*|text()|comment()" mode="melf"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="mn:*/text()" mode="melf">
    <xsl:copy>
      <xsl:apply-templates select="@*|." mode="melf"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="mn:*/comment()" mode="melf">
    <xsl:copy>
      <xsl:apply-templates mode="melf"/>
    </xsl:copy>
  </xsl:template>





  <xsl:template match="/">
    <exsl:document href="{$melf}" method="xml" indent="yes">
      <xsl:element name="mn:melf">
        <!-- <xsl:apply-templates -->
        <!--     select="//mn:entry|//mn:instance|//mn:example" -->
        <!--     mode="melf"/> -->
        <xsl:apply-templates mode="melf"/>
      </xsl:element>
    </exsl:document>

    <exsl:document href="{$stalog}" method="xml" indent="yes">
      <xsl:apply-templates mode="stalog">
        <!-- <xsl:with-param name="xpointer" select="boolean('true')"/> -->
        <xsl:with-param name="xpointer" select="1"/>
      </xsl:apply-templates>
    </exsl:document>
  </xsl:template>







  <!-- stalog-melf.xml -->
  <xsl:template match="@*|db:*|processing-instruction()|comment()|text()" mode="stalog">
    <xsl:param name="xpointer"/>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="*|text()" mode="stalog">
        <!-- <xsl:with-param name="xpointer" select="boolean('true')"/> -->
        <xsl:with-param name="xpointer" select="1"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="ig:ignore|ig:ignore/db:*|ig:*/*"/>
  <xsl:template match="ig:ignore|ig:ignore/db:*|ig:*/*"  mode="stalog"/>


  <xsl:template match="mn:entry" mode="stalog">
    <xsl:param name="xpointer"/>

    <xsl:for-each select="mn:instance|mn:example">
      <!-- to insert [in print] xml comment, only while include-xpointer of -->
      <!-- example and instance will not be added as xpointer is passed as 0 -->
      <xsl:apply-templates select="." mode="stalog">
        <!-- <xsl:with-param name="xpointer" select="boolean('false')"/> -->
        <xsl:with-param name="xpointer" select="0"/>
      </xsl:apply-templates>
    </xsl:for-each>

    <xsl:element name="xi:include">
      <xsl:attribute name="href">
        <xsl:value-of select="$melf"/>
      </xsl:attribute>

      <xsl:attribute name="xpointer">
        <xsl:text>xmlns(mn=http://www.merunetworks.com/namespace/melf)xpointer(/mn:melf//mn:entry[@uid='</xsl:text>
        <xsl:value-of select="concat('entry_uid_', count(preceding::mn:entry))"/>
        <xsl:text>'])</xsl:text>
      </xsl:attribute>
    </xsl:element>

    <xsl:value-of select="$newline"/>

  </xsl:template>

  <xsl:template match="mn:instance" mode="stalog">
    <xsl:param name="xpointer"/>

    <xsl:variable name="entry">
      <xsl:choose>
        <xsl:when test="local-name(..) = 'entry'">
          <xsl:copy-of select=".."/>
        </xsl:when>
        <xsl:when test="//mn:entry[@uid and @uid=current()/@uidref]">
          <xsl:copy-of select="//mn:entry[@uid=current()/@uidref]"/>
        </xsl:when>
        <xsl:when test="//mn:entry[@oldid and @oldid=current()/@oldidref]">
          <xsl:copy-of select="//mn:entry[@oldid=current()/@oldidref]"/>
        </xsl:when>
        <xsl:when test="//mn:entry[@id and @id=current()/@entry]">
          <xsl:copy-of select="//mn:entry[@id=current()/@entry]"/>
        </xsl:when>
        <xsl:when test="//mn:entry[@entryid and @entryid=current()/@entryref]">
          <xsl:copy-of select="//mn:entry[@entryid=current()/@entryref]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy-of select="//mn:entry[@uid and @uid=current()/@uidref]"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>


    <xsl:value-of select="$newline"/>
    <xsl:element name="ig:ignore">
      <!-- <xsl:text disable-output-escaping="yes">[in docboot] &lt;![CDATA[  </xsl:text> -->

      <xsl:value-of select="$newline"/>
      <xsl:comment>
        <xsl:text disable-output-escaping="yes"> [in print] &lt;![CDATA[  </xsl:text>
        <xsl:apply-templates select="exsl:node-set($entry)/mn:entry/mn:format"
                             mode="VALUE_FULL">
          <xsl:with-param  name="params">
            <xsl:copy-of select="mn:params"/>
          </xsl:with-param>
        </xsl:apply-templates>
        <xsl:text disable-output-escaping="yes">  ]]&gt; </xsl:text>
      </xsl:comment>
      <xsl:value-of select="$newline"/>

      <db:row>
        <db:entry>
          <!-- <db:screen> -->
          <xsl:apply-templates
              select="exsl:node-set($entry)/mn:entry/mn:format"
              mode="VALUE_FULL">
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
      </db:row>
      <!-- <xsl:text disable-output-escaping="yes">  ]]&gt;</xsl:text> -->
      <xsl:value-of select="$newline"/>
    </xsl:element>

    <xsl:value-of select="$newline"/>



    <!-- <xsl:if test="$xpointer"> -->
    <xsl:if test="$xpointer = 1">
      <xsl:element name="xi:include">
        <xsl:attribute name="href">
          <xsl:value-of select="$melf"/>
        </xsl:attribute>
        <xsl:attribute name="xpointer">
          <xsl:text>xmlns(mn=http://www.merunetworks.com/namespace/melf)xpointer(/mn:melf//mn:instance[@uid='</xsl:text>
          <xsl:value-of select="concat('instance_uid_', count(preceding::mn:instance))"/>
          <xsl:text>'])</xsl:text>
        </xsl:attribute>
      </xsl:element>
      <xsl:value-of select="$newline"/>

    </xsl:if>
  </xsl:template>

  <xsl:template match="mn:example" mode="stalog">
    <xsl:param name="xpointer"/>
    <xsl:variable name="entry">
      <xsl:choose>
        <xsl:when test="local-name(..) = 'entry'">
          <xsl:copy-of select=".."/>
        </xsl:when>
        <xsl:when test="//mn:entry[@uid and @uid=current()/@uidref]">
          <xsl:copy-of select="//mn:entry[@uid=current()/@uidref]"/>
        </xsl:when>
        <xsl:when test="//mn:entry[@oldid and @oldid=current()/@entry]">
          <xsl:copy-of select="//mn:entry[@oldid=current()/@entry]"/>
        </xsl:when>
        <xsl:when test="//mn:entry[@id and @id=current()/@entry]">
          <xsl:copy-of select="//mn:entry[@id=current()/@entry]"/>
        </xsl:when>
        <xsl:when test="//mn:entry[@entryid and @entryid=current()/@entryref]">
          <xsl:copy-of select="//mn:entry[@entryid=current()/@entryref]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy-of select="//mn:entry[@uid and @uid=current()/@uidref]"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:element name="ig:ignore">
      <xsl:value-of select="$newline"/>
      <xsl:comment>
        <xsl:text disable-output-escaping="yes"> [in print] &lt;![CDATA[  </xsl:text>
        <xsl:apply-templates
            select="exsl:node-set($entry)/mn:entry/mn:format"
            mode="VALUE_FULL">
          <xsl:with-param  name="params">
            <xsl:copy-of select="mn:params"/>
          </xsl:with-param>
        </xsl:apply-templates>
        <xsl:text disable-output-escaping="yes">  ]]&gt; </xsl:text>
      </xsl:comment>
      <xsl:value-of select="$newline"/>
    </xsl:element>
    <xsl:value-of select="$newline"/>
    <xsl:if test="$xpointer">
      <xsl:element name="xi:include">
        <xsl:attribute name="href">
          <xsl:value-of select="$melf"/>
        </xsl:attribute>
        <xsl:attribute name="xpointer">
          <xsl:text>xmlns(mn=http://www.merunetworks.com/namespace/melf)xpointer(/mn:melf//mn:example[@uid='</xsl:text>
          <xsl:value-of select="concat('example_uid_', count(preceding::mn:example))"/>
          <xsl:text>'])</xsl:text>
        </xsl:attribute>
      </xsl:element>
    </xsl:if>
  </xsl:template>



  <xsl:template
      match="db:screen/text()[preceding-sibling::ig:ignore[1]][following-sibling::mn:*[1]]"
      mode="stalog">
    <xsl:choose>
      <xsl:when test="string-length(normalize-space(.)) = 0">
        <xsl:value-of select="normalize-space(.)"/>
        <!-- <xsl:value-of select="translate(.,'&#xA;&#xD;', '  ')"/>
        -->
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="$debug != 0">
      <xsl:message>matched in screen <xsl:value-of select="string-length(normalize-space(.))"/></xsl:message>
    </xsl:if>
  </xsl:template>

  <xsl:template
      match="*/text()[preceding-sibling::ig:ignore[1]][following-sibling::mn:*[1]]"
      mode="stalog">
    <xsl:choose>
      <xsl:when test="string-length(normalize-space(.)) = 0">
        <xsl:value-of select="normalize-space(.)"/>
        <!-- <xsl:value-of select="translate(.,'&#xA;&#xD;', '  ')"/>
        -->
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="$debug != 0">
      <xsl:message>matched in other place <xsl:value-of select="string-length(normalize-space(.))"/></xsl:message>
    </xsl:if>
  </xsl:template>

  <!-- stalog-melf.xml -->






  <!-- melf.xml -->
  <xsl:template match="mn:entry" mode="melf">
    <xsl:copy>
      <!-- <xsl:apply-templates select="@*|node()"/> -->
      <xsl:if test="$regenuid = 1">
        <xsl:if test="1 or not(@uid)">
          <xsl:attribute name="uid">
            <xsl:value-of select="concat('entry_uid_', count(preceding::mn:entry))"/>
          </xsl:attribute>
        </xsl:if>
      </xsl:if>
      <xsl:attribute name="orderid">
        <xsl:value-of select="concat('entry_orderid_', count(preceding::mn:entry))"/>
      </xsl:attribute>
      <xsl:attribute name="fmtid">
        <xsl:apply-templates select="./mn:format" mode="CPP"/>
      </xsl:attribute>
      <xsl:if test="not(@oldid)">
        <xsl:if test="@oldid or @entryid or @id">
          <xsl:attribute name="oldid">
            <xsl:choose>
              <xsl:when test="@id">
                <xsl:value-of select="@id"/>
              </xsl:when>
              <xsl:when test="@entryid">
                <xsl:value-of select="@entryid"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@oldid"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:attribute>
        </xsl:if>
      </xsl:if>
      <xsl:apply-templates select="@*[local-name() != 'entry' and
                                   local-name() != 'entryid' and
                                   local-name() != 'fmtid' and
                                   local-name() != 'orderid' and
                                   ($regenuid != 1 or local-name() != 'uid')
                                   and
                                   local-name() != 'entryref' and
                                   local-name() != 'entryidref' and
                                   local-name() != 'fmtref' and
                                   local-name() != 'orderidref' and
                                   local-name() != 'oldidref' and
                                   ($regenuid != 1 or local-name() != 'uidref') and
                                   1]|node()"
                           mode="melf"/>
    </xsl:copy>
  </xsl:template>


  <xsl:template match="mn:entry/mn:instance|mn:instance" mode="melf">

    <xsl:variable name="entryref">
      <xsl:copy>
        <xsl:choose>
          <xsl:when test="local-name(..) = 'entry'">
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_', count(../preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_', count(../preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select=".." mode="identity"/>
          </xsl:when>

          <xsl:when test="//mn:entry[@oldid and @oldid=current()/@oldidref]">
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_',
                                      count(//mn:entry[@oldid=current()/@oldidref]/preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_',
                                    count(//mn:entry[@oldid=current()/@oldidref]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select="//mn:entry[@oldid=current()/@oldidref]" mode="identity"/>
          </xsl:when>

          <xsl:when test="//mn:entry[@oldid and @oldid=current()/@entryref]">
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_',
                                      count(//mn:entry[@oldid=current()/@entryref]/preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_',
                                    count(//mn:entry[@oldid=current()/@entryref]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select="//mn:entry[@oldid=current()/@entryref]" mode="identity"/>
          </xsl:when>


          <xsl:when test="//mn:entry[@oldid and @oldid=current()/@entry]">
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_',
                                      count(//mn:entry[@oldid=current()/@entry]/preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_',
                                    count(//mn:entry[@oldid=current()/@entry]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select="//mn:entry[@oldid=current()/@entry]" mode="identity"/>
          </xsl:when>

          <xsl:when test="//mn:entry[@id and @id=current()/@entry]">
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_',
                                      count(//mn:entry[@id=current()/@entry]/preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_',
                                    count(//mn:entry[@id=current()/@entry]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select="//mn:entry[@id=current()/@entry]" mode="identity"/>
          </xsl:when>

          <xsl:when test="//mn:entry[@uid and @uid=current()/@uidref]">
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_',
                                      count(//mn:entry[@uid and @uid=current()/@uidref]/preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_',
                                    count(//mn:entry[@uid and @uid=current()/@uidref]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select="//mn:entry[@uid=current()/@uidref]" mode="identity"/>
          </xsl:when>

          <xsl:when test="//mn:entry[@entryid and @entryid=current()/@entryref]">
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_',
                                      count(//mn:entry[@entryid and @entryid=current()/@entryref]/preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_',
                                    count(//mn:entry[@entryid and @entryid=current()/@entryref]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select="//mn:entry[@entryid=current()/@entryref]" mode="identity"/>
          </xsl:when>

          <xsl:otherwise>
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_',
                                      count(//mn:entry[@uid and @uid=current()/@uidref]/preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_',
                                    count(//mn:entry[@uid and @uid=current()/@uidref]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates
                select="//mn:entry[@uid and @uid=current()/@uidref]" mode="identity"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:copy>
    </xsl:variable>





    <xsl:copy>

      <xsl:if test="$regenuid = 1">
        <xsl:if test="1 or not(@uid)">
          <xsl:attribute name="uid">
            <xsl:value-of select="concat('instance_uid_', count(preceding::mn:instance))"/>
          </xsl:attribute>
          <xsl:attribute name="uidref">
            <xsl:value-of select="exsl:node-set($entryref)/mn:instance/@uidref"/>
          </xsl:attribute>
        </xsl:if>
      </xsl:if>

      <xsl:attribute name="orderid">
        <xsl:value-of select="concat('instance_orderid_', count(preceding::mn:instance))"/>
      </xsl:attribute>

      <xsl:attribute name="orderidref">
        <xsl:value-of select="exsl:node-set($entryref)/mn:instance/@orderidref"/>
      </xsl:attribute>

      <xsl:if test="$debug = 1">
        <xsl:message>
          <xsl:value-of select="exsl:node-set($entryref)"/>
        </xsl:message>
      </xsl:if>

      <xsl:attribute name="fmtid">
        <xsl:text>Ref </xsl:text>
        <xsl:apply-templates
            select="exsl:node-set($entryref)/mn:instance/mn:entry/mn:format"
            mode="CPP"/>
      </xsl:attribute>

      <xsl:attribute name="fmtref">
        <xsl:apply-templates
            select="exsl:node-set($entryref)/mn:instance/mn:entry/mn:format"
            mode="CPP"/>
      </xsl:attribute>

      <xsl:if test="not(@oldid)">
        <xsl:if test="@oldid or @entryid or @id">
          <xsl:attribute name="oldid">
            <xsl:choose>
              <xsl:when test="@id">
                <xsl:value-of select="@id"/>
              </xsl:when>
              <xsl:when test="@entryid">
                <xsl:value-of select="@entryid"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@oldid"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:attribute>
        </xsl:if>
      </xsl:if>

      <xsl:if test="not(@oldidref)">
        <xsl:if test="@oldidref or
                      @entry or
                      @entryref or
                      exsl:node-set($entryref)/mn:instance/mn:entry/@entryid or
                      exsl:node-set($entryref)/mn:instance/mn:entry/@id or
                      exsl:node-set($entryref)/mn:instance/mn:entry/@oldid">
          <xsl:attribute name="oldidref">
            <xsl:choose>
              <xsl:when test="@oldidref">
                <xsl:value-of select="@oldidref"/>
              </xsl:when>
              <xsl:when test="@entry">
                <xsl:value-of select="@entry"/>
              </xsl:when>
              <xsl:when test="@entryref">
                <xsl:value-of select="@entryref"/>
              </xsl:when>
              <xsl:when
                  test="exsl:node-set($entryref)/mn:instance/mn:entry/@entryid">
                <xsl:value-of select="exsl:node-set($entryref)/mn:instance/mn:entry/@entryid"/>
              </xsl:when>
              <xsl:when test="exsl:node-set($entryref)/mn:instance/mn:entry/@id">
                <xsl:value-of select="exsl:node-set($entryref)/mn:instance/mn:entry/@id"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="exsl:node-set($entryref)/mn:instance/mn:entry/@oldid"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:attribute>
        </xsl:if>
      </xsl:if>

      <xsl:apply-templates select="@*[local-name() != 'entry' and
                                   local-name() != 'entryid' and
                                   local-name() != 'fmtid' and
                                   local-name() != 'orderid' and
                                   ($regenuid != 1 or local-name() != 'uid')
                                   and
                                   local-name() != 'entryref' and
                                   local-name() != 'entryidref' and
                                   local-name() != 'fmtref' and
                                   local-name() != 'orderidref' and
                                   ($regenuid != 1 or local-name() != 'uidref') and
                                   1]|node()"
                           mode="melf"/>
    </xsl:copy>
  </xsl:template> <!-- <xsl:template match="mn:entry/mn:instance|mn:instance" mode="melf"> -->



  <xsl:template match="mn:entry/mn:example|mn:example" mode="melf">

    <xsl:variable name="entryref">
      <xsl:copy>
        <xsl:choose>
          <xsl:when test="local-name(..) = 'entry'">
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_', count(../preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_', count(../preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select=".." mode="identity"/>
          </xsl:when>

          <xsl:when test="//mn:entry[@oldid and @oldid=current()/@oldidref]">
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_',
                                      count(//mn:entry[@oldid=current()/@oldidref]/preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_',
                                    count(//mn:entry[@oldid=current()/@oldidref]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select="//mn:entry[@oldid=current()/@oldidref]" mode="identity"/>
          </xsl:when>

          <xsl:when test="//mn:entry[@oldid and @oldid=current()/@entryref]">
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_',
                                      count(//mn:entry[@oldid=current()/@entryref]/preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_',
                                    count(//mn:entry[@oldid=current()/@entryref]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select="//mn:entry[@oldid=current()/@entryref]" mode="identity"/>
          </xsl:when>


          <xsl:when test="//mn:entry[@oldid and @oldid=current()/@entry]">
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_',
                                      count(//mn:entry[@oldid=current()/@entry]/preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_',
                                    count(//mn:entry[@oldid=current()/@entry]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select="//mn:entry[@oldid=current()/@entry]" mode="identity"/>
          </xsl:when>

          <xsl:when test="//mn:entry[@id and @id=current()/@entry]">
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_',
                                      count(//mn:entry[@id=current()/@entry]/preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_',
                                    count(//mn:entry[@id=current()/@entry]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select="//mn:entry[@id=current()/@entry]" mode="identity"/>
          </xsl:when>

          <xsl:when test="//mn:entry[@uid and @uid=current()/@uidref]">
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_',
                                      count(//mn:entry[@uid and @uid=current()/@uidref]/preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_',
                                    count(//mn:entry[@uid and @uid=current()/@uidref]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select="//mn:entry[@uid=current()/@uidref]" mode="identity"/>
          </xsl:when>

          <xsl:when test="//mn:entry[@entryid and @entryid=current()/@entryref]">
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_',
                                      count(//mn:entry[@entryid and @entryid=current()/@entryref]/preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_',
                                    count(//mn:entry[@entryid and @entryid=current()/@entryref]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select="//mn:entry[@entryid=current()/@entryref]" mode="identity"/>
          </xsl:when>

          <xsl:otherwise>
            <xsl:if test="$regenuid = 1">
              <xsl:attribute name="uidref">
                <xsl:value-of select="concat('entry_uid_',
                                      count(//mn:entry[@uid and @uid=current()/@uidref]/preceding::mn:entry))"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:attribute name="orderidref">
              <xsl:value-of select="concat('entry_orderid_',
                                    count(//mn:entry[@uid and @uid=current()/@uidref]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates
                select="//mn:entry[@uid and @uid=current()/@uidref]" mode="identity"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:copy>
    </xsl:variable>


    <xsl:copy>
      <xsl:if test="$regenuid = 1">
        <xsl:if test="1 or not(@uid)">
          <xsl:attribute name="uid">
            <xsl:value-of select="concat('example_uid_', count(preceding::mn:example))"/>
          </xsl:attribute>
          <xsl:attribute name="uidref">
            <xsl:value-of select="exsl:node-set($entryref)/mn:example/@uidref"/>
          </xsl:attribute>
        </xsl:if>
      </xsl:if>

      <xsl:attribute name="orderid">
        <xsl:value-of select="concat('example_orderid_', count(preceding::mn:example))"/>
      </xsl:attribute>

      <xsl:attribute name="orderidref">
        <xsl:value-of select="exsl:node-set($entryref)/mn:example/@orderidref"/>
      </xsl:attribute>

      <xsl:if test="$debug = 1">
        <xsl:message>
          <xsl:value-of select="exsl:node-set($entryref)"/>
        </xsl:message>
      </xsl:if>

      <xsl:attribute name="fmtid">
        <xsl:text>Ref </xsl:text>
        <xsl:apply-templates
            select="exsl:node-set($entryref)/mn:example/mn:entry/mn:format"
            mode="CPP"/>
      </xsl:attribute>

      <xsl:attribute name="fmtref">
        <xsl:apply-templates
            select="exsl:node-set($entryref)/mn:example/mn:entry/mn:format"
            mode="CPP"/>
      </xsl:attribute>

      <xsl:if test="not(@oldid)">
        <xsl:if test="@oldid or @entryid or @id">
          <xsl:attribute name="oldid">
            <xsl:choose>
              <xsl:when test="@id">
                <xsl:value-of select="@id"/>
              </xsl:when>
              <xsl:when test="@entryid">
                <xsl:value-of select="@entryid"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@oldid"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:attribute>
        </xsl:if>
      </xsl:if>

      <xsl:if test="not(@oldidref)">
        <xsl:if test="@oldidref or
                      @entry or
                      @entryref or
                      exsl:node-set($entryref)/mn:example/mn:entry/@entryid or
                      exsl:node-set($entryref)/mn:example/mn:entry/@id or
                      exsl:node-set($entryref)/mn:example/mn:entry/@oldid">
          <xsl:attribute name="oldidref">
            <xsl:choose>
              <xsl:when test="@oldidref">
                <xsl:value-of select="@oldidref"/>
              </xsl:when>
              <xsl:when test="@entry">
                <xsl:value-of select="@entry"/>
              </xsl:when>
              <xsl:when test="@entryref">
                <xsl:value-of select="@entryref"/>
              </xsl:when>
              <xsl:when
                  test="exsl:node-set($entryref)/mn:example/mn:entry/@entryid">
                <xsl:value-of select="exsl:node-set($entryref)/mn:example/mn:entry/@entryid"/>
              </xsl:when>
              <xsl:when test="exsl:node-set($entryref)/mn:example/mn:entry/@id">
                <xsl:value-of select="exsl:node-set($entryref)/mn:example/mn:entry/@id"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="exsl:node-set($entryref)/mn:example/mn:entry/@oldid"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:attribute>
        </xsl:if>
      </xsl:if>

      <xsl:apply-templates select="@*[local-name() != 'entry' and
                                   local-name() != 'entryid' and
                                   local-name() != 'fmtid' and
                                   local-name() != 'orderid' and
                                   ($regenuid != 1 or local-name() != 'uid')
                                   and
                                   local-name() != 'entryref' and
                                   local-name() != 'entryidref' and
                                   local-name() != 'fmtref' and
                                   local-name() != 'orderidref' and
                                   ($regenuid != 1 or local-name() != 'uidref') and
                                   1]|node()"
                           mode="melf"/>
    </xsl:copy>
  </xsl:template> <!-- <xsl:template match="mn:entry/mn:example|mn:example" mode="melf"> -->
  <!-- melf.xml -->

  <!-- mode: CPP -->
  <xsl:template match="mn:format/text()|" mode="CPP">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="mn:format" mode="CPP">
    <!-- http://stackoverflow.com/questions/7635593/how-to-do-a-second-transform-on-the-output-of-an-xslt-template -->
    <!-- http://stackoverflow.com/questions/5084065/replace-special-characters-in-xslt -->
    <xsl:param name="params"/>
    <xsl:variable name="output">
      <xsl:apply-templates mode="CPP">
        <xsl:with-param  name="params" select="$params"/>
      </xsl:apply-templates>
    </xsl:variable>

    <xsl:value-of select="$output"/>
  </xsl:template>

  <xsl:template match="mn:format/mn:fmtparam" mode="CPP">
    <xsl:param name="params"/>
    <xsl:choose>
      <xsl:when test="exsl:node-set($params)/mn:params/mn:fmtparams/mn:param[mn:name/text()=current()/@name]/mn:name">
        <xsl:value-of
            select="exsl:node-set($params)/mn:params/mn:fmtparams/mn:param[mn:name/text()=current()/@name]/mn:name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of
            select="../../mn:params/mn:fmtparams/mn:param[mn:name/text()=current()/@name]/mn:name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- mode: CPP -->


  <!-- mode: VALUE -->
  <xsl:template match="mn:format/text()|" mode="VALUE">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="mn:format" mode="VALUE_FULL">
    <!-- http://stackoverflow.com/questions/7635593/how-to-do-a-second-transform-on-the-output-of-an-xslt-template -->
    <!-- http://stackoverflow.com/questions/5084065/replace-special-characters-in-xslt -->
    <xsl:param name="params"/>
    <xsl:variable name="output">
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

      <xsl:apply-templates mode="VALUE">
        <xsl:with-param  name="params" select="$params"/>
      </xsl:apply-templates>
    </xsl:variable>

    <xsl:value-of select="$output"/>
  </xsl:template>

  <xsl:template match="mn:format" mode="VALUE">
    <!-- http://stackoverflow.com/questions/7635593/how-to-do-a-second-transform-on-the-output-of-an-xslt-template -->
    <!-- http://stackoverflow.com/questions/5084065/replace-special-characters-in-xslt -->
    <xsl:param name="params"/>
    <xsl:variable name="output">
      <xsl:apply-templates mode="VALUE">
        <xsl:with-param  name="params" select="$params"/>
      </xsl:apply-templates>
    </xsl:variable>

    <xsl:value-of select="$output"/>
  </xsl:template>

  <xsl:template match="mn:format/mn:fmtparam" mode="VALUE">
    <xsl:param name="params"/>
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
  <!-- mode: VALUE -->

</xsl:stylesheet>
