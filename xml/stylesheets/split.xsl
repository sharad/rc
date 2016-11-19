<xsl:stylesheet
    version="1.0"
    xmlns:exsl="http://exslt.org/common"
    extension-element-prefixes="exsl"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:xi="http://www.w3.org/2003/XInclude"
    xmlns:m="http://www.merunetworks.com/info/0.0"
    xmlns:mn="http://www.merunetworks.com/namespace/melf"
    xmlns:db="http://docbook.org/ns/docbook"
    xmlns:xl="http://www.w3.org/1999/xlink"
    xmlns:meru="http://www.merunetworks.com/namespace/"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- <xsl:import href="util.xsl" /> -->

  <xsl:param name="debug">1</xsl:param>
  <xsl:param name="stylesheet"/>

  <xsl:param name="melf" select="melf1.xml"/>
  <xsl:param name="stalog" select="stalog-melf1.xml"/>
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
  <xsl:template match="@*|db:*|processing-instruction()|comment()" mode="stalog">
    <xsl:param name="xpointer"/>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates
          mode="stalog">
        <xsl:with-param name="xpointer" select="boolean('true')"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>


  <xsl:template match="mn:entry" mode="stalog">
    <xsl:param name="xpointer"/>

    <!-- <xsl:for-each select="mn:instance|mn:example"> -->
    <!--   <xsl:value-of select="$newline"/> -->
    <!--   <xsl:comment> -->
    <!--     <xsl:text disable-output-escaping="yes">[in print] &lt;![CDATA[  </xsl:text> -->
    <!--     <xsl:apply-templates select="../mn:format" mode="VALUE"> -->
    <!--     <xsl:with-param  name="params"> -->
    <!--       <xsl:copy-of select="mn:params"/> -->
    <!--     </xsl:with-param> -->
    <!--   </xsl:apply-templates> -->
    <!--   <xsl:text disable-output-escaping="yes">  ]]&gt;</xsl:text> -->
    <!--   </xsl:comment> -->
    <!--   <xsl:value-of select="$newline"/> -->
    <!-- </xsl:for-each> -->

    <xsl:for-each select="mn:instance|mn:example">
      <xsl:apply-templates select="." mode="stalog">
        <!-- <xsl:with-param name="xpointer" select="boolean('false')"/> -->
        <xsl:with-param name="xpointer" select="0"/>
      </xsl:apply-templates>
    </xsl:for-each>

    <xsl:element name="xi:include">
      <xsl:attribute name="href">
        <xsl:text>melf.xml</xsl:text>
      </xsl:attribute>
      <!-- <xsl:attribute name="xpointer"> -->
      <!--   <xsl:text>xmlns(mn=http://www.merunetworks.com/namespace/melf)xpointer(/mn:melf//mn:entry[@id='</xsl:text> -->
      <!--   <xsl:apply-templates select="./mn:format" mode="CPP"/> -->
      <!--   <xsl:text>'])</xsl:text> -->
      <!-- </xsl:attribute> -->
      <xsl:attribute name="xpointer">
        <xsl:text>xmlns(mn=http://www.merunetworks.com/namespace/melf)xpointer(/mn:melf//mn:entry[@uid='</xsl:text>
        <xsl:value-of select="concat('entry_', count(preceding::mn:entry))"/>
        <xsl:text>'])</xsl:text>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>

  <xsl:template match="mn:instance" mode="stalog">
    <xsl:param name="xpointer"/>

    <xsl:variable name="entry">
      <xsl:choose>
        <xsl:when test="local-name(..) = 'entry'">
          <xsl:copy-of select=".."/>
        </xsl:when>
        <xsl:when test="//mn:entry[@uid=current()/@uidref]">
          <xsl:copy-of select="//mn:entry[@uid=current()/@uidref]"/>
        </xsl:when>
        <xsl:when test="//mn:entry[@id=current()/@entry]">
          <xsl:copy-of select="//mn:entry[@id=current()/@entry]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy-of select="//mn:entry[@uid=current()/@uidref or @entryid=current()/@entryref]"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:value-of select="$newline"/>
    <xsl:comment>
      <xsl:text disable-output-escaping="yes">[in print] &lt;![CDATA[  </xsl:text>
      <xsl:apply-templates
          select="exsl:node-set($entry)/mn:entry/mn:format"
          mode="VALUE">
        <xsl:with-param  name="params">
          <xsl:copy-of select="mn:params"/>
        </xsl:with-param>
      </xsl:apply-templates>
      <xsl:text disable-output-escaping="yes">  ]]&gt; </xsl:text>
    </xsl:comment>
    <xsl:value-of select="$newline"/>


    <xsl:value-of select="$newline"/>
    <xsl:element name="ignore">
      <!-- <xsl:text disable-output-escaping="yes">[in docboot] &lt;![CDATA[  </xsl:text> -->
      <db:row>
        <db:entry>
          <!-- <db:screen> -->
          <xsl:apply-templates
              select="exsl:node-set($entry)/mn:entry/mn:format"
              mode="VALUE">
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
    </xsl:element>
    <xsl:value-of select="$newline"/>



    <!-- <xsl:if test="$xpointer"> -->
    <xsl:if test="$xpointer = 1">

      <xsl:value-of select="$newline"/>

      <xsl:element name="xi:include">
        <xsl:attribute name="href">
          <xsl:text>melf.xml</xsl:text>
        </xsl:attribute>
        <!-- <xsl:attribute name="xpointer"> -->
        <!--   <xsl:text>xmlns(mn=http://www.merunetworks.com/namespace/melf)xpointer(/mn:melf//mn:instance[@id='</xsl:text> -->
        <!--   <xsl:apply-templates select="exsl:node-set($entry)/mn:entry/mn:format" mode="CPP"/> -->
        <!--   <xsl:text>'])</xsl:text> -->
        <!-- </xsl:attribute> -->
        <xsl:attribute name="xpointer">
          <xsl:text>xmlns(mn=http://www.merunetworks.com/namespace/melf)xpointer(/mn:melf//mn:instance[@uid='</xsl:text>
          <xsl:value-of select="concat('instance_', count(preceding::mn:instance))"/>
          <xsl:text>'])</xsl:text>
        </xsl:attribute>
      </xsl:element>

    </xsl:if>

  </xsl:template>

  <xsl:template match="mn:example" mode="stalog">
    <xsl:param name="xpointer"/>
    <xsl:variable name="entry">
      <xsl:choose>
        <xsl:when test="local-name(..) = 'entry'">
          <xsl:copy-of select=".."/>
        </xsl:when>
        <xsl:when test="//mn:entry[@uid=current()/@uidref]">
          <xsl:copy-of select="//mn:entry[@uid=current()/@uidref]"/>
        </xsl:when>
        <xsl:when test="//mn:entry[@id=current()/@entry]">
          <xsl:copy-of select="//mn:entry[@id=current()/@entry]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy-of select="//mn:entry[@uid=current()/@uidref or @entryid=current()/@entryref]"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:value-of select="$newline"/>
    <xsl:comment>
      <xsl:text disable-output-escaping="yes">[in print] &lt;![CDATA[  </xsl:text>
      <xsl:apply-templates
          select="exsl:node-set($entry)/mn:entry/mn:format"
          mode="VALUE">
        <xsl:with-param  name="params">
          <xsl:copy-of select="mn:params"/>
        </xsl:with-param>
      </xsl:apply-templates>
      <xsl:text disable-output-escaping="yes">  ]]&gt;</xsl:text>
    </xsl:comment>
    <xsl:value-of select="$newline"/>

    <xsl:if test="$xpointer">
      <xsl:element name="xi:include">
        <xsl:attribute name="href">
          <xsl:text>melf.xml</xsl:text>
        </xsl:attribute>
        <!-- <xsl:attribute name="xpointer"> -->
        <!--   <xsl:text>xmlns(mn=http://www.merunetworks.com/namespace/melf)xpointer(/mn:melf//mn:example[@id='</xsl:text> -->
        <!--   <xsl:apply-templates select="exsl:node-set($entry)/mn:entry/mn:format" mode="CPP"/> -->
        <!--   <xsl:text>'])</xsl:text> -->
        <!-- </xsl:attribute> -->
        <xsl:attribute name="xpointer">
          <xsl:text>xmlns(mn=http://www.merunetworks.com/namespace/melf)xpointer(/mn:melf//mn:example[@uid='</xsl:text>
          <xsl:value-of select="concat('example_', count(preceding::mn:example))"/>
          <xsl:text>'])</xsl:text>
        </xsl:attribute>
      </xsl:element>

    </xsl:if>

    <!-- <xsl:value-of select="$newline"/> -->
    <!-- <xsl:apply-templates select="./comment()"/> -->
    <!-- <xsl:value-of select="$newline"/> -->
    <!-- <xsl:text disable-output-escaping="yes"> -->
    <!-- amplt lt semicolon bang doublehyphen </xsl:text> -->
    <!-- <xsl:apply-templates select="." mode="nocomment"/> -->
    <!-- <xsl:text disable-output-escaping="yes"> -->
    <!-- doublehyphen amp gt semicolon </xsl:text> -->
  </xsl:template>
  <!-- stalog-melf.xml -->






  <!-- melf.xml -->
  <xsl:template match="mn:entry" mode="melf">
    <xsl:copy>
      <!-- <xsl:apply-templates select="@*|node()"/> -->
      <xsl:attribute name="uid">
        <xsl:value-of select="concat('entry_', count(preceding::mn:entry))"/>
      </xsl:attribute>
      <xsl:attribute name="fmtid">
        <xsl:apply-templates select="./mn:format" mode="CPP"/>
      </xsl:attribute>
      <xsl:attribute name="entryid">
        <xsl:value-of select="@id"/>
      </xsl:attribute>
      <xsl:apply-templates select="@*[local-name() != 'id']|node()" mode="melf"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="mn:entry/mn:instance|mn:instance" mode="melf">

    <xsl:variable name="entryref">
      <xsl:copy>
        <xsl:choose>
          <xsl:when test="local-name(..) = 'entry'">
            <xsl:attribute name="uidref">
              <xsl:value-of select="concat('entry_', count(../preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select=".." mode="identity"/>
          </xsl:when>
          <xsl:when test="//mn:entry[@id=current()/@entry]">
            <xsl:attribute name="uidref">
              <xsl:value-of select="concat('entry_',
                                    count(//mn:entry[@uid=current()/@uidref
                                                     or
                                                     @entry=current()/@entry]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select="//mn:entry[@id=current()/@entry]" mode="identity"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="uidref">
              <xsl:value-of select="concat('entry_',
                                    count(//mn:entry[@uid=current()/@uidref or
                                                     @entryid=current()/@entryref]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates
                select="//mn:entry[@uid=current()/@uidref or @entryid=current()/@entryref]" mode="identity"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:copy>
    </xsl:variable>


    <xsl:copy>
      <xsl:attribute name="uid">
        <xsl:value-of select="concat('instance_', count(preceding::mn:instance))"/>
      </xsl:attribute>
      <xsl:attribute name="uidref">
        <xsl:value-of select="exsl:node-set($entryref)/mn:instance/@uidref"/>
      </xsl:attribute>
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
      <xsl:attribute name="entryref">
        <xsl:choose>
          <xsl:when test="@entry">
            <xsl:value-of select="@entry"/>
          </xsl:when>
          <xsl:when test="exsl:node-set($entryref)/mn:instance/mn:entry/@entryid">
            <xsl:value-of select="exsl:node-set($entryref)/mn:instance/mn:entry/@entryid"/>
          </xsl:when>
          <xsl:when test="exsl:node-set($entryref)/mn:instance/mn:entry/@id">
            <xsl:value-of select="exsl:node-set($entryref)/mn:instance/mn:entry/@id"/>
          </xsl:when>
        </xsl:choose>
      </xsl:attribute>
      <xsl:apply-templates select="@*[local-name() != 'entry' and
                                   local-name() != 'entryid' and
                                   local-name() != 'fmtid' and
                                   local-name() != 'uidref' and
                                   local-name() != 'fmtref' and
                                   local-name() != 'entryref']|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="mn:example" mode="melf">

    <xsl:variable name="entryref">
      <xsl:copy>
        <xsl:choose>
          <xsl:when test="local-name(..) = 'entry'">
            <xsl:attribute name="uidref">
              <xsl:value-of select="concat('entry_', count(../preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select=".." mode="identity"/>
          </xsl:when>
          <xsl:when test="//mn:entry[@id=current()/@entry]">
            <xsl:attribute name="uidref">
              <xsl:value-of select="concat('entry_', count(//mn:entry[@id=current()/@entry]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates select="//mn:entry[@id=current()/@entry]" mode="identity"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="uidref">
              <xsl:value-of select="concat('entry_',
                                    count(//mn:entry[@uid=current()/@uidref
                                                     or
                                                     @entryid=current()/@entryref]/preceding::mn:entry))"/>
            </xsl:attribute>
            <xsl:apply-templates
                select="//mn:entry[@uid=current()/@uidref or
                                   @entryid=current()/@entryref]" mode="identity"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:copy>
    </xsl:variable>


    <xsl:copy>
      <xsl:attribute name="uid">
        <xsl:value-of select="concat('example_', count(preceding::mn:example))"/>
      </xsl:attribute>
      <xsl:attribute name="uidref">
        <xsl:value-of select="exsl:node-set($entryref)/mn:example/@uidref"/>
      </xsl:attribute>
      <xsl:attribute name="fmtid">
        <xsl:text>Ref</xsl:text>
        <xsl:apply-templates
            select="exsl:node-set($entryref)/mn:example/mn:entry/mn:format"
            mode="CPP"/>
      </xsl:attribute>
      <xsl:attribute name="fmtref">
        <xsl:apply-templates
            select="exsl:node-set($entryref)/mn:example/mn:entry/mn:format"
            mode="CPP"/>
      </xsl:attribute>
      <xsl:attribute name="entryref">
        <xsl:choose>
          <xsl:when test="@entry">
            <xsl:value-of select="@entry"/>
          </xsl:when>
          <xsl:when test="exsl:node-set($entryref)/mn:example/mn:entry/@entryid">
            <xsl:value-of select="exsl:node-set($entryref)/mn:example/mn:entry/@entryid"/>
          </xsl:when>
          <xsl:when test="exsl:node-set($entryref)/mn:example/mn:entry/@id">
            <xsl:value-of select="exsl:node-set($entryref)/mn:example/mn:entry/@id"/>
          </xsl:when>
        </xsl:choose>
      </xsl:attribute>
      <xsl:apply-templates select="@*[local-name() != 'entry' and
                                   local-name() != 'entryid' and
                                   local-name() != 'fmtid' and
                                   local-name() != 'uidref' and
                                   local-name() != 'fmtref' and
                                   local-name() != 'entryref']|node()"/>
    </xsl:copy>
  </xsl:template>
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
