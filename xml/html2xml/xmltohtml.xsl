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

  <!-- <xsl:template match="processing-instruction()|comment()"/> -->

  <!-- <xsl:template match="@*|*|node()|text()|comment()|processing-instruction()"> -->
  <xsl:template match="@*|*|node()|text()|comment()">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
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

  <!-- <xsl:template match="../processing-instruction('xml-stylesheet')"/> -->
  <!-- <xsl:template match="../processing-instruction('xml-stylesheet')[type='text/xsl']"/> -->
  <xsl:template match="processing-instruction('xml-stylesheet')[@type='text/xsl']"/>  <!-- Not working -->
  <!-- <xsl:template match="processing-instruction('xml-stylesheet')@type[contains(.,'text/xsl')]"/> -->
  <xsl:template match="processing-instruction('xml-stylesheet')"/>

  <xsl:template match="XML">
    <xsl:text>
    </xsl:text>
    <xsl:element name="html">
      <xsl:element name="head">

        <xsl:element name="link">
          <xsl:attribute name="href">Installing_AP832.css</xsl:attribute>
          <xsl:attribute name="rel">stylesheet</xsl:attribute>
          <xsl:attribute name="type">text/css</xsl:attribute>
          <xsl:text> </xsl:text>
        </xsl:element>


        <xsl:element name="title">
          <xsl:value-of select="TITLE/mnDocTitle"/>
        </xsl:element>

      </xsl:element>
      <xsl:element name="body">
        <xsl:apply-templates select="./*"/>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <xsl:template match="A">
    <xsl:if test="count(*)!=0">
      <xsl:copy-of select="."/>
    </xsl:if>
  </xsl:template>

  <xsl:template match="tf:heading/node()/@class">
    <xsl:attribute name="class"><xsl:value-of select="."/>XYZ</xsl:attribute>
  </xsl:template>

  <xsl:template match="tf:heading">
    <xsl:element name="span">
      <xsl:if test="count(A/*)=0">
        <xsl:copy-of select="A/@*"/>

        <xsl:if test="not(A/@id) and A/@name">
          <xsl:attribute name="id"><xsl:value-of select="A/@name"/></xsl:attribute>
        </xsl:if>

      </xsl:if>
      <xsl:if test="count(node()/A/*)=0">
        <xsl:copy-of select="node()/A/@*"/>

        <xsl:if test="not(node()/A/@id) and node()/A/@name">
          <xsl:attribute name="id"><xsl:value-of select="node()/A/@name"/></xsl:attribute>
        </xsl:if>


      </xsl:if>
      <xsl:if test="count(a/*)=0">
        <xsl:copy-of select="a/@*"/>

        <xsl:if test="not(a/@id) and a/@name">
          <xsl:attribute name="id"><xsl:value-of select="a/@name"/></xsl:attribute>
        </xsl:if>

      </xsl:if>

      <xsl:if test="count(node()/a/*)=0">
        <xsl:copy-of select="node()/a/@*"/>

        <xsl:if test="not(node()/a/@id) and node()/a/@name">
          <xsl:attribute name="id"><xsl:value-of select="node()/a/@name"/></xsl:attribute>
        </xsl:if>

      </xsl:if>
      <xsl:attribute name="class"><xsl:value-of select="@parent"/>-parent</xsl:attribute>
      <xsl:apply-templates select="./*|text()|comment()|processing-instruction()"/>
    </xsl:element>
  </xsl:template>


  <!-- Named template -->

  <xsl:template name="div">
    <xsl:choose>
      <xsl:when test="not(count(*) = 1 and div and div/@class != local-name())">
        <xsl:element name="div">
          <xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
          <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()"/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="span">
    <xsl:choose>
      <xsl:when test="not(count(*) = 1 and span and span/@class != local-name())">
        <xsl:element name="span">
          <xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
          <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()"/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="pre">
    <xsl:choose>
      <xsl:when test="not(count(*) = 1 and pre and pre/@class != local-name())">
        <xsl:element name="pre">
          <xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
          <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()"/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="p">
    <xsl:choose>
      <xsl:when test="not(count(*) = 1 and p and p/@class != local-name())">
        <xsl:element name="p">
          <xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
          <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()"/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="li">
    <xsl:choose>
      <xsl:when test="not(count(*) = 1 and li and li/@class != local-name())">
        <xsl:element name="li">
          <xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
          <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()"/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- config.htm -->

<!-- a|Anchor|ansi0|AppendixNumber|Argument|body|Body|Body2|Body3|Body-Indent|Bold|br|Bullet1|Bullet2|Bulleted|Bulleted2|Bulleted3|caption|Caution|CautionGraphic|CellBody|CellBullet|CellCode|CellHeading|ChapterNum|ChapterTitle|ChapterTitleNoNum|Code|Code1-|CodeEnd|CodeIndent|CodeIndentOutput|CodeStart|CodeWide|CodeWide1-|div|emphasis|Emphasis|FigCaption|FigCaptionWide|GlossaryTerm|GlossaryText|head|Heading1|Heading1Gloss|Heading2|Heading3|Heading4|html|HTML-Typewriter|img|Lettered|Lettered1|link|map|meta|Normal|Note|NoteGraphic|Numbered|Numbered1|ol|Strong|Superscript|table|TableTitle|td|tf:heading|th|title|tr|ul|URL|Warning|WarningGraphic|Xref|XRef| -->

  <!-- Div -->
  <xsl:template match="Heading1|Heading2|Heading3|GlossaryTerm">
    <xsl:call-template name="div"/>
  </xsl:template>

  <!-- Span -->
  <!-- <xsl:template match=""> -->
  <!--   <xsl:call-template name="span"/> -->
  <!-- </xsl:template> -->


  <!-- Li -->
  <xsl:template match="Bullet1|Bullet2|Bulleted|Bulleted2|Bulleted3|Bulleted2|Bulleted2List|BulletedList">
    <xsl:call-template name="li"/>
  </xsl:template>


  <!-- Installing_AP832.htm -->
  <!-- Div -->
  <xsl:template match="mnH1|mnH2|mH2|mH3|mnBodyText|mnNoteCautionWarningText|mnChapterTitle|mnProcedureHeading|Body|mnBody-in-StepLevel1|mnNoteIcon|mnCopyrightTextCoverPage|mnDocTitle|mnDocType|TITLE|BodyAfterHead|BodyBeforeHead|mnAppendixTitle|cellbody|CellBody">
    <xsl:call-template name="div"/>
  </xsl:template>

  <!-- Span -->
  <xsl:template match="mnCellHeading|mnCellBody|mnFigureTableTitle|mnAnchor|mnFigureTitle|mnLink|mnCautionIcon|mnWarningIcon|mnHeadingRunIn|mnNOTE-runIn|mnCE-alertICON">
    <xsl:call-template name="span"/>
  </xsl:template>


  <!-- cli.htm -->
  <!-- Div -->
  <xsl:template match="ChapterTitle|CommandName|Body|Body2">
    <xsl:call-template name="div"/>
  </xsl:template>

  <!-- Span -->
  <xsl:template match="Anchor|ansi0|Bold|CAPTION|Caution|CautionGraphic|CautionList|CELL|CellBody|CellBullet|CellBulletList|CellHeading|ChapterNum|ChapterTitle|Code|Code1-|CodeEnd|CodeStart|CodeWide|CodeWide1-|DIV|Emphasis|FigCaption|Heading3|IMAGE|Keyword|Normal|Note|NoteGraphic|NoteList|ROW|Syntax|TABLE|TableTitle|TH|TITLE|Xref">
    <xsl:call-template name="span"/>
  </xsl:template>

  <!-- pre -->
  <xsl:template match="Code1-|Code">
    <xsl:call-template name="pre"/>
  </xsl:template>


  <!-- Div -->
  <xsl:template match="CommandHeading">
    <xsl:element name="div">
      <xsl:if test="string-length(normalize-space(tf:heading)) &lt; 20">
        <xsl:attribute name="class"><xsl:value-of select="local-name()"/><xsl:text> </xsl:text><xsl:value-of select="translate(normalize-space(tf:heading),' ', '_')"/>_Heading</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()"/>
    </xsl:element>
  </xsl:template>


  <xsl:template match="XRef">
    <xsl:element name="a">
      <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()"/>
    </xsl:element>
  </xsl:template>


  <!-- <xsl:template match="@" -->

  <!-- Table -->
  <xsl:template match="ROW">
    <xsl:element name="tr">
      <xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
      <xsl:apply-templates select="./*|@*|text()|comment()|processing-instruction()"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="CELL">
    <xsl:element name="td">
      <xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
      <xsl:apply-templates select="./*|@*|text()|comment()|processing-instruction()"/>
    </xsl:element>
  </xsl:template>

  <!-- Bullet List -->
  <xsl:template match="mnBullet1List">
    <xsl:element name="ul">
      <xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
      <xsl:apply-templates select="./*|@*|text()|comment()|processing-instruction()"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="mnBullet1|mnBullet1-in-Step">
    <xsl:element name="li">
      <xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
      <xsl:apply-templates select="./*|@*|text()|comment()|processing-instruction()"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="mnStepFirstList|mnStepNextList">
    <xsl:element name="ol">
      <xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
      <xsl:apply-templates select="./*|@*|text()|comment()|processing-instruction()"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="mnStepFirst|mnStepNext">
    <xsl:element name="li">
      <xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
      <xsl:apply-templates select="./*|@*|text()|comment()|processing-instruction()"/>
    </xsl:element>
  </xsl:template>

  <!-- Emphasis -->
  <xsl:template match="mnEmphasisBold">
    <xsl:element name="b">
      <xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
      <xsl:apply-templates select="./*|@*|text()|comment()|processing-instruction()"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="Emphasis">
    <xsl:element name="i">
      <xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
      <xsl:apply-templates select="./*|@*|text()|comment()|processing-instruction()"/>
    </xsl:element>
  </xsl:template>

  <!-- IMAGE -->
  <xsl:template match="IMAGE">
    <xsl:element name="img">
      <xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
      <xsl:attribute name="src"><xsl:value-of select="@href"/></xsl:attribute>
      <xsl:apply-templates select="./*|@*|text()|comment()|processing-instruction()"/>
    </xsl:element>
  </xsl:template>

  <!-- Remove -->
  <xsl:template match="mnChapterTitleTOC|mnH1TOC|mH2TOC|mH3TOC|mnAppendixTitleTOC"/>



  <!-- HTML elemet -->
  <xsl:template match="table|TABLE">
    <!-- border="1" cellpadding="1" cellspacing="0" -->
    <xsl:copy>
      <xsl:attribute name="border">1</xsl:attribute>
      <xsl:attribute name="cellpadding">1</xsl:attribute>
      <xsl:attribute name="cellspacing">0</xsl:attribute>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>


  <!-- <xsl:template match="a/@href[not(starts-with(.,'http://')) and (contains(.,'cli.htm') or contains(.,'Installing_AP832.htm')) and not(contains(.,'/'))]"> -->
  <!--   <xsl:attribute name="href"> -->
  <!--     <xsl:value-of select="concat(substring-before(.,'.htm'),'-out.html',substring-after(.,'.htm'))"/> -->
  <!--   </xsl:attribute> -->
  <!-- </xsl:template> -->

  <xsl:template match="@href[not(starts-with(.,'http://')) and (contains(.,'cli.htm') or contains(.,'Installing_AP832.htm') or contains(.,'config.htm')) and not(contains(.,'/'))]">
  <!-- <xsl:template match="a/@href"> -->
    <xsl:attribute name="href">
      <xsl:value-of select="concat(substring-before(.,'.htm'),'-out.html',substring-after(.,'.htm'))"/>
    </xsl:attribute>
  </xsl:template>


</xsl:stylesheet>




