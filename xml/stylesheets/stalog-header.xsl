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



  <xsl:template match="mn:format" mode="define">
    <xsl:value-of select="$space"/>
    <xsl:value-of select="$doublequote"/>
    <xsl:apply-templates select="." mode="C">
      <xsl:with-param  name="params">
        <xsl:copy-of select="mn:params"/>
      </xsl:with-param>
    </xsl:apply-templates>
    <xsl:value-of select="$doublequote"/>
    <xsl:value-of select="$newline"/>
  </xsl:template>

  <xsl:template match="mn:format" mode="cpp_name">
    <xsl:text>#define</xsl:text>
    <xsl:value-of select="$space"/>
    <xsl:text>MELF_FMT_</xsl:text>
    <xsl:apply-templates select="." mode="CPP">
      <xsl:with-param  name="params">
        <xsl:copy-of select="mn:params"/>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="mn:entry" mode="uid_name">
    <xsl:text>#define</xsl:text>
    <xsl:value-of select="$space"/>
    <xsl:text>MELF_FMT_</xsl:text>
    <xsl:value-of select="translate(translate(normalize-space(@uid), ' ', '_'),
                                    $smallcase,
                                    $uppercase)"/>
  </xsl:template>


  <xsl:template match="mn:entry" mode="in_print">
    <xsl:text disable-output-escaping="yes">/* [in print] -  </xsl:text>
    <xsl:apply-templates
        select="mn:format"
        mode="VALUE">
      <xsl:with-param name="params">
        <xsl:copy-of select="mn:params"/>
      </xsl:with-param>
    </xsl:apply-templates>
    <xsl:text disable-output-escaping="yes">  */</xsl:text>
    <xsl:value-of select="$newline"/>
  </xsl:template>

  <xsl:template match="/">
/******************************************************************
 * Filename:  stalog.h
 * Component: Melfd
 * Subsystem: Melfd
 *
 *    Copyright Meru Networks 2003, All Rights Reserved
 *
 * File Overview
 * -------------
 *   Melf Event Formats.
 *
 ***********************************************************************/
#ifndef _STALOG_HEADER_H_
#define _STALOG_HEADER_H_


NAMESPACE_BEGIN(Meru)

<xsl:apply-templates/>


NAMESPACE_END(Meru)

#endif /* _STALOG_HEADER_H_ */
  </xsl:template>





  <xsl:template match="mn:entry">

    <xsl:value-of select="$newline"/>
    <xsl:text disable-output-escaping="yes">/* [location] -  </xsl:text>
    <xsl:value-of select="$newline"/>
    <xsl:apply-templates select="comment()"/>
    <xsl:value-of select="$newline"/>
    <xsl:text disable-output-escaping="yes"> */</xsl:text>
    <xsl:value-of select="$newline"/>
    <xsl:value-of select="$newline"/>

    <xsl:apply-templates select="." mode="in_print"/>
    <xsl:value-of select="$newline"/>


    <xsl:apply-templates select="." mode="uid_name"/>
    <xsl:apply-templates select="./mn:format" mode="define"/>

    <xsl:value-of select="$newline"/>

    <xsl:apply-templates select="./mn:format" mode="cpp_name"/>
    <xsl:apply-templates select="./mn:format" mode="define"/>

    <xsl:value-of select="$newline"/>

    <xsl:text>#define</xsl:text>
    <xsl:value-of select="$space"/>
    <xsl:value-of select="mn:cprintfformat"/>

    <xsl:apply-templates select="./mn:format" mode="define"/>

    <xsl:value-of select="$newline"/>


    <xsl:value-of select="$newline"/>
    <xsl:value-of select="$newline"/>
    <xsl:value-of select="$newline"/>
    <xsl:value-of select="$newline"/>
  </xsl:template> <!-- <xsl:template match="mn:entry"> -->

  <!-- <xsl:template match="mn:*/text()"> -->
  <!-- </xsl:template> -->

  <xsl:template match="mn:format/text()|">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="mn:format">
    <xsl:param name="params"/>
    <xsl:apply-templates>
      <xsl:with-param  name="params" select="$params"/>
    </xsl:apply-templates>
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

    <xsl:message select="exsl:node-set($output)"/>

    <xsl:variable name="routput"
                  select="translate(translate(exsl:node-set(normalize-space($output)), ' ', '_'),
                                    $smallcase,
                                    $uppercase)"/>
    <xsl:value-of
        select="translate($routput,
                          translate($routput, $cppvalidchars, ''),
                          '')"/>
  </xsl:template>


  <xsl:template match="mn:format/mn:fmtparam" mode="C">
    <xsl:param name="params"/>
    <xsl:choose>
      <xsl:when test="exsl:node-set($params)/mn:params/mn:fmtparams/mn:param[mn:name/text()=current()/@name]/mn:ctype">
        <xsl:value-of
            select="exsl:node-set($params)/mn:params/mn:fmtparams/mn:param[mn:name/text()=current()/@name]/mn:ctype"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of
            select="../../mn:params/mn:fmtparams/mn:param[mn:name/text()=current()/@name]/mn:ctype"/>
      </xsl:otherwise>
    </xsl:choose>
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


  <!-- <xsl:template match="mn:removecomment/comment()"> -->
  <!-- </xsl:template> -->

</xsl:stylesheet>
