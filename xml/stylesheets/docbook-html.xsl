<?xml version='1.0'?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns="http://www.w3.org/1999/xhtml"
    version="1.0">

  <xsl:import href="/usr/share/xml/docbook/stylesheet/nwalsh/xhtml/docbook.xsl"/>

  <xsl:param name="html.stylesheet" select="'stalog.css'"/>
  <xsl:param name="admon.graphics" select="1"/>


  <xsl:param name="generate.toc" select="nop"/>

  <!-- <xsl:param name="generate.toc" select="'book toc'"/> -->

  <!-- include our javascript -->
  <xsl:template name="user.head.content">
    <!-- <meta name="viewport" content="width=device-width, initial-scale=1"\> -->
    <script src="http://code.jquery.com/jquery-2.1.3.min.js" type="text/javascript"></script>
    <link rel="apple-touch-icon" href="res/apple-touch-icon.png"/>
    <link rel="stylesheet" href="res/css/normalize.min.css" media="all"/>
    <link rel="stylesheet" href="res/css/main.css" media="all"/>
    <link rel="stylesheet" href="res/css/custom.css" media="all"/>
    <!-- <script -->
    <!--     src="res/js/vendor/modernizr-2.8.3-respond-1.4.2.min.js"></script> -->

    <style>
      #container { width:960px; overflow:hidden; margin:0px auto; position:relative;}
      #content { width:660px; float:right;}
      #toc { width:200px; position:fixed; float:left;}
      #toc a { display:block; color:#0094FF;}
    </style>
  </xsl:template>

  <xsl:template name="meru.body.content">
    <div id="toc">
      <xsl:text>
      </xsl:text>
      <toc role="chunk-toc">
        <xsl:text>
        </xsl:text>
        <xsl:apply-templates select="/" mode="toc"/>
      </toc>
      <xsl:text>
      </xsl:text>
    </div>
  </xsl:template>


<xsl:template match="*" mode="process.root">
  <xsl:variable name="doc" select="self::*"/>

  <xsl:call-template name="user.preroot"/>
  <xsl:call-template name="root.messages"/>

  <html>
    <xsl:call-template name="root.attributes"/>
    <head>
      <xsl:call-template name="system.head.content">
        <xsl:with-param name="node" select="$doc"/>
      </xsl:call-template>
      <xsl:call-template name="head.content">
        <xsl:with-param name="node" select="$doc"/>
      </xsl:call-template>
      <xsl:call-template name="user.head.content">
        <xsl:with-param name="node" select="$doc"/>
      </xsl:call-template>
    </head>

    <body>

      <div id="container">

        <div class="merutoc">
          <xsl:call-template name="meru.body.content">
            <xsl:with-param name="node" select="$doc"/>
          </xsl:call-template>
        </div>

        <div id="content">
          <div class="main-container">
            <div class="main wrapper clearfix">
              <xsl:call-template name="body.attributes"/>
              <xsl:call-template name="user.header.content">
                <xsl:with-param name="node" select="$doc"/>
              </xsl:call-template>
              <xsl:apply-templates select="."/>
              <xsl:call-template name="user.footer.content">
                <xsl:with-param name="node" select="$doc"/>
              </xsl:call-template>
            </div>
          </div>
        </div>
      </div>
    </body>
  </html>
  <xsl:value-of select="$html.append"/>

  <!-- Generate any css files only once, not once per chunk -->
  <xsl:call-template name="generate.css.files"/>
</xsl:template>



</xsl:stylesheet>
