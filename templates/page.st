<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
          "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    $if(feed)$
    <link href="$base$/_feed/" type="application/atom+xml" rel="alternate" title="Sitewide ATOM Feed" />
    <link href="$base$/_feed$pageUrl$" type="application/atom+xml" rel="alternate" title="This page's ATOM Feed" />
    $endif$
    <title>$wikititle$ - $pagetitle$</title>
    $if(printable)$
    <link href="$base$/css/print.css" rel="stylesheet" media="all" type= "text/css" />
    $else$
    <link href="$base$/css/custom.css" rel="stylesheet" media="screen, projection" type="text/css" />
    <link href="$base$/css/print.css" rel="stylesheet" media="print" type= "text/css" />
    $endif$
    <!--[if IE]><link href="$base$/css/ie.css" rel="stylesheet" media="screen, projection" type="text/css" /><![endif]-->
  </head>
  <body>
    
    <div id="siteheader">
      <div id="cam-header">
        <div id="cam-blue-header">
          <div id="cam-insert">
            <a class="cam-insert-button" accesskey="4" href="http://www.cl.cam.ac.uk/search/">
              <img alt="[Search]" src="http://www.cl.cam.ac.uk/images/search.gif" />
            </a>
            <a class="cam-insert-button" href="http://www.cl.cam.ac.uk/az/">
              <img alt="[A-Z Index]" src="http://www.cl.cam.ac.uk/images/az.gif" />
            </a>
            <a class="cam-insert-button" href="http://www.cl.cam.ac.uk/contact/">
              <img alt="[Contact]" src="http://www.cl.cam.ac.uk/images/contact.gif" />
            </a>
          </div>
        </div>
        <div id="cam-image-header">
          <div id="cam-logo">
            <a href="http://www.cam.ac.uk/">
              <img src="http://www.cl.cam.ac.uk/images/identifier2.gif" />
            </a>
          </div>
          <div id="cprg-header">
            Computer Laboratory<br />
            Cambridge Programming Research Group
          </div>
        </div>
        <div class="cam-separator">
          &nbsp;
        </div>
      </div>
    </div>

    <div id="bread">
      <p>
        <a href="http://www.cl.cam.ac.uk/" class="bread">Computer&nbsp;Laboratory</a>
        &gt;
        <a href="http://www.cl.cam.ac.uk/UoCCL/research/" class="bread">Research</a>
        &gt;
        <a href="http://www.cl.cam.ac.uk/research/cprg/" class="bread">CPRG</a>
      </p>
    </div>
    
    <div id="doc3" class="yui-t1">
        <div id="yui-main">
          <div id="maincol" class="yui-b">
            $tabs$ 
            $content()$
            $footer()$
          </div>
        </div>
        <div id="sidebar" class="yui-b first">
          <!--
          $logo()$
          -->
          $if(sitenav)$
            $sitenav()$
          $endif$
          <!--
          $if(pagetools)$
            $pagetools()$
          $endif$
          -->
          $if(markuphelp)$
            $markuphelp()$
          $endif$
          $userbox()$
        </div>
    </div>
    $javascripts$
    $expire()$
    $getuser()$
  </body>
</html>
