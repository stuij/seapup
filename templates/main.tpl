<!DOCTYPE html>

<!-- Yay you're reading the source :)
     The site is up on Github, for your viewing and forking pleasure: https://github.com/stuij/seapup -->

<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
<html>
  <head>
    <title>AwareWolf/Seapup</title>

    <meta charset="UTF-8">
    <meta name="description" content="A Seapup and an AwareWolf. Stuck in one head.">
    <meta name="keywords" content="AwareWolf, Seapup, wolf, pup, Common Lisp, robot, lost at sea, AI winter is coming">
    <meta name="author" content="Ties Stuij">

    <link rel="alternate" type="application/rss+xml" title="Awarewolf/Seapup Feed" href="http://awarewolf.io/feed?cat=all"/>

    
    <!-- Automatic favicon resizings done by the very handy http://realfavicongenerator.net -->
    <link rel="shortcut icon" href="/static/img/favicons/favicon.ico">
    <link rel="apple-touch-icon" sizes="57x57" href="/static/img/favicons/apple-touch-icon-57x57.png">
    <link rel="apple-touch-icon" sizes="114x114" href="/static/img/favicons/apple-touch-icon-114x114.png">
    <link rel="apple-touch-icon" sizes="72x72" href="/static/img/favicons/apple-touch-icon-72x72.png">
    <link rel="apple-touch-icon" sizes="144x144" href="/static/img/favicons/apple-touch-icon-144x144.png">
    <link rel="apple-touch-icon" sizes="60x60" href="/static/img/favicons/apple-touch-icon-60x60.png">
    <link rel="apple-touch-icon" sizes="120x120" href="/static/img/favicons/apple-touch-icon-120x120.png">
    <link rel="apple-touch-icon" sizes="76x76" href="/static/img/favicons/apple-touch-icon-76x76.png">
    <link rel="apple-touch-icon" sizes="152x152" href="/static/img/favicons/apple-touch-icon-152x152.png">
    <link rel="icon" type="image/png" href="/static/img/favicons/favicon-196x196.png" sizes="196x196">
    <link rel="icon" type="image/png" href="/static/img/favicons/favicon-160x160.png" sizes="160x160">
    <link rel="icon" type="image/png" href="/static/img/favicons/favicon-96x96.png" sizes="96x96">
    <link rel="icon" type="image/png" href="/static/img/favicons/favicon-16x16.png" sizes="16x16">
    <link rel="icon" type="image/png" href="/static/img/favicons/favicon-32x32.png" sizes="32x32">
    <meta name="msapplication-TileColor" content="#000000">
    <meta name="msapplication-TileImage" content="/static/img/favicons/mstile-144x144.png">
    <meta name="msapplication-config" content="/static/img/favicons/browserconfig.xml">
    
    <link type="text/css" href="/static/css/pup.css" rel="stylesheet"/>

    <!-- doubling stylesheet setting here and in js to cover more browser/settings combinations <sigh/> -->
    <link rel="stylesheet" media="only screen and (max-width: 599px)" href="/static/css/narrow.css" />
    <link rel="stylesheet" media="only screen and (min-width: 600px) and (max-width: 699px)" href="/static/css/middle.css" />
    <link id="size-stylesheet" type="text/css" rel="stylesheet" media="screen and (min-width: 700px)" href="/static/css/wide.css" />

    <!--[if lt ie 7]>
        <style type="text/css">
          #output { margin-bottom: 0; }
        </style>
        <![endif]-->
    
    <script type="text/javascript">
      var debug=<!-- TMPL_VAR debug -->;
      var acceptI18ns=<!-- TMPL_VAR accept-i18ns -->;
      var session_id="<!-- TMPL_VAR session-val -->";
    </script>
    <script type="text/javascript" src="/static/js/contrib/jquery-1.9.1.js"></script>
    <script type="text/javascript" src="/static/js/contrib/jquery.history.js"></script>
    <script type="text/javascript" src="/static/js/contrib/verge.js"></script>
    <script type="text/javascript" src="/static/js/site-code/pup.js"></script>
    <script type="text/javascript">
      jQuery(document).ready(pup.init);
    </script>
  </head> 
  <body>
    <div class="background"></div>    
    <div id="output">
      <div id="padding">
      </div>
      <div id="top">
        <!-- TMPL_VAR welcome -->
        <br/>
        <div class="termSliver" id="termId0">
          <div class=user>&gt; <span class="user-input"><!-- TMPL_VAR input --></span>
          </div>
          <div class=eliza>
            <!-- TMPL_VAR output -->
          </div>
        </div>
      </div>
    </div>
    <div class="scroll-down-maybe">
      <div class="scroll-down-maybe-inner"></div>
    </div>
    <div class="input">
      <form class="input-form" autocomplete="off" method="post" action="<!-- TMPL_VAR input-url -->">
        &gt; <input id="input-input" name="input" type="text" autofocus>
        <input type="hidden" name="<!-- TMPL_VAR session-key -->" value="<!-- TMPL_VAR session-val -->" />
      </form> 
    </div>
  </body>
</html>
