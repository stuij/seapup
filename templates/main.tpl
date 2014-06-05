<!DOCTYPE html>
<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0" />
<html>
  <head>
    <title>Seapup</title>
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
    <link id="size-stylesheet" type="text/css" rel="stylesheet" href="/static/css/wide.css" /> 

    <!--[if lt ie 7]>
        <style type="text/css">
          #output { margin-bottom: 0; }
        </style>
        <![endif]-->
    
    <!-- <script src="http://code.jquery.com/jquery-1.9.1.min.js"></script>-->
    <script type="text/javascript">
      debug=<!-- TMPL_VAR debug -->;
      acceptI18ns=<!-- TMPL_VAR accept-i18ns -->;
    </script>
    <script type="text/javascript" src="/static/js/contrib/jquery-1.9.1.js"></script>
    <script type="text/javascript" src="/static/js/contrib/jquery.form.js"></script>
    <script type="text/javascript" src="/static/js/contrib/json2.js"></script>
    <script type="text/javascript" src="/static/js/contrib/verge.js"></script>
    <script type="text/javascript" src="/static/js/site-code/pup.js"></script>
    <script type="text/javascript">
      jQuery(document).ready(pup.init);
    </script>
  </head> 
  <body>
    <div class="background"></div>
    <div class="scroll-down-maybe">
      <div class="scroll-down-maybe-inner"></div>
    </div>
    <div id="output">
      <div id="padding">
      </div>
      <div id="top">Welcome to Seapup's cosy home!<br/>
        Type some stuff below if you feel like.
        <br/><br/><br/>
      </div>
    </div>
    <div class="input">
      <form class="input-form" autocomplete="off">
        &gt; <input id="input-input" type="text">
      </form> 
    </div>
  </body>
</html>
