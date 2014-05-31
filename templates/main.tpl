<!DOCTYPE html>
<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0" />
<html>
  <head>
    <title>Seapup</title>
    
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
