if(typeof console === "undefined") {
    console = { log: function() { } };
}

if(typeof console.log === "undefined") {
    console.log = function() {};
}

if(typeof console.debug === "undefined") {
    console.debug = function() {};
}

var log = function () {
    if(debug) {
        console.log.apply(null, arguments);
    }
};

var dbg = function () {
    if(debug) {
        console.debug.apply(null, arguments);
    }
};

var detectIE = function () {
    var ua = window.navigator.userAgent;
    var msie = ua.indexOf('MSIE ');
    var trident = ua.indexOf('Trident/');

    if (msie > 0) {
        // IE 10 or older => return version number
        return parseInt(ua.substring(msie + 5, ua.indexOf('.', msie)), 10);
    }

    if (trident > 0) {
        // IE 11 (or newer) => return version number
        var rv = ua.indexOf('rv:');
        return parseInt(ua.substring(rv + 3, ua.indexOf('.', rv)), 10);
    }

    // other browser
    return false;
};

var pup = 
    (function ($) {

         $.extend(verge);

         $.expr[':'].external = function(obj){
             return !obj.href.match(/^mailto\:/)
                 && (obj.hostname != location.hostname)
                 && !obj.href.match(/^javascript\:/)
                 && !obj.href.match(/^$/);
         };

         // var
         var rpc_id=1;

         // fns
         var jrpc = function(url, id, method, params, success, error) {
             var request = JSON.stringify(
                 {'jsonrpc': '2.0', 'method': method,
                  'params': params, 'id': id});
             return $.ajax(
                 { url: url,
                   data: request,
                   success: success,
                   error: error,
                   contentType: 'application/json',
                   dataType: 'json',
                   async: true,
                   cache: false,
                   //timeout: 1,
                   type: 'POST'});
         };

         var terminal_rpc = function(input){
             var id = rpc_id++;
             if (input === '') {
                 return -1;
             } else {
                 terminal.pause();
                 jrpc('/ajax', id, 'eliza', 
                      [{input: input, session: session_id}],
                      function(json) {
                          if (!json.error) {
                              session_id = json.result.session;
                              terminal.print(json); 
                          } else {
                              terminal.error(id, 'Oops.. got a json response error: ' + json.error.message);
                          }
                          terminal.resume();
                      }, 
                      function(xhr, status, error) {
                          terminal.error(id, "Oops.. got a server error. "
                                         + "Status is: " + status +
                                         ', Server reponse is: \n' +
                                         xhr.responseText);
                          terminal.resume();
                      });
             };
             return id;
         };

         var adjustStyle = function (width) {
             width = parseInt(width);
             if (width < 600) {
                 $("#size-stylesheet").attr("href", "/static/css/narrow.css");
             } else if (width < 700) {
                 $("#size-stylesheet").attr("href", "/static/css/middle.css");
             } else {
                 $("#size-stylesheet").attr("href", "/static/css/wide.css");
             }
         };

         var resToStyle = function() {
             adjustStyle($(this).width());            
         };
         
         var init = function($) {
             resToStyle();
             $(window).resize(resToStyle);
             terminal.init(terminal_rpc, $(".input-form"), $("#output"));
         };

         
         // interface
         return {
             init: init
         };
         
     })(jQuery);

var terminal = 
    (function () {
         // vars
         var $in_form;
         var $in_input;
         var $out;
         var rpc;
         var History = window.History;

         var focus_on_input = function () {
             $in_input[0].focus();
         };

         // fns
         var init = function (an_rpc, textIn, domOut) {
             $in_form = textIn;
             $out = domOut;
             rpc = an_rpc;

             setup_plumbing();
             focus_on_input();
             massage_term_div($("#top"));
             scrollTo($("#top"));
             maybe_input_hash();
             // $("#padding").height(0);
         };

         var maybe_input_hash = function () {
             var hash = decode_hash(location.hash);
             var input = get_parameter_by_name("input", location.search);
             if(hash && !input) {
                 user_input(hash);
             }             
         };

         var setup_plumbing = function () {
             $in_input = $(":input", $in_form);
             $in_form.on('submit', function(e) {
                             e.preventDefault();
                             input_submit();
                         });
             $("#padding").height($.viewportH());
             setup_scrolling();
         };

         var get_IE_all_down = function () {
             return document.body.scrollHeight - $(this).scrollTop() + 12
                 <= $(this).height() ? true : false;
         };
         
         var get_others_all_down = function () {             
             return window.innerHeight + window.scrollY - 12 >= 
                 document.body.offsetHeight ? true : false;
         };

         var setup_scrolling = function () {
             window.onscroll =
                 function(){
                     var allDown = detectIE() ? 
                         get_IE_all_down() : get_others_all_down();

                     if (allDown) {
                         $(".scroll-down-maybe-inner")
                             .css("background-image", "none");
                     } else {
                         $(".scroll-down-maybe-inner")
                             .css("background-image", 
                                  "url(/static/img/arrow-faint.png)");
                     }
                 };
         };

         var input_submit = function () {
             user_input($in_input.val());
             $in_input.val("");
             return false;
         };

         var user_input = function (msg) {
             if(msg) {
                 var id = rpc(msg);
                 user_output(msg, id);
             }
         };

         var user_output = function (string, id) {
             var $msg = $("<div class='termSliver' id='termId" + id + "'>");
             $msg.append("<div class=user>> <span class='user-input'>" 
                         + string + "</span></div>");
             $out.append($msg);
             massage_term_div($msg);
             scrollTo($msg);
         };

         var print_result = function (json) {
             output_result(json.id, json.result.output);
         };

         var output_result = function (id, string) {
             var $div = $("#termId" + id);
             $div.append("<div class=eliza>" + string + "</div>");
             massage_term_div($div);
             scrollTo($div);
         };

         var make_term_url = function (msg) {
             var l = window.location;
             return l.protocol + "//" + l.host + l.pathname + "#! " + msg;  
         };

         var term_click = function (e) {
             e.preventDefault();
             var msg = get_link_input(e);
             if(msg) {
                 user_input(msg);
                 History.pushState(null, msg, "?input=" + encodeURIComponent(msg));
             }
             focus_on_input();
         };

         var get_link_input = function (e) {
             return get_parameter_by_name("input", e.target.search)
                 || decode_hash(e.target.hash);
         };

         var massage_term_div = function ($div) {
             $(".termLink", $div).click(term_click);
             $("a:external", $div).attr('target', '_blank');
             $(".img-link", $div).attr('target', '_blank');
         };

         var pause = function () {
             // we don't need this yet
         };

         var resume = function () {
             // and this neither
         };

         var error = function (id, string) {
             output_result(id, string);
         };

         // helpers
         var scrollTo = function ($elem){
             $("html,body").animate(
                 { scrollTop: $elem.offset().top - 12},
                 "slow");
         };

         var get_hash_msg = function () { 
             return decode_hash(window.location.hash);
         };

         var decode_hash = function (fragment) {
             return get_parameter_by_name("input", fragment.slice(1));
         };

         var get_parameter_by_name = function (name, href) {
             var regex_string = "[\\?&]" + name + "=([^&#]*)";
             var regex = new RegExp(regex_string);
             var results = regex.exec(href);
             if( results == null )
                 return null;
             else
                 return decodeURIComponent(results[1].replace(/\+/g, " "));
         };

         // not used
         var put_hash_msg = function (msg) {
             window.location.replace("#" + encodeURIComponent(msg));
         };

         // interface
         return {
             init: init,
             print: print_result,
             pause: pause,
             resume: resume,
             error: error,
             self: this
         };
     })();
