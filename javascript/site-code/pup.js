jQuery.extend(verge);

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

var pup = 
    (function ($) {
         // var
         var rpc_id=0;

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
                 jrpc('/ajax', id, 'eliza', [input], 
                      function(json) {
                          if (!json.error) {
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
             scrollTo($("#top"));
             
             var msg = decode_hash(location.hash);
             if(msg) {
                 user_input(msg);
             }
         };

         var setup_plumbing = function () {
             $in_input = $(":input", $in_form);
             $in_form.ajaxForm(input_submit);
             $("#padding").height($.viewportH());
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
                         + massage_msg(string) + "</span></div>");
             $out.append($msg);
             massage_term_div($msg);
             scrollTo($msg);
         };

         var print_result = function (json) {
             output_result(json.id, json.result);
         };

         var output_result = function (id, string) {
             var $div = $("#termId" + id);
             $div.append("<div class=eliza>" +massage_msg(string) + "</div>");
             massage_term_div($div);
             scrollTo($div);
         };

         var make_term_url = function (msg) {
             var l = window.location;
             return l.protocol + "//" + l.host + l.pathname + "#" + msg;  
         };

         var replace_regex_tokens = function (match, msg_name, msg_val, offset, string) {
             var enc_msg = encodeURIComponent(msg_val);
             var url = make_term_url(enc_msg);
             return "<a class='termLink' href=\"" + url + "\"'>" 
                 + msg_name + "</a>";
         };

         var term_click = function (e) {
             e.preventDefault();
             var msg = decode_hash(e.target.hash);
             if(msg) {
                 put_hash_msg(msg);
                 user_input(msg);
             }
             focus_on_input();
             return false; //for good measure
         };

         var massage_term_div = function ($div) {
             $(".termLink", $div).click(term_click);
         };

         var massage_msg = function (msg) {
             return msg.replace(/_-(.*?)-__-(.*?)-_/g, replace_regex_tokens);
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

         var decode_hash = function (msg) {
             return decodeURIComponent(msg.slice(1));
         };

         var put_hash_msg = function (msg) {
             window.location.replace("#" + encodeURIComponent(msg));
         };

         var ltrim = function () {
             return this.replace(/^\s+/, '');
         };

         var rtrim = function () {
             return this.replace(/\s+$/, '');
         };

         var trim = function () {
             return this.ltrim().rtrim();
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
