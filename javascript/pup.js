var pup = 
    (function ($) {
         // var
         var rpc_id=0;

         // fns
         var jrpc = function(url, id, method, params, success, error) {
             console.log("rpc-ing");
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
         
         var init = function($) {
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

         // fns
         var init = function (an_rpc, textIn, domOut) {
             $in_form = textIn;
             $in_input = $(":input", $in_form);
             $out = domOut;
             rpc = an_rpc;
             $in_input[0].focus();
             $in_form.submit(input_submit);
         };

         var input_submit = function (e) {
             var val = $in_input.val();
             var id = rpc(val);
             print_user(val, id);
             $in_input.val("");
             return false;
             scrollTo("#top");
         };

         var print_user = function (string, id) {
             $out.append("<div class=user id='termId" + id + "'>> " 
                         + string + "</div>");
             scrollTo("termId" + id);
         };

         var print_result = function (json) {
             _print_result(json.id, json.result);
         };

         var _print_result = function (id, string) {
             $out.append("<div class=eliza>" + string + "</div>");
             scrollTo("termId" + id);
             $(".termClick").click(function(event){
                                       event.preventDefault();
                                       // Ajax here
                                       return false; //for good measure
                                   });
         };


         var pause = function () {
             
         };

         var resume = function () {
             
         };

         var error = function (id, string) {
             _print_result(id, string);
         };

         // helpers
         function scrollTo(id){
             $("html,body").animate(
                 { scrollTop: $("#"+id).offset().top},
                 "slow");
         }

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
