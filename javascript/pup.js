var pup = 
    (function ($) {      
         var rpc_id=0;

         var terminal_rpc = function(input, terminal){
             if (input === '') {
                 return;
             } else {
                 terminal.pause();
                 $.jrpc('/ajax', rpc_id++, 'eliza', [input], function(json) {
                            if (!json.error) {
                                terminal.echo(json.result); 
                            } else {
                                terminal.error('&#91;RPC&#93; ' + json.error.message);
                            }
                            terminal.resume();
                        }, function(xhr, status, error) {
                            terminal.error('&#91;AJAX&#93; ' + status +
                                           ' - Server reponse is: \n' +
                                           xhr.responseText);
                            terminal.resume();
                        });    
             };
         };
         
         var init = function($) {
             $('body').terminal(terminal_rpc, {
                                    login: false,
                                    greetings: "<div class=\'eliza\'>Welcome to Sea Pup's cosy home<br/>Type 'help' for help</div>"
                                });
         };

         return {
             init: init
         };
         
     })(jQuery);