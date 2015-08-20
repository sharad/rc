

if (false) {
//{{ - obsolete
function org_remember(url, title, text, window) {
    var eurl = encodeURIComponent(url);
    var etitle = encodeURIComponent(title);
    var etext = encodeURIComponent(text);
    var cmd_str = "emacsclient -f ~/.emacs.d/server/general -c org-protocol://remember://" + eurl + "/" + etitle + "/" + etext;
    window.minibuffer.message("Issuing " + cmd_str);
    shell_command_blind(cmd_str);
}

interactive("org-remember", "Remember the current url with org-remember",
        function (I) {
          org_remember(I.buffer.display_uri_string,
                       I.buffer.document.title,
                       I.buffer.top_frame.getSelection(),
                       I.window);
        });
//}}
}

//{{ latest
// Conkeror setup

// Setting up org-protocol in Conkeror (an emacs inspired Mozilla web
// browser) requires a slightly different method. You may simply add
// the following snippets of code to your .conkerorrc file.2

// For org-store-link, add the following to .conkerorrc:
var emacs_cmd="emacsclient -f ~/.emacs.d/server/general"

function org_store_link (url, title, window) {
    var cmd_str = emacs_cmd + ' ' + '\"org-protocol://store-link://'+url+'/'+title+'\"';
    if (window != null) {
        window.minibuffer.message('Issuing ' + cmd_str);
    }
    shell_command_blind(cmd_str);
}

interactive("org-store-link", "Stores [[url][title]] as org link and copies url to emacs kill ring",
            function (I) {
                org_store_link(encodeURIComponent(I.buffer.display_uri_string),
                               encodeURIComponent(I.buffer.document.title),
                               I.window);
            });

//  For org-capture (or org-remember — just exchange capture with remember), use the following:

function org_capture (url, title, selection, window) {
    var cmd_str = emacs_cmd + ' ' + '\"org-protocol://capture://'+url+'/'+title+'/'+selection+'\"';
    if (window != null) {
        window.minibuffer.message('Issuing ' + cmd_str);
    }
    shell_command_blind(cmd_str);
}

interactive("org-capture", "Clip url, title, and selection to capture via org-protocol",
            function (I) {
                org_capture(encodeURIComponent(I.buffer.display_uri_string),
                            encodeURIComponent(I.buffer.document.title),
                            encodeURIComponent(I.buffer.top_frame.getSelection()),
                            I.window);
            });

// Now, you should be able to invoke the commands from within conkeror
// with M-x org-store-link and M-x org-capture (or remember).

// Or, if you'd like your familiar emacs keybindings, you can add the
// following to your .conkerorrc:

define_key(content_buffer_normal_keymap, "C-c r", "org-capture");
define_key(content_buffer_normal_keymap, "C-c l", "org-store-link");

//}}
