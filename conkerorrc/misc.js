

// {{ http://conkeror.org/CookieCuller
const cookie_culler_chrome = "chrome://cookieculler/content/CookieCuller.xul";

interactive("cookie-culler-dialog", "Show the CookieCuller settings in a dialog box.",
    function (I) {
        var frame = I.buffer.top_frame;
        frame.openDialog(cookie_culler_chrome,
                         "CookieCuller",
                         "centerscreen,chrome,dialog,modal,resizable");
    });

interactive("cookie-culler", "Open the CookieCuller settings in a new buffer.",
    "find-url-new-buffer",
    $browser_object = cookie_culler_chrome);
// }}




//{{

interactive("cleartrack", "Google page mode clear track.",
            function (I) {
                var buf = I.buffer;
                var elem = buf.document.querySelector("//a[@class='l']|//a[@class='l vst']|//a[@class='gs-title']|//h3[@class='r']/a");
                I.minibuffer.message(elem.length + "test");
                if (elem)
                    dom_node_click(elem, 1, 1);
                else
                    I.minibuffer.message(error_message);
            });

//}}


//{{ check http://www.emacswiki.org/emacs/BrowseUrl#toc18
url_remoting_fn = load_url_in_new_buffer;
//}}


//{{ https://github.com/ivoarch/.dot-org-files/blob/master/conkeror.org
url_completion_use_history = true;
url_completion_use_bookmarks = true;
url_completion_use_webjumps = true;
minibuffer_auto_complete_default = true;

// Enabling the Password Manager

session_pref("signon.rememberSignons", true);
session_pref("signon.expireMasterPassword", false);
session_pref("signon.SignonFileName", "signons.txt");
Components.classes['@mozilla.org/login-manager;1']
    .getService(Components.interfaces.nsILoginManager);

// Modeline
// Widgets

// I disabled only the clock widget, everything else is enabled.

require("mode-line.js");
// remove_hook("mode_line_hook", mode_line_adder(clock_widget));
add_hook("mode_line_hook", mode_line_adder(buffer_icon_widget), true);
add_hook("mode_line_hook", mode_line_adder(loading_count_widget), true);
add_hook("mode_line_hook", mode_line_adder(buffer_count_widget), true);
add_hook("mode_line_hook", mode_line_adder(zoom_widget));
// http://bugs.conkeror.org/issue495
// add_hook("mode_line_hook", mode_line_adder(downloads_status_widget));

// Favicons
// Activate the favicons in the modeline.
require("favicon.js");
add_hook("mode_line_hook", mode_line_adder(buffer_icon_widget), true);
read_buffer_show_icons = true;

// Hinting
// Displaying the url of a link in hints mode.
hints_display_url_panel = true;
// Display information about the currently selected hint.
hints_minibuffer_annotation_mode(true);
//}}

// Local Variables: **
// folded-file:f **
// mode:js2 **
// comment-column:0 **
// comment-start: "// "  **
// comment-end:   "// "  **
// End: **
