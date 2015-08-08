

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

// Local Variables: **
// folded-file:f **
// mode:js2 **
// comment-column:0 **
// comment-start: "// "  **
// comment-end:   "// "  **
// End: **
