
// see http://conkeror.org/UserVariables
require("clicks-in-new-buffer.js")
// browser_default_open_target = OPEN_NEW_BUFFER_BACKGROUND;
// browser_default_open_target = OPEN_NEW_BUFFER;
browser_default_open_target = OPEN_CURRENT_BUFFER;
// clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND;
// clicks_in_new_buffer_target = OPEN_NEW_BUFFER;
clicks_in_new_buffer_target = OPEN_CURRENT_BUFFER;
// download_buffer_automatic_open_target = OPEN_NEW_BUFFER_BACKGROUND;
download_buffer_automatic_open_target = 100;

///{{{ temporary fix
// http://bugs.conkeror.org/issue514
// if (false)
// {
// browser_dom_window.prototype = {
//     constructor: browser_dom_window,
//     QueryInterface: generate_QI(Ci.nsIBrowserDOMWindow),

//     openURI: function (aURI, aOpener, aWhere, aContext) {
//         // Reference: http://www.xulplanet.com/references/xpcomref/ifaces/nsIBrowserDOMWindow.html
//         var target = this.next_target;
//         if (target == null || target == FOLLOW_DEFAULT)
//             target = browser_default_open_target;
//         this.next_target = null;

//         /* Determine the opener buffer */
//         var opener = get_buffer_from_frame(this.window, aOpener);
//         if (aOpener) {
//             this.browser.presetOpenerWindow(aOpener);
//         }

//         switch (browser_default_open_target) {
//         case OPEN_CURRENT_BUFFER:
//             return aOpener.top;
//         case FOLLOW_CURRENT_FRAME:
//             return aOpener;
//         case OPEN_NEW_BUFFER:
//             var buffer = new content_buffer(this.window, $opener = opener);
//             this.window.buffers.current = buffer;
//             return buffer.top_frame;
//         case OPEN_NEW_BUFFER_BACKGROUND:
//             var buffer = new content_buffer(this.window, $opener = opener);
//             return buffer.top_frame;
//         case OPEN_NEW_WINDOW:
//         default: /* shouldn't be needed */

//             /* We don't call make_window here, because that will result
//              * in the URL being loaded as the top-level document,
//              * instead of within a browser buffer.  Instead, we can
//              * rely on Mozilla using browser.chromeURL. */
//             window_set_extra_arguments(
//                 {initial_buffer_creator: buffer_creator(content_buffer, $opener = opener)}
//             );
//             return null;
//         }
//     }
// };
// }
///}}}


// http://conkeror.org/History

// History completion

// The way to browse to an url from your history in Conkeror is in the
// find-url prompt. For historical reasons having to do with a Mozilla
// bug, history urls are not automatically included in the completions
// list. To have the find-url prompt included history urls in its
// completions, set this in your rc:

url_completion_use_history = true;

// If you prefer not to have history mixed in with webjumps and
// bookmarks, you can create a separate command with its own bindings
// for it instead. The following code creates a command to just browse
// history, with separate bindings top open it in the existing buffer
// or a new buffer.

define_browser_object_class(
    "history-url", null,
    function (I, prompt) {
        check_buffer (I.buffer, content_buffer);
        var result = yield I.buffer.window.minibuffer.read_url(
            $prompt = prompt,  $use_webjumps = false, $use_history = true, $use_bookmarks = false);
        yield co_return (result);
    });

interactive("find-url-from-history",
            "Find a page from history in the current buffer",
            "find-url",
            $browser_object = browser_object_history_url);

interactive("find-url-from-history-new-buffer",
            "Find a page from history in the current buffer",
            "find-url-new-buffer",
            $browser_object = browser_object_history_url);

define_key(content_buffer_normal_keymap, "h", "find-url-from-history-new-buffer");
define_key(content_buffer_normal_keymap, "H", "find-url-from-history");

// Sorting

// See url_completion_sort_order for information on
// url_completion_sort_order. The constants listed can be used in
// read_url with $sort_order per the example above.

// History clearing preferences

// The preference browser.history_expire_days determines how long in
// days history entries are kept before being automatically expired.

session_pref('browser.history_expire_days', 10);

///////////////// Similarly bookmark ///////////////////////

define_browser_object_class(
    "bookmark-url", null,
    function (I, prompt) {
        check_buffer (I.buffer, content_buffer);
        var result = yield I.buffer.window.minibuffer.read_url(
            $prompt = prompt,  $use_webjumps = false, $use_history = false, $use_bookmarks = true);
        yield co_return (result);
    });

interactive("find-url-from-bookmark",
            "Find a page from bookmark in the current buffer",
            "find-url",
            $browser_object = browser_object_bookmark_url);

interactive("find-url-from-bookmark-new-buffer",
            "Find a page from bookmark in the current buffer",
            "find-url-new-buffer",
            $browser_object = browser_object_bookmark_url);

define_key(content_buffer_normal_keymap, "k", "find-url-from-bookmark-new-buffer");
define_key(content_buffer_normal_keymap, "K", "find-url-from-bookmark");
///////////////// Similarly bookmark ///////////////////////


///////////////// find id attribute to get exact link. /////////////////

define_browser_object_class("id", null,
    xpath_browser_object_handler("//*[@id]"),
    $hint = "select search result");
define_key(content_buffer_normal_keymap, "* x", "browser-object-id");

///////////////// find id attribute to get exact link. /////////////////



/////////////////////////////////////////////////////////////

function possibly_valid_url (str) {
    return (/[\.\/:]/.test(str)) &&
        !(/\S\s+\S/.test(str)) &&
        !(/^\s*$/.test(str));
}

// excellent
// http://conkeror.org/UserVariables see read_url_handler_list
read_url_handler_list = [read_url_make_default_webjump_handler("lucky")];

// /////////////////////////////////////////////////////////////////////
// define_browser_object_class("paste-url",
//     "Browser object which reads an url from the X Primary Selection, "+
//     "falling back on the clipboard for operating systems which lack one.",
//     function (I, prompt) {
// 		var url = read_from_x_primary_selection();
// 		// trim spaces
// 		url = url.replace(/^\s*|\s*$/,"");
// 		// add http:// if needed
// 		if (url.match(/^[^:]+\./)) {
// 			url = "http://" + url;
// 		}
//         try {
//             return make_uri(url).spec;
//         } catch (e) {
//             var result = yield I.buffer.window.minibuffer.try_read_url_handlers(url);
//             // return make_uri(I.buffer.window.minibuffer.try_read_url_handlers(url)).spec;
//             yield co_return(result);
//             // throw new interactive_error("errorXX: malformed url: "+url);
//         }
//     });

// interactive("paste-url", "Open a URL from the clipboard in the current buffer.",
// 	    alternates(follow_current_buffer, follow_new_buffer, follow_new_window),
// 	    $browser_object = browser_object_paste_url);

// interactive("paste-url-new-buffer", "Open a URL from the clipboard in a new buffer.",
// 	    alternates(follow_new_buffer, follow_new_window),
// 	    $browser_object = browser_object_paste_url);

// interactive("paste-url-new-window", "Open a URL from the clipboard in a new window.",
// 	    follow_new_window,
// 	    $browser_object = browser_object_paste_url);

/////////////////////////////////////////////////////////////////////



// http://www.surf-proxy.de/index.php?q=http%3A%2F%2Fconkeror.org%2FContentHandlers
content_handlers.set("application/pdf", content_handler_prompt);


//http://conkeror.org/NoScript
require("noscript");
// After performing the above require, a function M-x ns-toggle-temp
// is defined, which will enable/disable javascript for the current
// site/page after prompting for confirmation.


//{{ Using Esc key in Conkeror [https://truongtx.me/2013/08/08/using-esc-key-in-conkeror/]
require("global-overlay-keymap");
define_key_alias("C-o", "escape");
//}}

//{{ NOT using https://truongtx.me/2012/12/30/conkeror-get-tinyurl-for-the-current-page/
// get tiny url for the current page
// press * q and then c to generate and copy the tinyurl into clipboard

interactive("tinyurl",
            "tinyurl",
            function (I, prompt) {
              var element = yield read_browser_object(I);
              browser_set_element_focus(I.buffer, element);
              var text = browser_element_text(I.buffer, element);

              let createurl = 'http://tinyurl.com/api-create.php?url=' + encodeURIComponent( text );
              try {
                var content = yield send_http_request(
                  load_spec({uri: createurl}));

                writeToClipboard(content.responseText);
                I.buffer.window.minibuffer.message("Copied: " + content.responseText);

              } catch (e) { }

            },
            $browser_object = browser_object_links);

//}}




//{{ searchfollow
function searchfollow (I, target) {
    var searchUrl = "http://google.com/search?btnI=I%27m+Feeling+Lucky&q=";
    var navclient = "&sourceid=navclient";
    if (target == null)
        target = FOLLOW_DEFAULT;
    I.target = target;
    if (target == OPEN_CURRENT_BUFFER)
        check_buffer(I.buffer, content_buffer);
    var element = yield read_browser_object(I);
    try {
        element = load_spec(element);
        if (I.forced_charset)
            element.forced_charset = I.forced_charset;
    } catch (e) {}
    var text = browser_element_text(I.buffer, element);
    let createurl = searchUrl + encodeURIComponent( text ) + navclient;
    var selement  = load_spec({uri: createurl});

    browser_object_follow(I.buffer, target, selement);
}

// var searchUrl = "http://google.com/search?btnI=I%27m+Feeling+Lucky&q=";
// var navclient = "";// "&sourceid=navclient";

function searchfollow (I, target) {
    var searchUrl = "http://www.google.com/search?btnI&q=";
    var navclient = "";
    if (target == null)
        target = FOLLOW_DEFAULT;
    I.target = target;
    if (target == OPEN_CURRENT_BUFFER)
        check_buffer(I.buffer, content_buffer);
    var element = yield read_browser_object(I);
    var text     = browser_element_text(I.buffer, element);
    let createurl = searchUrl + encodeURIComponent( text ) + navclient;
    var selement  = load_spec({uri: createurl});

    browser_object_follow(I.buffer, target, selement);
}

function searchfollow_new_buffer (I) {
    yield searchfollow(I, OPEN_NEW_BUFFER);
}

function searchfollow_new_buffer_background (I) {
    yield searchfollow(I, OPEN_NEW_BUFFER_BACKGROUND);
}

function searchfollow_new_window (I) {
    yield searchfollow(I, OPEN_NEW_WINDOW);
}

function searchfollow_current_frame (I) {
    yield searchfollow(I, FOLLOW_CURRENT_FRAME);
}

function searchfollow_current_buffer (I) {
    yield searchfollow(I, OPEN_CURRENT_BUFFER);
}
interactive("searchfollow", null,
    alternates(searchfollow, searchfollow_new_buffer, searchfollow_new_window),
    $browser_object = browser_object_links);

interactive("searchfollow-top", null,
    alternates(searchfollow_current_buffer, searchfollow_current_frame),
    $browser_object = browser_object_frames,
    $prompt = "Searchfollow");

interactive("searchfollow-new-buffer",
    "Searchfollow a link in a new buffer",
    alternates(searchfollow_new_buffer, searchfollow_new_window),
    $browser_object = browser_object_links,
    $prompt = "Searchfollow");

interactive("searchfollow-new-buffer-background",
    "Searchfollow a link in a new buffer in the background",
    alternates(searchfollow_new_buffer_background, searchfollow_new_window),
    $browser_object = browser_object_links,
    $prompt = "Searchfollow");

interactive("searchfollow-new-window",
    "Searchfollow a link in a new window",
    searchfollow_new_window,
    $browser_object = browser_object_links,
    $prompt = "Searchfollow");

interactive("searchfind-url", "Open a URL in the current buffer",
    alternates(searchfollow_current_buffer, searchfollow_new_buffer, searchfollow_new_window),
    $browser_object = browser_object_url);

interactive("searchfind-url-new-buffer",
    "Open a URL in a new buffer",
    alternates(searchfollow_new_buffer, searchfollow_new_window),
    $browser_object = browser_object_url,
    $prompt = "Searchfind url");

interactive("searchfind-url-new-window", "Open a URL in a new window",
    searchfollow_new_window,
    $browser_object = browser_object_url,
    $prompt = "Searchfind url");

interactive("searchfind-alternate-url", "Edit the current URL in the minibuffer",
    "searchfind-url",
    $browser_object =
        define_browser_object_class("alternate-url", null,
            function (I, prompt) {
                check_buffer(I.buffer, content_buffer);
                var result = yield I.buffer.window.minibuffer.read_url(
                    $prompt = prompt,
                    $initial_value = I.buffer.display_uri_string);
                yield co_return(result);
            }),
            $prompt = "Searchfind url");

define_key(content_buffer_normal_keymap, "S", "searchfollow");
// define_key(content_buffer_normal_keymap, "H", "searchfollow");

//}}
