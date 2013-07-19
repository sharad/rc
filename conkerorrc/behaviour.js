
// http://conkeror.org/History

// History completion

// The way to browse to an url from your history in Conkeror is in the
// find-url prompt. For historical reasons having to do with a Mozilla
// bug, history urls are not automatically included in the completions
// list. To have the find-url prompt included history urls in its
// completions, set this in your rc:

// url_completion_use_history = true;

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











