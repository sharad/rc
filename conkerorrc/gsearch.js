/**
 * (C) Copyright 2008 Jeremy Maitin-Shepard
 * (C) Copyright 2009-2010 John J. Foerch
 *
 * Use, modification, and distribution are subject to the terms specified in the
 * COPYING file.
**/

require("content-buffer.js");


define_keymap("google_search_results_keymap", $display_name = "google-search-results");

// Keys for the "experimental" keyboard search
define_key(google_search_results_keymap, "j", "ensure-content-focused", $fallthrough);
define_key(google_search_results_keymap, "k", "ensure-content-focused", $fallthrough);
define_key(google_search_results_keymap, "o", "ensure-content-focused", $fallthrough);
define_key(google_search_results_keymap, "/", "ensure-content-focused", $fallthrough);
define_key(google_search_results_keymap, "return", "ensure-content-focused", $fallthrough);//BAD

/**
 * Note: escape already does the same thing as the Google key binding.
 */

define_browser_object_class("google-search-results-links", null,
    xpath_browser_object_handler("//a[@class='l']|//a[@class='l vst']|//a[@class='gs-title']|//h3[@class='r']/a"),
    $hint = "select search result");


// Bind keys 1 through 9 to follow corresponding results links
//
define_browser_object_class("google-search-result-by-digit", null,
    function (I, prompt) {
        var doc = I.buffer.document;
        var digit = I.event.charCode - 48;
        var res = doc.evaluate("//a[parent::node()/@class='r']", doc, null,
                               Ci.nsIDOMXPathResult.ORDERED_NODE_SNAPSHOT_TYPE,
                               null);
        yield co_return(res.snapshotItem(digit - 1));
    });

function google_search_bind_number_shortcuts () {
    for (var j = 1; j <= 9; j++) {
        let o = j;
        var function_name = "gsearch-follow-result-" + j;
        interactive(function_name,
                    "Follow google search result number " + j,
                    "follow",
                    $browser_object = browser_object_google_search_result_by_digit);
        define_key(google_search_results_keymap, String(j), function_name);
    }
}


var google_search_results_link_commands =
    ["follow", "follow-new-buffer", "follow-new-buffer-background",
     "follow-new-window", "save", "copy", "shell-command-on-file"];

var google_search_results_modality = {
    normal: google_search_results_keymap
};


function docEvaluateArray (expr, doc, context, resolver) {
    var i, result, a = [];
    doc = doc || (context ? context.ownerDocument : document);
    resolver = resolver || null;
    context = context || doc;

    result = doc.evaluate(expr, context, resolver, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
    for(i = 0; i < result.snapshotLength; i++) {
        a[i] = result.snapshotItem(i);
    }
    return a;
}

// while(anchor = doc.evaluate("//a[@class='l']|//a[@class='l vst']|//a[@class='gs-title']|//h3[@class='r']/a", doc, null, XPathResult.ANY_TYPE, null).iterateNext()) {
// get_recent_conkeror_window().alert("hello world");
// void(correctlink(buffer.document));

// var iterator = document.evaluate('//phoneNumber', documentNode, null, XPathResult.UNORDERED_NODE_ITERATOR_TYPE, null );
// var iterator = doc.evaluate("//a", doc, null, XPathResult.ANY_TYPE, null);


// while(anchor = doc.evaluate("//a", doc, null, XPathResult.ANY_TYPE, null).iterateNext()) {
//     anchor.href = "http://asdfsdf.com";
//     get_recent_conkeror_window().alert("hello world " + anchor);
// }
//        dump( 'Error: Document tree modified during iteration ' + e );
//     var iterator = doc.evaluate("//a", doc, null, XPathResult.UNORDERED_NODE_ITERATOR_TYPE, null);

//get_recent_conkeror_window().alert("HELLO WORLD11");

//             get_recent_conkeror_window().alert( thisNode.textContent );
//         get_recent_conkeror_window().alert("hello world22" + e);


function correctlink(buffer) {
    get_recent_conkeror_window().alert("hello world22" + docEvaluateArray);

    var els = docEvaluateArray('//a');


}

// var iterator = doc.evaluate("//a[@class='l']|//a[@class='l vst']|//a[@class='gs-title']|//h3[@class='r']/a", doc, null, 0, null);


// http://www.google.co.in/url?q=http://mozdev.org/pipermail/conkeror/2013-February/003215.html&sa=U&ei=hwrkUb-aEs-fkgXT3YCoDA&ved=0CC0QFjAF&sig2=c3sGHnf4nh34_09TI3Acdg&usg=AFQjCNFgyxoyeeqfRx0WkeQxPHOEJlvjiw
// return link.replace("/url\?q=(.+)&/", "/\1/.");
//     return (unescape(link.match("url?q=([^&]*)")[1]));
// link.match("url\?q=([^&]*)") ;

// var x = (link.match("url\?q=([^&]*)")[1]);
        // var x =rredirect.match(url);

    // var rredirect = /\/url\?(?:url|q)=([^&]*)/i;
    // if (rredirect.test(link)) {
    //     get_recent_conkeror_window().alert("hello world22 ");
    // } else
    //     var x = link;

    return link.match(, "sdfdsf");


function cleanlink(link) {

    var re1 = new RegExp("\/url\?(?:url|q)=([^&]*)");
    if (re1.test(link))
        get_recent_conkeror_window().alert("matched ... ");

    return link;

}

function cleanpage(buffer) {
    doc = buffer.document;

    var iterator = doc.evaluate("//a", doc, null, 0, null);

    try {
        var thisNode = iterator.iterateNext();
        while (thisNode) {
            thisNode = iterator.iterateNext();
            thisNode.href = cleanlink(thisNode.href)
        }
    }
    catch (e) {
    }
    get_recent_conkeror_window().alert("hello world22");

}


define_page_mode("google-search-results-mode",

                 build_url_regexp($domain = "google",
                                  $allow_www = true,
                                  $path = /search\?|cse\?/,
                                  $tlds = ["com", "com.au", "co.uk", "de", "dk", "es", "fr", "it", "no", "se", "uk", "co.in", "in"]),

                 function enable (buffer) {


                     for each (var c in google_search_results_link_commands) {
                         buffer.default_browser_object_classes[c] =
                             browser_object_google_search_results_links;
                     }
                     buffer.content_modalities.push(google_search_results_modality);
                     add_hook.call(buffer, "buffer_dom_content_loaded_hook",
                                   cleanpage);

                 },

                 function disable (buffer) {
        for each (var c in google_search_results_link_commands) {
            delete buffer.default_browser_object_classes[c];
        }
        var i = buffer.content_modalities.indexOf(google_search_results_modality);
        if (i > -1)
            buffer.content_modalities.splice(i, 1);
    },

                 $display_name = "Sharad1 -- Google Search Results");

page_mode_activate(google_search_results_mode);

provide("google-search-results");

