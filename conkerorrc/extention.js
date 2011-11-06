// 29. Firebug Lite

// The following code provides a firebug command which will launch
// Firebug Lite in the current page. To improve load time, and/or use
// Firebug Lite offline, refer to the section Using Firebug Lite
// Offline at http://getfirebug.com/firebuglite.

define_variable("firebug_url",
    "http://getfirebug.com/releases/lite/1.2/firebug-lite-compressed.js");

function firebug (I) {
    var doc = I.buffer.document;
    var script = doc.createElement('script');
    script.setAttribute('type', 'text/javascript');
    script.setAttribute('src', firebug_url);
    script.setAttribute('onload', 'firebug.init();');
    doc.body.appendChild(script);
}
interactive("firebug", "open firebug lite", firebug);




// {{ http://conkeror.org/WebDevelopment

// Using Conkeror for Web Development
// Script Injection
// You can load any toolkit you want into a page by adding a <script> tag to the I.buffer.document object.
// JQuery

// For example, you can load JQuery from Google Ajaxlibs without prior planning:

function install_jquery(d) {
    var script_el = d.createElementNS(XHTML_NS, "script");
    script_el.setAttribute("language", "javascript");
    script_el.setAttribute("type", "text/javascript");
    script_el.setAttribute("src", "http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js");
    d.body.appendChild(script_el);
}

function jquery_this_doc(d, js_code) {
    install_jquery(d);
    var script_el = d.createElementNS(XHTML_NS, "script");
    script_el.textContent = js_code;
    d.body.appendChild(script_el);
}

interactive("jquery-here",
            "load jquery.js into this page, then your command",
            function(I) {
                jquery_this_doc(I.buffer.document, (yield I.minibuffer.read($prompt = "jq: ", $history = "jquery-here")));
                // Providing a $history is what allows editing/replay of earlier commands
            });

// To use this,

//   M-x jquery-here
//   jq: $("h1").css("backgroundColor", "blue");

// should turn all of the H1 headings blue. The convenient bit is that
// you can re-run the command and edit it in place, just like any
// other minibuffer-prompt, letting you experiment with selectors
// "live"; just reload the page to undo everything and start again.

// Hey, use it for hiding '#sidebar' $('#sidebar-left').toggle();
// in blog post http://drupal.org/node/547970


// }}
