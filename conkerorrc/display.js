// -*- mode: js2 -*-

// from: http://conkeror.org/Tips#Darkenthecurrentpage
// {{ follow-new-buffer
define_key(content_buffer_normal_keymap, "d", "follow-new-buffer-background");
// }}



//{{ darken_page: thanks very much for saving my eyes.
{
    // var newSS, styles='* { background: black ! important; color: grey !important }'
    // var newSS, styles='* { background: black ! important; color: white !important }'
    // var newSS, styles='* { background: #EFFFEF ! important; color: white !important }'
    // var newSS, styles='* { background: #428a42 ! important; color: white !important }'
    // var newSS, styles='* { background: DarkGreen ! important; color: white !important }'
    // var newSS, styles='* { background: DarkSlateGray  ! important; color: white !important }'
    function generate_darken_css_attribs(heavy)
    {
        var styles =
            // '* { background-color: Black  ! important; color: grey !important }'
            '* { background-color: Black ' + (heavy ? "!important" : "") + '; color: grey !important }'
            + ':link, :link * { color: #4986dd !important }'
            + ':visited, :visited * { color: #d75047 !important }';
        var attribs = { rel: "stylesheet", href: "data:text/css, " + escape(styles)};
        return attribs;
    }

    function createStyleElement(doc, id, type, heavy)
    {
        var attribs = generate_darken_css_attribs(heavy);
        var newEl = doc.createElement(type);
        newEl.id = id;
        for(var k in attribs) newEl[k] = attribs[ k ];
        return newEl;
    }

    function addCustomElement(doc, id, type, heavy)
    {
        doc.getElementsByTagName("head")[0]
            .appendChild( createStyleElement(doc, id, type, heavy) );
    }
    function removeCustomElement(doc, id, type)
    {
        var delEl = doc.getElementById(id);
        if (delEl) delEl.parentNode.removeChild(delEl);
    }
    function build_darken_id(heavy)
    {
        return ("_darken_page_" + (heavy ? "heavy" : "light") + "_");
    }
    function disable_local_darken_page(doc, heavy)
    {
        var id = build_darken_id(heavy);
        if (doc.getElementById(id))
            removeCustomElement(doc, id, "link");
    }
    function enable_local_darken_page(doc, heavy)
    {
        var id = build_darken_id(heavy);
        if (!doc.getElementById(id))
            addCustomElement(doc, id, "link", heavy);
    }
    function toggle_local_darken_page(doc, heavy)
    {
        var id = build_darken_id(heavy);
        if (!doc.getElementById(id))
            enable_local_darken_page(doc, heavy);
        else
            disable_local_darken_page(doc, heavy);
    }

    function disable_local_light_darken_page(I)
    {
        disable_local_darken_page(I.window.buffers.current.document, false);
    }
    interactive("disable-local-light-darken-page",
                "Darken the page in an attempt to save your eyes.",
                disable_local_light_darken_page);
    function enable_local_light_darken_page(I)
    {
        enable_local_darken_page(I.window.buffers.current.document, false);
    }
    interactive("enable-local-light-darken-page",
                "Darken the page in an attempt to save your eyes.",
                enable_local_light_darken_page);
    function toggle_local_light_darken_page(I)
    {
        toggle_local_darken_page(I.window.buffers.current.document, false);
    }
    interactive("toggle-local-light-darken-page",
                "Darken the page in an attempt to save your eyes.",
                toggle_local_light_darken_page);
    define_key(content_buffer_normal_keymap, "f1", "toggle-local-light-darken-page");

    function disable_local_heavy_darken_page(I)
    {
        disable_local_darken_page(I.window.buffers.current.document, true);
    }
    interactive("disable-local-heavy-darken-page",
                "Darken the page in an attempt to save your eyes.",
                disable_local_heavy_darken_page);
    function enable_local_heavy_darken_page(I)
    {
        enable_local_darken_page(I.window.buffers.current.document, true);
    }
    interactive("enable-local-heavy-darken-page",
                "Darken the page in an attempt to save your eyes.",
                enable_local_heavy_darken_page);
    function toggle_local_heavy_darken_page(I)
    {
        toggle_local_darken_page(I.window.buffers.current.document, true);
    }
    interactive("toggle-local-heavy-darken-page",
                "Darken the page in an attempt to save your eyes.",
                toggle_local_heavy_darken_page);
    define_key(content_buffer_normal_keymap, "f2", "toggle-local-heavy-darken-page");

    function is_local_light_darken_page(I)
    {
        I.window.minibuffer.message("index: " + conkeror['buffer_loaded_hook'].indexOf(toggle_local_light_darken_page));
    }
    interactive("is-local-light-darken-page",
                "Darken the page in an attempt to save your eyes.",
                is_local_light_darken_page);
// }
//}}


//{{
// {
    var local_light_darken_page_fn = enable_local_light_darken_page;
    function is_local_light_darken_page(I)
    {
        I.window.minibuffer.message("index: " + conkeror['buffer_loaded_hook'].indexOf(local_light_darken_page_fn));
    }
    interactive("is-local-light-darken-page",
                "Darken the page in an attempt to save your eyes.",
                is_local_light_darken_page);

    function disable_global_darken_page_internal(window)
    {
        var hook = conkeror['buffer_loaded_hook'];
        if (hook.indexOf(local_light_darken_page_fn) != -1) {
            register_user_stylesheet(
                make_css_data_uri(
                    [
                        "body {background-color: white; color: grey; color: grey !important; }",
                    ]
                ));
            remove_hook('buffer_loaded_hook', local_light_darken_page_fn, true, true);
            if (window)
            {
                disable_local_darken_page(window.buffers.current.document, true);
                disable_local_darken_page(window.buffers.current.document, false);
                window.minibuffer.message("removed");
            }
        }
    }
    function enable_global_darken_page_internal(window)
    {
        var hook = conkeror['buffer_loaded_hook'];
        if (hook.indexOf(local_light_darken_page_fn) == -1) {
            register_user_stylesheet(
                make_css_data_uri(
                    [
                        "body {background-color: black; color: white; color: white !important; }",
                    ]));
            add_hook('buffer_loaded_hook', local_light_darken_page_fn, true, true);
            if (window)
            {
                enable_local_darken_page(window.buffers.current.document, true);
                enable_local_darken_page(window.buffers.current.document, false);
                window.minibuffer.message("added");
            }
        }
    }
    function toggle_global_darken_page_internal(window)
    {
        var hook = conkeror['buffer_loaded_hook'];
        if (hook.indexOf(local_light_darken_page_fn) == -1) {
            enable_global_darken_page_internal( window );
        } else {
            disable_global_darken_page_internal( window );
        }
    }
    function toggle_global_darken_page(I)
    {
        toggle_global_darken_page_internal( I.window );
    }
    interactive("toggle-global-darken-page",
                "Darken the page in an attempt to save your eyes.",
                toggle_global_darken_page);

    define_key(content_buffer_normal_keymap, "f7", "toggle-global-darken-page");
    toggle_global_darken_page_internal(); // enable it.
}
//}}

//{{ CSS
// http://conkeror.org/Tips#Renderthewebpagewithdefault.28custom.29colors
// 4.8. Render the web page with default (custom) colors

// This is how you can force xulrunner to render the web page with
// default black on white coloring.  First thing is to disable use of
// system colors in your conkeror. You can do that in 3 different
// ways:

//     in profile in
//     ~/.conkeror.mozdev.org/conkeror/$PROFILE_ID/prefs.js by setting
//     these options. You better quit conkeror before making these
//     changes to the file.  in about:config which will be saved in
//     prefs.js in .conkerorrc (prefered way).

if (false ) {
    user_pref("browser.display.use_system_colors", false);
    user_pref("browser.active_color", "#EE0000");
    user_pref("browser.anchor_color", "#0000EE");
    user_pref("browser.display.background_color", "#000000");
    user_pref("browser.display.foreground_color", "#FFFFFF");
    user_pref("browser.display.background_color", "#FFFFFF");
    user_pref("browser.display.foreground_color", "#000000");
    user_pref("browser.visited_color", "#551A8B");
}
// And you can have make it possible to toggle between default and document colors:

interactive("colors-toggle", "toggle between document and forced colors",
            function (I) {
                var p = "browser.display.use_document_colors";
                if (get_pref(p)) {
                    session_pref("browser.display.background_color", "#FFFFFF");
                    session_pref("browser.display.foreground_color", "#000000");
                    session_pref("browser.display.use_system_colors", false);
                    session_pref(p, false);
                    I.window.minibuffer.message("changes to false");
                }
                else {
                    session_pref("browser.display.background_color", "#000000");
                    session_pref("browser.display.foreground_color", "#FFFFFF");
                    session_pref("browser.display.use_system_colors", true);
                    session_pref(p, true);
                    I.window.minibuffer.message("changes to true");
                }
            });
define_key(content_buffer_normal_keymap, "f6", "colors-toggle");

// get_pref("browser.display.use_document_colors")

if (false ) {
    user_pref(   "browser.display.use_system_colors",   false);
    session_pref("browser.display.use_document_colors", false);
    user_pref("browser.display.background_color", "#000000"); // BLACK

}
// Next issue is with input elements. To override it's colors you have to use this CSS hack.
// Create a file ~/.conkeror.css with this content

// To put this file in use, load this file in the .conkerorrc like this


if (false) {
    // register_user_stylesheet('file://' + get_home_directory().path + "/.conkerorrc/conkeror.css");
    // "data:text/css,"+escape("input { background-color: white; color: black;-moz-appearance: none !important;}")

    // white
    register_user_stylesheet(make_css_data_uri(
        [
            "body {background-color: white; color: grey; color: grey !important; -moz-appearance: none !important; }",
            "input, textarea { background-color: black; color: white; -moz-appearance: none !important;}"
        ]
    ));

    // black
    register_user_stylesheet(make_css_data_uri(
        [
            "body {background-color: black; color: white; color: white !important; -moz-appearance: none !important; }",
            "input, textarea { background-color: white; color: black; -moz-appearance: none !important;}"
        ]
    ));

    // white
    register_user_stylesheet(make_css_data_uri(
        [
            "body {background-color: white; color: grey; color: grey !important; }",
        ]
    ));
    // black
    register_user_stylesheet(make_css_data_uri(
        [
            "body {background-color: black; color: white; color: white !important; }",
        ]
    ));
}

// if (false)
// let (mycss = get_home_directory().path + "/.conkerorrc/conkeror.css") {
//     register_user_stylesheet('file://' + mycss);
//     // To toggle the style sheet's' active state:
//     var global_css_registered=true;
//     function toggle_global_css(I){
//         global_css_registered=global_css_registered ? false : true;
//         if(global_css_registered){
//             register_user_stylesheet('file://' + mycss);
//         }else{
//             unregister_user_stylesheet('file://' + mycss);
//         }
//     }
//     interactive("toggle-global-css", "Toggle global.css", toggle_global_css);
//     define_key(default_global_keymap, "C-t", "toggle-global-css");
// }

// add_hook("window_initialize_late_hook",function(){session_pref("browser.display.use_document_colors", false);});


function saveeyes() {
    session_pref("browser.display.use_document_colors", false);
    setTimeout(saveeyes,1000);
}

if (false)
    setTimeout(saveeyes,1000);

//}}

//{{ Theming
// http://truongtx.me/2012/12/27/conkeror-display-tab-bar/
// http://conkeror.org/Appearance#Theming
theme_load_paths.unshift(get_home_directory().path + "/.conkerorrc/themes/");
theme_load("default");
theme_load("tommytxtruong");
//}}
