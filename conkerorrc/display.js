// -*- mode: js2 -*-



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
                    session_pref("browser.display.use_system_colors", false)
                    session_pref(p, false);
                }
                else { // no custom
                    session_pref("browser.display.background_color", "#000000");
                    session_pref("browser.display.foreground_color", "#FFFFFF");
                    session_pref("browser.display.use_system_colors", true)
                    session_pref(p, true);
                }
            });
define_key(content_buffer_normal_keymap, "f6", "colors-toggle");

if (false ) {
user_pref(   "browser.display.use_system_colors",   false);
session_pref("browser.display.use_document_colors", false);
user_pref("browser.display.background_color", "#000000"); // BLACK

}
// Next issue is with input elements. To override it's colors you have to use this CSS hack.
// Create a file ~/.conkeror.css with this content

// To put this file in use, load this file in the .conkerorrc like this



// register_user_stylesheet('file://' + get_home_directory().path + "/.conkerorrc/conkeror.css");

if (false)
let (mycss = get_home_directory().path + "/.conkerorrc/conkeror.css") {
    register_user_stylesheet('file://' + mycss);
    // To toggle the style sheet's' active state:
    var global_css_registered=true;
    function toggle_global_css(I){
        global_css_registered=global_css_registered ? false : true;
        if(global_css_registered){
            register_user_stylesheet('file://' + mycss);
        }else{
            unregister_user_stylesheet('file://' + mycss);
        }
    }
    interactive("toggle-global-css", "Toggle global.css", toggle_global_css);
    define_key(default_global_keymap, "C-t", "toggle-global-css");
}

// add_hook("window_initialize_late_hook",function(){session_pref("browser.display.use_document_colors", false);});


function saveeyes() {
    session_pref("browser.display.use_document_colors", false);
    setTimeout(saveeyes,1000);
}

if (false)
    setTimeout(saveeyes,1000);

//{{ Theming
// http://truongtx.me/2012/12/27/conkeror-display-tab-bar/
// http://conkeror.org/Appearance#Theming
theme_load_paths.unshift("~/.conkerorrc/themes/");
theme_unload("default");
theme_load("tommytxtruong");





//}}
