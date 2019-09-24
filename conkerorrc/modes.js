// from http://stackoverflow.com/questions/768263/what-does-your-conkerorrc-look-like

// page modes
require("page-modes/google-search-results.js"); // google search results
require("page-modes/wikipedia.js");     // wikipedia mode

//{{
// load session module
// http://conkeror.org/Sessions
require("session.js");
session_auto_save_auto_load = true; // auto-load session
// session_auto_save_auto_load = "prompt"; // auto-load session
// Setup how long in days, history entries are kept before being automatically expired.
session_pref('browser.history_expire_days', 30);

function session_stop_loading_buffers(window) {
    if (!window) window = get_recent_conkeror_window();
    window.alert("Now Running session_stop_loading_buffers.")
    for (var i = 0; i < window.buffers.count; i++)
    {
        stop_loading( window.buffers.get_buffer(i) );
    }
}

let _session_stop_loading_buffers = function (window) {
    remove_hook("window_initialize_late_hook", _session_stop_loading_buffers);
    // spawn(_session_auto_save_auto_load(user_gave_urls));
    // session_stop_loading_buffers();
    window = get_recent_conkeror_window();
    if (window) {
        window.setTimeout(session_stop_loading_buffers, 2);
        window.alert("Will run session_stop_loading_buffers after 2 secs.")
    }
};

add_hook("window_initialize_late_hook", _session_stop_loading_buffers);

session_stop_loading_buffers();

//}}

// tab bar
require("new-tabs.js");

// clicks in new buffer
require("clicks-in-new-buffer.js");
// Set to either OPEN_NEW_BUFFER(_BACKGROUND)
clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND; // Now buffers open in background.




// don't open download buffer automatically
remove_hook("download_added_hook", open_download_buffer_automatically);

// don't show clock
remove_hook("mode_line_hook", mode_line_adder(clock_widget));

// add favicons
require("favicon");
add_hook("mode_line_hook", mode_line_adder(buffer_icon_widget), true);
read_buffer_show_icons = true;

// add content handlers
content_handlers.set("application/pdf", content_handler_prompt);
content_handlers.set("application/torrent", content_handler_save); // pdf
content_handlers.set("application/mp3", content_handler_save); // pdf
content_handlers.set("application/ogg", content_handler_save); // pdf
// torrent
// mp3
// ogg

function define_switch_buffer_key (key, buf_num) {
    define_key(default_global_keymap, key,
               function (I) {
           switch_to_buffer(I.window,
                                    I.window.buffers.get_buffer(buf_num));
               });
}
for (let i = 0; i < 10; ++i) {
    define_switch_buffer_key(String((i+1)%10), i);
}

function enable_scrollbars (buffer) {
    buffer.top_frame.scrollbars.visible = true;
}
add_hook("create_buffer_late_hook", enable_scrollbars);

//{{
for (var i = 0; i < get_recent_conkeror_window().buffers.count; i++)
{
  stop_loading(get_recent_conkeror_window().buffers.get_buffer(i));
}

add_hook("window_initialize_late_hook",
         function (window) {
           for (var i = 0; i < window.buffers.count; i++)
           {
             stop_loading(window.buffers.get_buffer(i));
           }
         });
//}}
