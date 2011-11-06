// from http://stackoverflow.com/questions/768263/what-does-your-conkerorrc-look-like

// page modes
require("page-modes/google-search-results.js"); // google search results
require("page-modes/wikipedia.js");     // wikipedia mode

// load session module
require("session.js");
session_auto_save_auto_load = true; // auto-load session


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
content_handlers.set("application/pdf", content_handler_save); // pdf
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
