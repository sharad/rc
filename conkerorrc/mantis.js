/**
 * (C) Copyright 2008 Jeremy Maitin-Shepard
 * (C) Copyright 2009-2010 John J. Foerch
 *
 * Use, modification, and distribution are subject to the terms specified in the
 * COPYING file.
**/

require("content-buffer.js");


function cleanpage(buffer) {
    var doc    = buffer.document;

    var title = doc.title;

    var addbugnotes     = doc.getElementsByName("addbugnote");
                        
    var tables          = doc.getElementsByTagName("table");
    var table2_trs      = tables ? tables[2].getElementsByTagName("tr") : null;
    var table2_tr3_tds  = table2_trs[2] ? table2_trs[2].getElementsByTagName("td") : null;

    var table2_tr13_tds = table2_trs[13] ? table2_trs[13].getElementsByTagName("td") : null;
    var table2_tr13_td2 = table2_tr13_tds ? table2_tr13_tds[1] : "No disc";

    var bugno           = table2_tr3_tds ? table2_tr3_tds[0].innerHTML.trim() : "0";
    var bugdesc         = table2_tr13_td2.innerHTML.trim();
    var newTitle        = "mantis: " + bugno + "; " + bugdesc;

    addbugnotes[0].outerHTML = '<div name="addbugnote">' + addbugnotes[0].innerHTML + '</div>';

    doc.title = newTitle;
    doc.querySelector('title').textContent = newTitle;
}


define_page_mode("mantis-mode",

                 build_url_regexp($domain = "mantis.fortinet", // mantis.fortinet.com/bug_view_page.php
                                  $allow_www = true,
                                  $path = /bug_view_page.php/,
                                  $tlds = ["com"]),

                 function enable (buffer) {

                     add_hook.call(buffer, "buffer_dom_content_loaded_hook",
                                   cleanpage);
                     add_hook.call(buffer, "content_buffer_finished_loading_hook",
                                   cleanpage);
                 },

                 function disable (buffer) {
                     var i = 0;
                 },

                 $display_name = "Mantis");

page_mode_activate(mantis_mode);

provide("mantis");


