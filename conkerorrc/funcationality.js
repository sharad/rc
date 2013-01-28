// -*- mode: js2 -*-

// from: http://conkeror.org/Tips#Darkenthecurrentpage
// {{ follow-new-buffer
define_key(content_buffer_normal_keymap, "d", "follow-new-buffer-background");
// }}



// {{ darken_page: thanks very much for saving my eyes.
function darken_page (I) {
    // var newSS, styles='* { background: black ! important; color: grey !important }'
    // var newSS, styles='* { background: black ! important; color: white !important }'
    // var newSS, styles='* { background: #EFFFEF ! important; color: white !important }'
    // var newSS, styles='* { background: #428a42 ! important; color: white !important }'
    // var newSS, styles='* { background: DarkGreen ! important; color: white !important }'
    // var newSS, styles='* { background: DarkSlateGray  ! important; color: white !important }'
    var newSS, styles='* { background: Black  ! important; color: grey !important }'
        + ':link, :link * { color: #4986dd !important }'
        + ':visited, :visited * { color: #d75047 !important }';

    var document = I.window.buffers.current.document;

    if (document.createStyleSheet) {
        document.createStyleSheet("javascript:'" + styles + "'");
    }
    else {
        newSS=document.createElement('link');
        newSS.rel='stylesheet';
        newSS.href='data:text/css,'+escape(styles);
        document.getElementsByTagName("head")[0].appendChild(newSS);
    }
}

interactive("darken-page",
            "Darken the page in an attempt to save your eyes.",
            darken_page);

// add_hook('buffer_loaded_hook', darken_page, true, true);

define_key(content_buffer_normal_keymap, "f1", "darken-page");

// }}

// {{ Remember the last save directory for downloads

// Add the following code to your rc:

{
   let _save_path = get_home_directory();

   function update_save_path(info) {
       _save_path = info.target_file.parent.path;
   }

   add_hook("download_added_hook", update_save_path);

   suggest_save_path_from_file_name = function (filename, buffer) {
       let file = make_file(_save_path);
       file.append(filename);
       return file.path;
   }
}
// }}

// {{ If you are using Conkeror with multiple profiles, you may find
// the following code handy.  It puts the name of your current profile
// into the titles of Conkeror's windows.
function my_title_format (window) {
    // http://conkeror.org/Profiles
    return '{'+get_current_profile()+'} '+window.buffers.current.description;
}

title_format_fn = my_title_format;
// }}

// {{ Readability is a simple tool that makes reading on the web more enjoyable by removing the clutter around what you are reading
// from: http://conkeror.org/Tips?highlight=%28add%5C_hook%29%7C%28hook%29#Makethecurrentpagereadablebyremovingclutter
// http://lab.arc90.com/experiments/readability/
interactive("readability_arc90",
            "Readability is a simple tool that makes reading on the web more enjoyable by removing the clutter around what you are reading",
            function readability_arc90(I) {
                var document = I.window.buffers.current.document;

                _readability_readStyle=document.createElement('SCRIPT');
                _readability_readStyle.text = 'var readStyle = style-newspaper;';
                document.getElementsByTagName('head')[0].appendChild(_readability_readStyle);
                _readability_readSize=document.createElement('SCRIPT');
                _readability_readSize.text = 'var readSize = size-medium;';
                document.getElementsByTagName('head')[0].appendChild(_readability_readSize);
                _readability_readMargin=document.createElement('SCRIPT');
                _readability_readMargin.text = 'var readMargin = margin-wide;';
                document.getElementsByTagName('head')[0].appendChild(_readability_readMargin);
                _readability_script=document.createElement('SCRIPT');
                _readability_script.type='text/javascript';
                _readability_script.src='http://lab.arc90.com/experiments/readability/js/readability.js?x='+(Math.random());
                document.getElementsByTagName('head')[0].appendChild(_readability_script);
                _readability_css=document.createElement('LINK');
                _readability_css.rel='stylesheet';
                _readability_css.href='http://lab.arc90.com/experiments/readability/css/readability.css';
                _readability_css.type='text/css';
                _readability_css.media='screen';
                document.getElementsByTagName('head')[0].appendChild(_readability_css);
                _readability_print_css=document.createElement('LINK');
                _readability_print_css.rel='stylesheet';
                _readability_print_css.href='http://lab.arc90.com/experiments/readability/css/readability-print.css';
                _readability_print_css.media='print';
                _readability_print_css.type='text/css';
                document.getElementsByTagName('head')[0].appendChild(_readability_print_css);
            });
//Bind it to 'z'

define_key(content_buffer_normal_keymap, "z", "readability_arc90");
// }}

// {{ 20. Integrate delicious with conkeror

// Since moving from firefox to conkeror (great!), i haven't really
// used bookmarks because i don't know how to import and i asked
// myself what if i switch browsers or computer soon? Thus i decided
// to use delicious with conkeror. Note that modules/webjumps.js does
// include webjumps for use with delicious, but i wanted a tighter
// integration. Put the following in your .conkerorrc file:

// interactive("gtrans",
//             "Google translate",
//             function (I) {
//                 // check_buffer(I.buffer, content_buffer);
//                 var sendurl = encodeURIComponent('http://translate.google.com/#auto|en|')+
//                     encodeURIComponent(I.buffer.display_uri_string);

//                 //var content = yield send_http_request(load_spec({uri: sendurl}));
//                 //I.window.minibuffer.message(content.responseText);
//                 $browser_object = sendurl;
//             });

// function deliciousSuggestions(uri, completions) {
//     var domParser=Components.classes["@mozilla.org/xmlextras/domparser;1"].createInstance(Components.interfaces.nsIDOMParser);
//     var xsendurl = 'https://api.del.icio.us/v1/posts/suggest?&url='+encodeURIComponent(uri);
//     var xcontent = (yield send_http_request(load_spec({uri: xsendurl})));
//     var c = domParser.parseFromString(xcontent.responseText, "text/xml");
//     // I.window.alert(xcontent.responseText);
//     // var completions = new Array();
//     var eles = c.getElementsByTagName('recommended');
//     for (i=0; i< eles.length; i++) {
//         // I.window.alert(eles[i].text);
//         completions.push(eles[i].text);
//     }
//     return completions;
// }


// function getSelText()
// {
//     var txt = '';
//      if (window.getSelection)
//     {
//         txt = window.getSelection();
//              }
//     else if (document.getSelection)
//     {
//         txt = document.getSelection();
//             }
//     else if (document.selection)
//     {
//         txt = document.selection.createRange().text;
//             }
//     else return;
//     // document.aform.selectedtext.value =  txt;
//     return txt;
// }

interactive("delicious-post-sel",
            "bookmark the page via delicious",
            function (I) {
                check_buffer(I.buffer, content_buffer);
                var domParser=Components.classes["@mozilla.org/xmlextras/domparser;1"].createInstance(Components.interfaces.nsIDOMParser);
                var xsendurl = 'https://api.del.icio.us/v1/posts/suggest?&url='+encodeURIComponent(I.buffer.top_frame.getSelection());
                var xcontent = (yield send_http_request(load_spec({uri: xsendurl})));
                var c = domParser.parseFromString(xcontent.responseText, "text/xml");

                // I.window.alert(xcontent.responseText);
                var completions = new Array();
                var eles = c.getElementsByTagName('recommended');
                for (i=0; i< eles.length; i++) {
                    // I.window.alert(eles[i].text);
                    completions.push(eles[i].text);
                }
                var completer = prefix_completer($completions = completions);

                var sendurl = 'https://api.del.icio.us/v1/posts/add?&url='+
                // var sendurl = 'https://api.del.icio.us/v2/posts/add?&url='+
                    encodeURIComponent((yield I.minibuffer.read(
                        $prompt = "url (required): ",
                        $initial_value = I.buffer.top_frame.getSelection().toString()))) +
                    '&description='+
                    encodeURIComponent((yield I.minibuffer.read(
                        $prompt = "name (required): ",
                        $initial_value = I.buffer.top_frame.getSelection().toString())))+
                    '&replace=yes' +
                    '&tags='+
                    encodeURIComponent((yield I.minibuffer.read(
                                            $prompt = "tags (space delimited): ",
                                            $completer = completer
                                        )).replace(new RegExp(/\s+/g), ','))+
                    '&extended='+
                    encodeURIComponent((yield I.minibuffer.read(
                        $prompt = "extended description: ")));
                var content = yield send_http_request(load_spec({uri: sendurl}));
                I.window.minibuffer.message(content.responseText);
            });



// https://api.del.icio.us/v1/posts/get?url={URL} get all tags for old url

var delicious_shared = null;

interactive("delicious-shared-set",
            "bookmark setting delicious shared.",
            function (I) {
                delicious_shared = (("y" == (yield I.minibuffer.read_single_character_option(
                                                 $prompt = "Shared? (y/n)",
                                                 $initial_value = "y",
                                                 $options = ["y", "n"]))) ? "yes" : "no");
            });

interactive("delicious-post",
            "bookmark the page via delicious",
            function (I) {
                check_buffer(I.buffer, content_buffer);
                var domParser=Components.classes["@mozilla.org/xmlextras/domparser;1"].createInstance(Components.interfaces.nsIDOMParser);

                // {{ completer
                var xsendurl = 'https://api.del.icio.us/v1/posts/suggest?&url='+encodeURIComponent(I.buffer.display_uri_string);
                var xcontent = (yield send_http_request(load_spec({uri: xsendurl})));
                var cc = domParser.parseFromString(xcontent.responseText, "text/xml");
                // I.window.alert(xcontent.responseText);
                var completions = new Array();
                var sug = cc.getElementsByTagName('recommended');
                var pop = cc.getElementsByTagName('popular');
                for (i=0; i< sug.length; i++) {
                    // I.window.alert(sug[i].attributes[0].textContent);
                    completions.push(sug[i].attributes[0].textContent);
                }
                for (i=0; i< pop.length; i++) {
                    // I.window.alert(pop[i].attributes[0].textContent);
                    completions.push(pop[i].attributes[0].textContent);
                }
                var completer = prefix_completer($completions = completions);
                // }}

                // {{ initial value
                var tsendurl = 'https://api.del.icio.us/v1/posts/get?url=' + encodeURIComponent(I.buffer.display_uri_string);
                var tagcontent = (yield send_http_request(load_spec({uri: tsendurl})));
                // I.window.alert(tagcontent.responseText);
                var tc         = domParser.parseFromString(tagcontent.responseText, "text/xml");
                var post = tc.getElementsByTagName('post');
                var tags  = (post.length > 0)  ? post[0].attributes[6].textContent : "";

                if ((post.length > 0 && post[0].attributes[0].textContent.length > 0)) {
                    var desc = post[0].attributes[0].textContent;
                } else {
                    var desc = (I.buffer.title == "" ? I.buffer.display_uri_string : I.buffer.title);
                }

                var shared = null;
                if (post.length > 0 && post[0].attributes[5].textContent.length > 0) {
                    shared = post[0].attributes[5].textContent;
                }

                // }}


                var sendurl = 'https://api.del.icio.us/v1/posts/add?&url='+
                // var sendurl = 'https://api.del.icio.us/v2/posts/add?&url='+
                    encodeURIComponent((yield I.minibuffer.read(
                        $prompt = "url (required): ",
                        $initial_value = I.buffer.display_uri_string)))
                    // encodeURIComponent(I.buffer.display_uri_string)
                    +
                    '&description='+
                    encodeURIComponent((yield I.minibuffer.read(
                        $prompt = "name (required): ",
                        $initial_value = desc))) +
                    '&replace=yes' +
                    '&tags='+
                    encodeURIComponent((yield I.minibuffer.read(
                                            $prompt = "tags (space delimited): ",
                                            $completer = completer,
                                            $initial_value = tags + " " + read_from_x_primary_selection()
                                        )).replace(new RegExp(/\s+/g), ','))+
                    '&extended='+
                    encodeURIComponent((yield I.minibuffer.read(
                        $prompt = "extended description: "))) +
                    '&shared=' +
                    (delicious_shared == null ?
                     (("y" == (yield I.minibuffer.read_single_character_option(
                                   $prompt = ("Shared? (y/n)" + (shared ? (" [" + shared + "]") : "")),
                                   $options = ["y", "n"]))) ? "yes" : "no")  : delicious_shared );

                var content = yield send_http_request(load_spec({uri: sendurl}));
                I.window.minibuffer.message(content.responseText);
                if (typeof(debug_level) != "undefined" && debug_level)
                    I.window.minibuffer.message(sendurl);

            });

interactive("delicious-post-link",
            "bookmark the link via delicious",
            function (I) {
                bo = yield read_browser_object(I) ;
                mylink = load_spec_uri_string(load_spec(encodeURIComponent(bo)));
                check_buffer(I.buffer, content_buffer);

                var domParser=Components.classes["@mozilla.org/xmlextras/domparser;1"].createInstance(Components.interfaces.nsIDOMParser);


                // {{ completer
                var xsendurl = 'https://api.del.icio.us/v1/posts/suggest?&url='+mylink;
                var xcontent = (yield send_http_request(load_spec({uri: xsendurl})));
                var cc = domParser.parseFromString(xcontent.responseText, "text/xml");
                // I.window.alert(xcontent.responseText);
                var completions = new Array();
                var sug = cc.getElementsByTagName('recommended');
                var pop = cc.getElementsByTagName('popular');
                for (i=0; i< sug.length; i++) {
                    // I.window.alert(sug[i].attributes[0].textContent);
                    completions.push(sug[i].attributes[0].textContent);
                }
                for (i=0; i< pop.length; i++) {
                    // I.window.alert(pop[i].attributes[0].textContent);
                    completions.push(pop[i].attributes[0].textContent);
                }
                var completer = prefix_completer($completions = completions);
                // }}

                // {{ initial value
                var tsendurl = 'https://api.del.icio.us/v1/posts/get?url=' + mylink;
                var tagcontent = (yield send_http_request(load_spec({uri: tsendurl})));
                // I.window.alert(tagcontent.responseText);
                var tc         = domParser.parseFromString(tagcontent.responseText, "text/xml");
                var post = tc.getElementsByTagName('post');
                var tags  = (post.length > 0)  ? post[0].attributes[6].textContent : "";
                if ((post.length > 0 && post[0].attributes[0].textContent.length > 0)) {
                    var desc = post[0].attributes[0].textContent;
                } else {
                    var desc = (bo.textContent == "" ? bo : bo.textContent);
                }

                var shared = null;
                if ((post.length > 0 && post[0].attributes[5].textContent.length > 0)) {
                    shared = post[0].attributes[5].textContent;
                }


                // }}


                let sendurl = 'https://api.del.icio.us/v1/posts/add?&url=' +
                    // mylink
                    encodeURIComponent((yield I.minibuffer.read(
                        $prompt = "url (required): ",
                        // $initial_value = decodeURIComponent(mylink)))) +
                        $initial_value = bo))) +
                    '&description=' +
                    encodeURIComponent((yield I.minibuffer.read($prompt = "name (required): " , $initial_value = desc))) +
                    '&replace=yes' +
                    '&tags=' + encodeURIComponent((yield I.minibuffer.read(
                                                       $prompt = "tags (space delimited): ",
                                                       $completer = completer,
                                                       $initial_value = tags
                                                   )).replace(new RegExp(/\s+/g), ',')) +
                    '&extended=' + encodeURIComponent((yield I.minibuffer.read($prompt = "extended description: "))) +
                    '&shared=' +
                    (delicious_shared == null ?
                     (("y" == (yield I.minibuffer.read_single_character_option(
                                   $prompt = ("Shared? (y/n)" + (shared ? (" [" + shared + "]") : "")),
                                   $options = ["y", "n"]))) ? "yes" : "no")  : delicious_shared );

                var content = yield send_http_request(
                    load_spec({uri: sendurl}));
                I.window.minibuffer.message(content.responseText);
                if (typeof(debug_level) != "undefined" && debug_level)
                    I.window.minibuffer.message(sendurl);

            },
            $browser_object = browser_object_links);

define_key(default_global_keymap, "p", "delicious-post");
define_key(default_global_keymap, "P", "delicious-post-link");
define_key(default_global_keymap, "Z", "delicious-post-sel");

define_webjump("del", "http://delicious.com/search?p=%s&chk=&context=userposts%7Csh4r4d&fr=del_icio_us&lc=");

// interactive("anon-followlink",
//             "bookmark the link via delicious",
//             function (I) {
//                 bo = yield read_browser_object(I) ;
//                 mylink = load_spec_uri_string(load_spec(decodeURIComponent(bo)));
//                 check_buffer(I.buffer, content_buffer);
//                 // let sendurl = 'https://api.del.icio.us/v1/posts/add?&url=' +
//                 return mylink;
//             }, $browser_object = browser_object_links);

// define_key(default_global_keymap, "A", "anon-followlink");


// define_webjump("gtrans", function (url) {
//     if (url) {
//         // return "http://translate.google.com/#auto|en|" + url;
//         return "http://translate.google.com/translate?js=y&prev=_t&hl=en&ie=UTF-8&layout=1&eotf=1&u=" + url + "&sl=auto&tl=en";
//     } else {
//         return "javascript:window.location.href='http://translate.google.com/translate?js=y&prev=_t&hl=en&ie=UTF-8&layout=1&eotf=1&u='+window.location.href+'&sl=auto&tl=en';";
//     }
// }, $argument = "optional");




// Change YOUR_USERNAME_RIGHT_HERE to your username.

// NOTE: you can modify the above code easily if you want to include
// more fields, see Delicious's api specs:
// http://delicious.com/help/api.

// Now, while surfing, you can hit "p" to bookmark the page, and type
// in the name, tags, and extended description in conkeror. If you
// want to bookmark a link, hit "P" [It'd be nice to make the
// suggested name be the words that is linked-afied or the name of the
// page]. When bookmarking for the first time in a session, delicious
// will ask for your username and password. Just type them in and
// save. Also, after bookmarking, look for a "done" message to know
// the bookmark works. Otherwise, you will see a "something went
// wrong" from delicious.

// Use the webjump del to search for tags. Ideally, it'd be nice if we
// can use a google-like webjump, where links appear in the minibuffer
// for us to select. However, I don't know how to do this.

// }}

// {{ Posting to Bibsonomy

// The following snippet lets you post easily to http://bibsonomy.org :

interactive("bibsonomy-post-publication",
            "Post a publication to Bibsonomy. Either uses the URL and scrapes the page, or sends the selected bibtex.",
            function (I) {
                var element = yield read_browser_object(I);
                var spec = load_spec(element);
                newspec = 'http://www.bibsonomy.org/BibtexHandler?requTask=upload&url='
                    + encodeURIComponent(load_spec_uri_string(spec))
                    + '&description='
                    + encodeURIComponent(load_spec_title(spec))
                    + '&selection='+encodeURIComponent(I.buffer.top_frame.getSelection());
                browser_object_follow(I.buffer, OPEN_CURRENT_BUFFER, newspec);
            },
            $browser_object = browser_object_frames);
define_key(content_buffer_normal_keymap, "C-c b", "bibsonomy-post-publication");

// }}

// {{ Mode line buttons for basic browser control

// Simple GUI buttons can be enabled to control conkeror. They are
// intended to be unobtrusive and to steal as little screen space as
// possible. Clicking on them executes a conkeror command. Hovering
// over them tells you the command and the corresponding keystroke.

// The buttons are intended to make conkeror usable for a casual user
// and also to aid the novice user while they become familiar with
// conkeror's interface.

// load_paths.unshift("chrome://conkeror-contrib/content/");
// MODULE IS NOT AVAILABLE HERE -me
// require("mode-line-buttons.js");
// add_mode_line_buttons(standard_mode_line_buttons, true);

// }}

// {{ Big Hint Numbers

// register_user_stylesheet(
//     "data:text/css," +
//         escape(
//             "@namespace url(\"http://www.w3.org/1999/xhtml\");\n" +
//                 "span.__conkeror_hint {\n"+
//                 "  font-size: 18px !important;\n"+
//                 "  line-height: 18px !important;\n"+
//                 "}"));

register_user_stylesheet(
    "data:text/css," +
        escape(
            "@namespace url(\"http://www.w3.org/1999/xhtml\");\n" +
                "span.__conkeror_hint {\n"+
                "  font-size: 12px !important;\n"+
                "  line-height: 12px !important;\n"+
                "}"));

// }}

// // {{ darken google search
// register_user_stylesheet(       //small black
//     make_css_data_uri(["body{background: black !important; color: grey !important;}",
//                        ":link, :link * {color: #4986dd !important;}",
//                        ":visited, :visited * {color: #d75047 !important;}"],
//                       $url_prefixes = "http://www.google."));
// // }}

// {{ darken google search
// register_user_stylesheet(
//     make_css_data_uri(['* { background: Black  ! important; color: grey !important };\n'
//                         + ':link, :link * { color: White !important }\n'
//                         + ':visited, :visited * { color: Green !important }\n'],
//                        $url_prefixes = "http://www.google."));

var $blackurls = [
    "http://www.emacswiki.org",
    "http://emacswiki.org",
    "http://www.google.",
    "http://www.toodledo.com",
    "http://www.delicious.com",
    "http://www.linuxtoday.com"
];
register_user_stylesheet(
    make_css_data_uri(['* { background: Black  ! important; color: grey !important };\n'
                       + ':link, :link * { color: White !important }\n'
                       + ':visited, :visited * { color: Green !important }\n'],
                      $url_prefixes = $blackurls));

// register_user_stylesheet(
//     make_css_data_uri([(<r><![CDATA[
//                                       * {
//                                           background: Black  !important
//                                           color:      grey !important
//                                       };
//                                   :link, :link * {
//                                       color: White !important
//                                   };
//                                   :visited, :visited * {
//                                       color: Green !important
//                                   };
//                               ]]></r>).toString(),],
//                       $url_prefixes = $blackurls));



// // from http://mook.wordpress.com/2005/10/30/multi-line-strings-in-javascript/ not working
// $blackencss = (<r><![CDATA[
//                              * {
//                                  background: Black  !important
//                                  color:      grey !important
//                              };
//                          :link, :link * {
//                              color: White !important
//                          };
//                          :visited, :visited * {
//                              color: Green !important
//                          };
//                      ]]></r>).toString();
// // from http://mook.wordpress.com/2005/10/30/multi-line-strings-in-javascript/
// register_user_stylesheet(
//     make_css_data_uri([ $blackencss ],
//                       $url_prefixes = $blackurls));



// }}

// {{ Default Zoom Level

function my_zoom_set (buffer) {
    browser_zoom_set(buffer, false, 100);
}
add_hook('create_buffer_hook', my_zoom_set);

// }}


// {{ Set Homepage to a File in the Home Directory

// The following code is how to set your homepage to a file in your
// home directory, in a way that is cross-platform safe, without
// hard-coding the path.

// let (home = get_home_directory()) {
//     home.append("public_html/foo.html");
//     homepage = home.path;
// };
// }}

// {{ Manipulate Cache Settings

// Clearing caches:

// M-: cache_clear(CACHE_ALL)
// M-: cache_clear(CACHE_DISK)
// M-: cache_clear(CACHE_MEMORY)
// M-: cache_clear(CACHE_OFFLINE)

// Disabling caches:

// M-: cache_disable(CACHE_ALL)
// M-: cache_disable(CACHE_DISK)
// M-: cache_disable(CACHE_MEMORY)
// M-: cache_disable(CACHE_OFFLINE)

// Enabling caches:

// M-: cache_enable(CACHE_ALL)
// M-: cache_enable(CACHE_DISK)
// M-: cache_enable(CACHE_MEMORY)
// M-: cache_enable(CACHE_OFFLINE)

// }}


// {{ Create a TinyURL for the Current Buffer's URL

// The following code makes a browser-object class for a tiny-url of
// the page you are currently browsing. It binds * q to the browser
// object, so to put a tinyurl on the clipboard, you would use the
// sequence * q c.

// last updated September 22, 2009
define_browser_object_class(
    "tinyurl", "Get a tinyurl for the current page",
    function (I, prompt) {
        check_buffer(I.buffer, content_buffer);
        let createurl = 'http://tinyurl.com/api-create.php?url=' +
            encodeURIComponent(
                load_spec_uri_string(
                    load_spec(I.buffer.top_frame)));
        try {
            var content = yield send_http_request(
                load_spec({uri: createurl}));
            yield co_return(content.responseText);
        } catch (e) { }
    });

define_key(content_buffer_normal_keymap, "* q", "browser-object-tinyurl");

// }}

// {{ Switch to Buffers 1-10 Using Number Keys 1 through 0

// Handy as the number keys are unbound by default. I.e., 1 will
// switch to the first buffer, 2 to the second buffer, 0 to the tenth
// buffer. These bindings work as expected with the tab-bar module.

function define_key_buffer_switch(key, buf_num) {
    define_key(content_buffer_normal_keymap, key, function (I) {
        switch_to_buffer(I.window, I.window.buffers.get_buffer(buf_num));
    });
    define_key(download_buffer_keymap, key, function (I) {
        switch_to_buffer(I.window, I.window.buffers.get_buffer(buf_num));
    });
}
for (let i = 0; i < 10; ++i) {
    define_key_buffer_switch(i == 9 ? "0" : (i+1).toString(), i);
}

// }}

// {{ Open Middle-Clicked Links in New Buffers

// require("clicks-in-new-buffer.js");

// You can control whether buffers are created in the foreground or
// background (foreground is default).

// Set to either OPEN_NEW_BUFFER or OPEN_NEW_BUFFER_BACKGROUND
// clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND; // Now
// buffers open in background.

// You can control the mouse button which triggers buffer creation
// (middle is default).

// Set to 0 = left mouse, 1 = middle mouse, 2 = right mouse
// clicks_in_new_buffer_button = 2; //  Now right mouse follows links in new buffers.
// }}

// {{ Select Current Page with Browser Object Commands
// Certain commands (like 'copy', 'save' etc.) prompt for a link and
// show hint numbers; when you just want to select the url of the
// current buffer, you can just type 0 get it. Let us say you want to
// save the current buffer, just type the following

// M-x save RET 0 RET

// and you will be prompted to choose a file path. With that, you
// could also duplicate buffers, which is a function found in many
// browsers:

// M-x follow RET 0 RET

// Note: This will not duplicate the state (like colum/line position etc.) of the buffer.

// If you want to bind this to a key, you can use something like the following:

interactive("duplicate-buffer", "Duplicate buffer",
            function (I) {
                browser_object_follow(I.buffer, OPEN_NEW_BUFFER, I.buffer.current_uri.spec);
            });
define_key(content_buffer_normal_keymap, "M-N", "duplicate-buffer");
// }}

// {{ Hide Scroll Bars
// break isearch
// function disable_scrollbars (buffer) {
//     buffer.browser.contentWindow.scrollbars.visible = false;
// }

// not working
interactive("enable_scrollbars", "Enable Scrollbars",
            function enable_scrollbars (buffer) {
                buffer.browser.contentWindow.scrollbars.visible = true;
            });
// break isearch
// add_hook("content_buffer_location_change_hook", disable_scrollbars);
// }}

// {{ Keyboard Shortcuts for Often-Used Sites
// Here is an example of how to bind a key to go to a specific
// website. Because the command is defined as an alias of the follow
// command, the prefix key C-u will open the site in a new buffer.

interactive("open-gmail", "Go to gmail", "follow",
            $browser_object = "http://gmail.com/");
define_key(content_buffer_normal_keymap, "f2", "open-gmail");
// }}

// {{ Disable fixed background image or opacity in CSS
// Useful command to force user defined CSS style attributes.
// Add the following to your RC or execute in runtime.
// force scrollable background
// register_user_stylesheet("data:text/css,"+escape ("* {background-attachment: scroll !important;}"));
// force full opacity (no alpha transparency)
// register_user_stylesheet("data:text/css,"+escape ("* {opacity: 1.0 !important;}"));
// }}

// from: https://www.squarefree.com/bookmarklets/zap.html
/* Removes most presentational attributes and tags while leaving style sheets intact. */
function zap_presentation(I){

    var
    H = ["bgcolor",
         "bgColor",
         "background",
         "color",
         "align",
         "text",
         "alink",
         "vlink"],
    Y = {FONT:1,
         CENTER:1},
    d=[],
    p,
    document = I.window.buffers.current.document;

    function R(N){
        var a,x,i,t;
        if(t=N.tagName){

            t=t.toUpperCase();

            for (i=0; a=H[i]; ++i)
                if(N.getAttribute(a))
                    N.removeAttribute(a);

            for(i=0; x=N.childNodes[i]; ++i)
                R(x);

            if (Y[t])
                d.push(N);
        }
    }

    R(document.documentElement);

    for (i=0;N=d[i];++i) {
        p=N.parentNode;
        while(N.firstChild)
            p.insertBefore(N.firstChild,N);
        p.removeChild(N);
    }
}


interactive("zap-presentation",
            "Removes most presentational attributes and tags while leaving style sheets intact.",
            zap_presentation);

function zap_style_sheets(I) {
    var i, x;
    document = I.window.buffers.current.document;

    for(i=0; x=document.styleSheets[i]; ++i)
        x.disabled=true;
}

interactive("zap-style-sheets",
            "Disables all style sheets.",
            zap_style_sheets);


// http://conkeror.org/BreakingChanges
add_hook("before_quit_hook",
         function () {
             cookie_manager.removeAll();
             I.window.alert("bye .. removed cookies");
             var w = get_recent_conkeror_window();
             var result = (w == null) ||
                 "y" == (yield w.minibuffer.read_single_character_option(
                     $prompt = "Quit Conkeror? (y/n)",
                     $options = ["y", "n"]));
             yield co_return(result);
         });

// {{{


function delicious(I, mylink) {
    var sendurl = 'https://api.del.icio.us/v2/posts/add?&url='+
        mylink +
        '&description=' +
        encodeURIComponent((yield I.minibuffer.read($prompt = "name (required): " , $initial_value = bo.textContent))) +
        '&tags=' + encodeURIComponent((yield I.minibuffer.read($prompt = "tags (space delimited): "))) +
        '&extended=' + encodeURIComponent((yield I.minibuffer.read($prompt = "extended description: ")));
    var content = yield send_http_request(
        load_spec({uri: sendurl}));
    I.window.minibuffer.message(content.responseText);
}

interactive("acition-link",
            "do action on link",
            function (I) {
                bo = yield read_browser_object(I) ;
                mylink = load_spec_uri_string(load_spec(encodeURIComponent(bo)));
                check_buffer(I.buffer, content_buffer);

                // here implement it so it will ask for function name like delicious
                delicious(I, mylink);
                // let sendurl = 'https://api.del.icio.us/v1/posts/add?&url=' +
            }, $browser_object = browser_object_links);

// }}}



// {{{
// http://www.mozdev.org/pipermail/conkeror/2009-February/001334.html
// select text and google it.  See also "**c" for selecting text
interactive("search-clipboard-contents",
            "Search in Google the content of the X clipboard (the selected text)",
            "find-url",
            $browser_object = function(I) {
                return "g "+ read_from_x_primary_selection();
            }
           );
interactive("search-clipboard-contents-doublequoted",
            "Search in Google the content of the X clipboard (the selected text) as a fixed string",
            "find-url",
            $browser_object = function(I) {
                return "g \""+ read_from_x_primary_selection()+"\"";
            }
           );
define_key(content_buffer_normal_keymap, "l", "search-clipboard-contents");
define_key(content_buffer_normal_keymap, "L", "search-clipboard-contents-doublequoted");
// Now try "l" and "C-u l" with your copied text.
// }}}














// Local Variables: **
// folded-file:f **
// mode:js2 **
// comment-column:0 **
// comment-start: "// "  **
// comment-end:   "// "  **
// End: **








