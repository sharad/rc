// -*- mode: js -*-


// {{ Remember the last save directory for downloads

// Add the following code to your rc:

function update_save_path(info)
{
  _save_path = info.target_file.parent.path;
}

{
  let _save_path = get_home_directory();

  add_hook("download_added_hook", update_save_path);

  suggest_save_path_from_file_name =
    function (filename, buffer)
    {
      let file = make_file(_save_path);
      file.append(filename);
      return file.path;
    }
}
// }}

// {{ If you are using Conkeror with multiple profiles, you may find
// the following code handy.  It puts the name of your current profile
// into the titles of Conkeror's windows.
function my_title_format (window)
{
  // http://conkeror.org/Profiles
  return '{'+get_current_profile()+'} [' + window.buffers.count + '] '+window.buffers.current.description;
}

title_format_fn = my_title_format;
// }}

// {{ Readability is a simple tool that makes reading on the web more
// enjoyable by removing the clutter around what you are reading
// from: http://conkeror.org/Tips?highlight=%28add%5C_hook%29%7C%28hook%29#Makethecurrentpagereadablebyremovingclutter
// http://lab.arc90.com/experiments/readability/
interactive("readability_arc90",
            "Readability is a simple tool that makes reading on the web more enjoyable by removing the clutter around what you are reading",
            function readability_arc90(I)
            {
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


// var delicious_api_server = 'api.del.icio.us';
// var delicious_api_server = 'del.icio.us';
var delicious_api_server = 'api.pinboard.in';
var delicious_api_version = 'v1';



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
//     var xsendurl = 'https://' + delicious_api_server + '/' + delicious_api_version + '/posts/suggest?&url='+encodeURIComponent(uri);
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
            function (I)
            {
              check_buffer(I.buffer, content_buffer);
              var domParser = Components.classes["@mozilla.org/xmlextras/domparser;1"].createInstance( Components.interfaces.nsIDOMParser );
              var xsendurl  = 'https://' + delicious_api_server + '/' + delicious_api_version + '/posts/suggest?&url=' + encodeURIComponent(I.buffer.top_frame.getSelection());
              var xcontent  = (yield send_http_request(load_spec({uri: xsendurl})));
              var c         = domParser.parseFromString(xcontent.responseText, "text/xml");

              // I.window.alert(xcontent.responseText);
              var completions = new Array();
              var eles = c.getElementsByTagName('recommended');
              for (i=0; i< eles.length; i++)
              {
                // I.window.alert(eles[i].text);
                completions.push(eles[i].text);
              }
              // var completer = prefix_completer($completions = completions);
              var completer = new prefix_completer($completions = completions);

              var sendurl =
                // var sendurl = 'https://' + delicious_api_server + '/v2/posts/add?&url='+
                'https://' + delicious_api_server + '/' + delicious_api_version + '/posts/add?&url='                           +
                encodeURIComponent(
                  (yield                    I.minibuffer.read(
                     $prompt = "url (required): ",
                     $initial_value =
                       I.buffer.top_frame.getSelection().toString()))) +
                '&description=' +
                encodeURIComponent(
                  (yield I.minibuffer.read(
                    $prompt = "name (required): ",
                    $initial_value =
                      I.buffer.top_frame.getSelection().toString())))                    +
                '&replace=yes' + '&tags=' +
                encodeURIComponent(
                  (yield I.minibuffer.read(
                    $prompt = "tags (space delimited): ",
                    $completer = completer,
                    $initial_value =
                      tags +
                      " "  +
                      read_from_x_primary_selection())).replace(new RegExp(/\s+/g), ',')) +
                '&extended=' +
                encodeURIComponent(
                  (yield I.minibuffer.read(
                    $prompt = "extended description: ")));

              var content =
                yield send_http_request(load_spec({uri: sendurl}));

              I.window.minibuffer.message(content.responseText);
            });



// https://' + delicious_api_server + '/' + delicious_api_version + '/posts/get?url={URL} get all tags for old url

var delicious_shared = null;

interactive("delicious-shared-set",
            "bookmark setting delicious shared.",
            function (I)
            {
              delicious_shared =
                (("y" ==
                  (yield I.minibuffer.read_single_character_option(
                    $prompt = "Shared? (y/n)",
                    $initial_value = "y",
                    $options = ["y", "n"]))) ?
                 "yes" : "no");
            });

interactive("delicious-post",
            "bookmark the page via delicious",
            function (I) {
                check_buffer(I.buffer, content_buffer);
                var domParser=Components.classes["@mozilla.org/xmlextras/domparser;1"].createInstance(Components.interfaces.nsIDOMParser);

                // {{ completer
                var xsendurl = 'https://' + delicious_api_server + '/' + delicious_api_version + '/posts/suggest?&url='+encodeURIComponent(I.buffer.display_uri_string.replace(/[^\x00-\x7F]/g, ''));
                var xcontent = (yield send_http_request(load_spec({uri: xsendurl})));
                var cc = domParser.parseFromString(xcontent.responseText, "text/xml");
                // I.window.alert(xcontent.responseText);
                var completions = new Array();
                var sug = cc.getElementsByTagName('recommended');
                if (sug)
                {
                    for (i = 0; i < sug.length; i++)
                    {
                        // I.window.alert(sug[i].attributes[0].textContent);
                        if (sug[i].attributes[0]) completions.push(sug[i].attributes[0].textContent);
                    }
                }
                var pop = cc.getElementsByTagName('popular');
                if (pop)
                {
                    for (i=0; i< pop.length; i++) {
                        // I.window.alert(pop[i].attributes[0].textContent);
                        if (pop[i].attributes[0]) completions.push(pop[i].attributes[0].textContent);
                    }
                }
                // var completer = prefix_completer($completions = completions);
                var completer  = new prefix_completer($completions = completions);
                // }}

                // {{ initial value
                var tsendurl   = 'https://' + delicious_api_server + '/' + delicious_api_version + '/posts/get?url=' + encodeURIComponent(I.buffer.display_uri_string.replace(/[^\x00-\x7F]/g, ''));
                var tagcontent = (yield send_http_request(load_spec({uri: tsendurl})));
                // I.window.alert(tagcontent.responseText);
                var tc         = domParser.parseFromString(tagcontent.responseText, "text/xml");
                var post       = tc.getElementsByTagName('post');


                // var tags       = (post.length > 0)  ? post[0].attributes[6].textContent : "";
                var tags       = (post.length > 0)  ? post[0].attributes['tag'].textContent : "";

                // if (post.length > 0 && //
                //     post[0].attributes[0].textContent.length > 0) { //
                //     var desc = post[0].attributes[0].textContent.replace(/[^\x00-\x7F]/g, ''); //
                // } else { //
                //     var desc = (I.buffer.title == "" ? I.buffer.display_uri_string : I.buffer.title).replace(/[^\x00-\x7F]/g, ''); //
                // } //

                if (post.length > 0 &&
                    post[0].attributes['description'].textContent.length > 0) {
                    var desc = post[0].attributes['description'].textContent.replace(/[^\x00-\x7F]/g, '');
                } else {
                    var desc = (I.buffer.title == "" ? I.buffer.display_uri_string : I.buffer.title).replace(/[^\x00-\x7F]/g, '');
                }

                var shared = null;
                if (post.length > 0 && post[0].attributes[5].textContent.length > 0) {
                    shared = post[0].attributes[5].textContent;
                }

                // }}


                var sendurl = 'https://' + delicious_api_server + '/' + delicious_api_version + '/posts/add?&url='+
                    // var sendurl = 'https://' + delicious_api_server + '/v2/posts/add?&url='+
                    encodeURIComponent((yield I.minibuffer.read(
                        $prompt = "url (required): ",
                        // $initial_value = I.buffer.display_uri_string)))
                        $initial_value = I.buffer.display_uri_string.replace(/[^\x00-\x7F]/g, '')))) +
                    // encodeURIComponent(I.buffer.display_uri_string) +
                    '&description=' +
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
                    '&extended=' +
                    encodeURIComponent((yield I.minibuffer.read(
                        $prompt = "extended description: "))) +
                    '&shared=' +
                    (delicious_shared == null ?
                     (("y" == (yield I.minibuffer.read_single_character_option(
                         $prompt = ("Shared? (y/n)" +
                                    (shared ? (" [" + shared + "]") : "")),
                         $options = ["y", "n"]))) ?
                      "yes" : "no")  : delicious_shared );

                var content = yield send_http_request(load_spec({uri: sendurl}));
                I.window.minibuffer.message(content.responseText);
                if (typeof(debug_level) != "undefined" && debug_level)
                    I.window.minibuffer.message(sendurl);
            });

interactive("delicious-post-link",
            "bookmark the link via delicious",
            function (I) {
                var bo = yield read_browser_object(I);
                var mylink = load_spec_uri_string(load_spec(encodeURIComponent(bo))).replace(/[^\x00-\x7F]/g, '');
                check_buffer(I.buffer, content_buffer);

                var domParser=Components.classes["@mozilla.org/xmlextras/domparser;1"].createInstance(Components.interfaces.nsIDOMParser);


                // {{ completer
                var xsendurl = 'https://' + delicious_api_server + '/' + delicious_api_version + '/posts/suggest?&url='+mylink;
                var xcontent = (yield send_http_request(load_spec({uri: xsendurl})));
                // var xcontent = xcontent.replace(/[^\x00-\x7F]/g, '');
                var cc = domParser.parseFromString(xcontent.responseText, "text/xml");
                // I.window.alert(xcontent.responseText);
                var completions = new Array();
                var sug = cc.getElementsByTagName('recommended');
                if (sug)
                {
                    for (i = 0; i < sug.length; i++)
                    {
                        // I.window.alert(sug[i].attributes[0].textContent);
                        if (sug[i].attributes[0]) completions.push(sug[i].attributes[0].textContent);
                    }
                }
                var pop = cc.getElementsByTagName('popular');
                if (pop)
                {
                    for (i=0; i< pop.length; i++) {
                        // I.window.alert(pop[i].attributes[0].textContent);
                        if (pop[i].attributes[0]) completions.push(pop[i].attributes[0].textContent);
                    }
                }
                // var completer = prefix_completer($completions = completions);
                var completer = new prefix_completer($completions = completions);
                // }}

                // {{ initial value
                var tsendurl   = 'https://' + delicious_api_server + '/' + delicious_api_version + '/posts/get?url=' + mylink;
                var tagcontent = (yield send_http_request(load_spec({uri: tsendurl})));
                // I.window.alert(tagcontent.responseText);
                var tc         = domParser.parseFromString(tagcontent.responseText, "text/xml");
                var post       = tc.getElementsByTagName('post');
                var tags       = (post.length > 0)  ? post[0].attributes['tag'].textContent : "";
                if (post.length > 0 &&
                    post[0].attributes['description'].textContent.length > 0)
                {
                    var desc = post[0].attributes['description'].textContent.replace(/[^\x00-\x7F]/g, '');
                }
                else
                {
                    var desc = (bo.textContent == "" ? bo : bo.textContent).replace(/[^\x00-\x7F]/g, '');
                }

                var shared = null;
                if (post.length > 0 &&
                    post[0].attributes[5].textContent.length > 0)
                {
                    shared = post[0].attributes[5].textContent;
                }


                // }}


                let sendurl = 'https://' + delicious_api_server + '/' + delicious_api_version + '/posts/add?&url=' +
                    // mylink
                    encodeURIComponent((yield I.minibuffer.read(
                        $prompt = "url (required): ",
                        $initial_value = decodeURIComponent(mylink)))) +
                    // $initial_value = bo // ))) +
                    '&description=' +
                    encodeURIComponent(
                        (yield I.minibuffer.read(
                            $prompt = "name (required): " , $initial_value = desc))) +
                    '&replace=yes' +
                    '&tags=' + encodeURIComponent((yield I.minibuffer.read(
                        $prompt = "tags (space delimited): ",
                        $completer = completer,
                        $initial_value = tags + " " + read_from_x_primary_selection()
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
//                 // let sendurl = 'https://' + delicious_api_server + '/' + delicious_api_version + '/posts/add?&url=' +
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
  // "http://www.google.",
  "http://www.toodledo.com",
  "http://www.delicious.com",
  "http://www.linuxtoday.com"
];
if (false )
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

require("clicks-in-new-buffer.js");

// You can control whether buffers are created in the foreground or
// background (foreground is default).

// Set to either OPEN_NEW_BUFFER or OPEN_NEW_BUFFER_BACKGROUND
clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND; // Now
// buffers open in background.

// You can control the mouse button which triggers buffer creation
// (middle is default).

// Set to 0 = left mouse, 1 = middle mouse, 2 = right mouse
clicks_in_new_buffer_button = 2; //  Now right mouse follows links in new buffers.
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
define_key(content_buffer_normal_keymap, "f3", "open-gmail");
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
    t = N.tagName;
    if( t ){

      t=t.toUpperCase();

      for (i=0; a=H[i]; ++i)
        if(N.getAttribute(a))
          N.removeAttribute(a);

      for(i = 0;
          x = N.childNodes[i];
          ++i)
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
can_kill_last_buffer = true;

add_hook("before_quit_hook",
         function ()
         {
           cookie_manager.removeAll();
           var w = get_recent_conkeror_window();
           var result = (w.buffers.count == 1) ||
             "y" ==
             (yield w.minibuffer.read_single_character_option(
               $prompt = "Quit Conkeror? (y/n)",
               $options = ["y", "n"]));
           yield co_return(result);
         });

add_hook("kill_buffer_hook",
         function (buffer)
         {
           // var buffers = buffer.window.buffers;
           var w = buffer.window;
           if (w.buffers.count == 1) {
             w.alert("bye .. removed cookies");
             cookie_manager.removeAll();
           }

           var result = (w.buffers.count == 1) ||
             "y" == (yield w.minibuffer.read_single_character_option(
               $prompt = "Quit Conkeror? (y/n)",
               $options = ["y", "n"]));
           yield co_return(result);
         });
can_kill_last_buffer = false;
// {{{

//{{{ I think by the time kill_buffer_hook runs the buffer is gone so I
// patch kill_buffer

var kill_buffer_original = kill_buffer_original || kill_buffer;

var killed_buffer_urls = [];

kill_buffer = function (buffer, force) {
    if (buffer.display_uri_string) {
        killed_buffer_urls.push(buffer.display_uri_string);
    }
    var w = buffer.window;
    // var result = (w.buffers.count == 1) &&
    //     "y" == (yield w.minibuffer.read_single_character_option(
    //         $prompt = "Quit Conkeror? (y/n)",
    //         $options = ["y", "n"]));
    if ((w.buffers.count == 1)) {
        cookie_manager.removeAll();
    }
    kill_buffer_original(buffer,force);
};

interactive("restore-killed-buffer-url", "Loads url from a previously killed buffer",
            function restore_killed_buffer_url (I) {
                if (killed_buffer_urls.length !== 0) {
                    var url = yield I.minibuffer.read(
                        $prompt = "Restore killed url:",
                        $completer = all_word_completer($completions = killed_buffer_urls),
                        $default_completion = killed_buffer_urls[killed_buffer_urls.length - 1],
                        $auto_complete = "url",
                        $auto_complete_initial = true,
                        $auto_complete_delay = 0,
                        $match_required);

                    load_url_in_new_buffer(url);
                } else {
                    I.window.minibuffer.message("No killed buffer urls");
                }
            });
//}}}


function delicious(I, mylink) {
    var sendurl = 'https://' + delicious_api_server + '/v2/posts/add?&url='+
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
                // let sendurl = 'https://' + delicious_api_server + '/' + delicious_api_version + '/posts/add?&url=' +
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


//{{
// Stop loading all buffer (key A-h)
define_key(default_global_keymap, "A-h",
          function (I)
          {
              for (var i = 0; i < I.window.buffers.count; i++)
              {
                  stop_loading(I.window.buffers.get_buffer(i));
              }
          });
// Reload all buffer (key A-r)

define_key(default_global_keymap, "A-r",
          function (I)
          {
              for (var i = 0; i < I.window.buffers.count; i++)
              {
                  reload(I.window.buffers.get_buffer(i));
              }
          });
//}}

//{{ Limiting Buffers
    // if (res == "y") { //
    //   kill_buffer(buffer, true); //
    // } //
      // kill_buffer_to_limit_internal( (yield w.minibuffer.read_single_character_option($prompt = ("more than 10 buffers should kill buffer (y/n)"), $options = ["y", "n"])) );

var fixed_max_buffer_hard_limit         = 30;
var fixed_max_buffer_soft_limit         = 10;
var dyn_max_buffer_soft_limit           = 10;
var fixed_max_buffer_soft_limit_increment = 5;
var kill_buffer_to_limit_delay          = 3000;
function kill_buffer_to_limit(buffer) {
  var w = buffer.window;
  w.setTimeout(
    function() {
      if (w.buffers.count > dyn_max_buffer_soft_limit) {
        if (w.buffers.count > fixed_max_buffer_hard_limit ||
            w.confirm("more than " + dyn_max_buffer_soft_limit +" buffers[" + w.buffers.count +"] should kill buffer")) {

          kill_buffer(buffer, true);

          if (w.buffers.count > fixed_max_buffer_hard_limit) {
            w.minibuffer.message("killed the buffer it exceeding max hard limit " + fixed_max_buffer_hard_limit);
          }

        } else {

          dyn_max_buffer_soft_limit += fixed_max_buffer_soft_limit_increment;
          w.minibuffer.message("current dyn_max_buffer_soft_limit is incremented to " + dyn_max_buffer_soft_limit);

        }
      } else if (dyn_max_buffer_soft_limit > fixed_max_buffer_soft_limit &&
                 dyn_max_buffer_soft_limit - w.buffers.count >= fixed_max_buffer_soft_limit_increment) {
        dyn_max_buffer_soft_limit -= fixed_max_buffer_soft_limit_increment;
        w.minibuffer.message("current dyn_max_buffer_soft_limit is decremented to " + dyn_max_buffer_soft_limit);
      }
    }, kill_buffer_to_limit_delay);
}
add_hook("create_buffer_late_hook", kill_buffer_to_limit);

// interactive("test-xzy", //
//             "Opens all bookmarks in the given group name.", //
//            function (I) { //
//              var w = I.window; //
//              if (w.buffers.count > 1) { //
//                w.alert("Hello"); //
//                var res = yield w.minibuffer.read_single_character_option($prompt = ("more than 10 buffers[" + w.buffers.count +"] should kill buffer (y/n)"), $options = ["y", "n"]); //
//                if (res == "y") //
//                { //
//                  w.alert("Hello"); //
//                } //
//              } //
//            }); //
// define_key(content_buffer_normal_keymap, "C-i", "test-xzy"); //

//}}


//{{ Bookmark Group [https://ericjmritz.name/2015/08/12/creating-bookmark-groups-in-conkeror/]
let bookmark_groups = {
  "Krishan": [
    "https://www.mojarto.com/blogs/top-6-krishna-artworks-for-your-walls-this-janmashtami?pfrom=home-specialprojects",
      "https://www.mojarto.com/blogs/top-6-krishna-artworks-for-your-walls-this-janmashtami?pfrom=home-specialprojects",
  ],
    "Crypto": [
        "http://news.mit.edu/topic/cryptography",
        "https://www.google.co.in/search?q=cryptography+News",
    ],
    "Coding": [
        "https://www.securecoding.cert.org",
        "http://resources.sei.cmu.edu/library/asset-view.cfm?assetid=473603",
    ],
    "SoftEngg": [
        "https://www.cert.org",
        "https://www.cert.org/news/",
    ],
    "Network": [
        "https://tools.ietf.org/dailydose/",
        "https://www.icann.org/news",
        "https://www.similarweb.com/website/ietf.org#similarSites",
    ],
  "NewsTech": [
    "http://www.osnews.com",
  ],
    "Linux": [
        "http://www.linuxtoday.com",
        "http://xmodulo.com",
        "http://planet.ubuntu.com"
    ],
  "NewsScience": [
    "https://www.newscientist.com",
    "https://www.sciencedaily.com",
    "http://phys.org/",
    "http://www.sciencemag.org",
    "https://www.quantamagazine.org",
    "http://www.nature.com",
    "https://www.sciencenews.org",
      "https://plus.maths.org/content/News",
  ],
  "NewsIndia": [
    "http://www.aninews.in",
    "http://www.ptinews.com",
    "http://www.ndtv.com",
      "http://www.jantakareporter.com",
  ],
  "NewsWorld": [
    "http://www.bbc.com",
    "http://www.economist.com",
    "http://www.huffingtonpost.com",
    "http://www.theatlantic.com",
    "http://sputniknews.com",
    "https://www.theguardian.com",
    "http://www.telegraph.co.uk",
    "http://dunyanews.tv",
      "http://www.dawn.com",
  ],
  "NewsOffBeats": [
    "http://www.bbc.com/future",
    "http://www.wired.com",
    "http://www.messagetoeagle.com",
      "http://www.sci-news.com",
  ],
  "NewsTv": [
    "http://www.reuters.tv",
  ],
  "NewsHindi": [
    "http://khabar.ndtv.com",
    "http://www.bhaskar.com",
    "http://www.amarujala.com",
    "http://www.jagran.com",
      "http://hindi.boldsky.com",
  ],
  "NewsElectronics": [
    "https://www.quora.com/What-are-the-top-10-websites-electronics-communication-students-must-visit",
    "http://www.physics-and-radio-electronics.com/electronic-devices-and-circuits.html",
    "http://www.allaboutcircuits.com/",
    "http://www.extremetech.com/category/electronics",
    "http://www.circuitstoday.com",
    "http://electronicdesign.com",
    "http://www.eetimes.com",
      "http://www.embedded.com",

  ],
  "NewsAgra": [
    "http://www.amarujala.com/uttar-pradesh/agra",
    "http://www.jagran.com/local/uttar-pradesh_agra-city-news-hindi.html",
      "http://www.bhaskar.com/uttar-pradesh/agra/",
  ],
  "NewsGossip": [
      "https://thedirty.com",
  ],
  "NewsIndianHistory": [
      "http://www.stephen-knapp.com",
  ],
  "NewsPsychology": [
    "http://www.apa.org",
    "https://www.psychologytoday.com",
  ],
  "NewsDIY": [
      "http://www.instructables.com",
  ],
  "Lua": [
    "http://www.lua.org/manual/5.2/",
      "http://www.love2d.org/wiki/Main_Page",
  ],
  "Reddit": [
    "http://reddit.com/r/programming",
    "http://reddit.com/r/emacs",
    "http://reddit.com/r/coolgithubprojects",
    "http://reddit.com/r/lua",
  ],
  "Scheme": [
    "http://www.schemers.org/Documents/Standards/R5RS/HTML/",
    "http://scsh.net/docu/html/man-Z-H-1.html#node_toc_start",
      "http://scsh.net/docu/docu.html",
  ],
  "Bloodborne": [
    "https://www.reddit.com/r/huntersbell/new/",
      "http://bloodborne.wikidot.com/",
  ],
    "Pending": [
        "https://www.google.co.in/search?q=Xwindow+with+cairo+xlib++XCB+good+intro+book",
        "https://tronche.com/gui/x/xlib-tutorial/2nd-program-anatomy.html",
        "https://en.wikipedia.org/wiki/X_Toolkit_Intrinsics",
        "https://www.google.co.in/search?q=Xorg+how+to+make+translucent+window",
        "http://conky.pitstop.free.fr/wiki/index.php5?title=Goto/offset/voffset_(en)",
        "https://insights.ubuntu.com/2015/06/30/publishing-lxd-images/",
        "https://github.com/spencertipping/conky-compiler",
        "https://www.google.co.in/search?q=X+window+writing+into+Root+Window",
    ]


};

interactive("open-bookmark-group",
    "Opens all bookmarks in the given group name.",
    function (I) {
      var name = yield I.minibuffer.read(
        $prompt = "Group", $history = "Group",
        $completer = new prefix_completer($completions = Object.keys(bookmark_groups)),
        $default_completion = "Plutono",
        $require_match,
        $auto_complete);

      bookmark_groups[name].forEach(
        function (element, index, array) {
          load_url_in_new_buffer(element, I.window);
      });
    });

define_key(content_buffer_normal_keymap, "C-c b", "open-bookmark-group");
//}}


//{{ https://bugzilla.mozilla.org/show_bug.cgi?id=55181
// https://bugzilla.mozilla.org/show_bug.cgi?id=55181#c92
// Queries the HTTPAuth Manager and clears all sessions
function httpAuthPasswordExpire() {
    var httpAuth = Components.classes['@mozilla.org/network/http-auth-manager;1'];
    httpAuth = httpAuth.getService();
    httpAuth = httpAuth.QueryInterface(Components.interfaces.nsIHttpAuthManager);
    httpAuth.clearAll();
}

// Components.classes['@mozilla.org/network/http-auth-manager;1']. //
//     getService(Components.interfaces.nsIHttpAuthManager). //
//     clearAll(); //

// var wallet = Components. //
//     classes['@mozilla.org/wallet/wallet-service;1']. //
//     getService(Components.interfaces.nsIWalletService); //

interactive("clear-http-auth",
            "Clear http auth.",
            function (I) {
                httpAuthPasswordExpire();
            });

// define_key(content_buffer_normal_keymap, "C-c b", "open-bookmark-group");

//}}

// tab_bar_mode(false); //disable

// Local Variables: **
// folded-file:t **
// mode:js **
// comment-column:0 **
// comment-start: "// "  **
// comment-end:   "// "  **
// End: **
