// find about Conkerorrc http://conkeror.org/ConkerorRC
// http://conkeror.org/Tips
// http://conkeror.org/FrontPage

// from: http://www.mail-archive.com/conkeror@mozdev.org/msg00396.html
// Find about conkeror key binding
// There is perhaps limited help available via the M-x describe-bindings
// (C-h b) command.
// from: http://www.mail-archive.com/conkeror@mozdev.org/msg00396.html
// For bookmark, the default object class is frames.
// You can customize the default object class for a command by setting in
// your RC file e.g.:
// hints_default_object_classes["bookmarks"] = "links";
// See the documentation (C-h v) for hints_default_object_classes.



// https://svn.acceleration.net/JsShellServer/trunk/js-mode.el


// reload conkerorrc
interactive("reload-rc",
            "Reload the Conkerorrc.",
            function(I) { load_rc_file(get_home_directory().path + "/.conkerorrc/init.js"); }
           );

// init_webjumps();

// http://conkeror.org/UserVariables
url_remoting_fn = load_url_in_new_buffer;
can_kill_last_buffer = false;

// // from: http://dotfiles.org/~sofeng/.conkerorrc
// // new bindings
// define_key(default_global_keymap, "C-w", "kill-current-buffer");
//
// // rebound from global.js
// define_key(default_global_keymap, "h", "find-url");
// define_key(default_global_keymap, "f1", default_help_keymap);
// define_key(default_global_keymap, "b", "switch-to-buffer");
// define_key(default_global_keymap, "C-f", "isearch-forward");
// // note, i had to comment out the "C-f" binding in basic-commands.js to make this work
//
// // rebound from isearch.js
// define_key(isearch_keymap, "C-f", "isearch-continue-forward");
//
// // rebound from normal.js
// define_key(content_buffer_normal_keymap, "back_space", "go-back");
// define_key(content_buffer_normal_keymap, "S-back_space", "go-forward");

define_key(content_buffer_normal_keymap, "M-left", "go-back");
define_key(content_buffer_normal_keymap, "M-right", "go-forward");
//
// // rebound from element.js
// //define_key(content_buffer_normal_keymap, "k", "bookmark");
//
// // rebound from basic-commands.js
// define_key(content_buffer_normal_keymap, "home","beginning-of-line");
// define_key(content_buffer_normal_keymap, "end","end-of-line");
// define_key(content_buffer_normal_keymap, "C-home","cmd_scrollTop");
// define_key(content_buffer_normal_keymap, "C-end","cmd_scrollBottom");
// define_key(content_buffer_normal_keymap, "C-c","cmd_copy");
//
// // rebound from text.js
// define_key(content_buffer_text_keymap, "home", "cmd_beginLine");
// define_key(content_buffer_text_keymap, "end", "cmd_endLine");
// define_key(content_buffer_text_keymap, "C-left", "cmd_wordPrevious");
// define_key(content_buffer_text_keymap, "C-right", "cmd_wordNext");
// define_key(content_buffer_text_keymap, "C-y", "cmd_redo");
// define_key(content_buffer_text_keymap, "C-z", "cmd_undo");
// define_key(content_buffer_text_keymap, "C-v", "cmd_paste");
// define_key(content_buffer_text_keymap, "C-x", "cmd_cut");
// define_key(content_buffer_text_keymap, "C-c", "cmd_copy");
//
// // new for textarea.js
// define_key(content_buffer_textarea_keymap, "home", "cmd_beginLine");
// define_key(content_buffer_textarea_keymap, "end", "cmd_endLine");
// define_key(content_buffer_textarea_keymap, "C-left", "cmd_wordPrevious");
// define_key(content_buffer_textarea_keymap, "C-right", "cmd_wordNext");
// define_key(content_buffer_textarea_keymap, "C-y", "cmd_redo");
// define_key(content_buffer_textarea_keymap, "C-z", "cmd_undo");
// define_key(content_buffer_textarea_keymap, "C-v", "cmd_paste");
// define_key(content_buffer_textarea_keymap, "C-x", "cmd_cut");
// define_key(content_buffer_textarea_keymap, "C-c", "cmd_copy");
//
// // rebound from textarea.js
// define_key(content_buffer_textarea_keymap, "C-home", "cmd_moveTop");
// define_key(content_buffer_textarea_keymap, "C-end", "cmd_moveBottom");
//
// // rebound from minibuffer.js
// define_key(minibuffer_base_keymap, "home", "minibuffer-cmd_beginLine");
// define_key(minibuffer_base_keymap, "end", "minibuffer-cmd_endLine");
// define_key(minibuffer_base_keymap, "C-left", "minibuffer-cmd_wordPrevious");
// define_key(minibuffer_base_keymap, "C-right", "minibuffer-cmd_wordNext");
// define_key(minibuffer_base_keymap, "C-v", "minibuffer-cmd_paste");
// define_key(minibuffer_base_keymap, "C-c", "minibuffer-cmd_copy");
//
// // rebound from zoom.js
define_key(content_buffer_normal_keymap, "C-subtract", "zoom-out-text");
define_key(content_buffer_normal_keymap, "C-add", "zoom-in-text");
//


function RGBtoHSL(RGBColor){
  with(Math){
    var R,G,B;
    var cMax,cMin;
    var sum,diff;
    var Rdelta,Gdelta,Bdelta;
    var H,L,S;
    R=RGBColor[0];
    G=RGBColor[1];
    B=RGBColor[2];
    cMax=max(max(R,G),B);
    cMin=min(min(R,G),B);
    sum=cMax+cMin;
    diff=cMax-cMin;
    L=sum/2;
    if(cMax==cMin){
      S=0;
      H=0;
    }else{
      if (L<=(1/2)) S=diff/sum;
      else S=diff/(2-sum);
      Rdelta=R/6/diff;
      Gdelta=G/6/diff;
      Bdelta=B/6/diff;
      if(R==cMax)H=Gdelta-Bdelta;
      else if(G==cMax)H=(1/3)+Bdelta-Rdelta;
      else H=(2/3)+Rdelta-Gdelta;
      if(H<0)H+=1;
      if(H>1)H-=1;
    }
    return[H,S,L];
  }
}

function getRGBColor(node,prop){
  var rgb=getComputedStyle(node,null).getPropertyValue(prop);
  var r,g,b;
  if(/rgb\((\d+),\s(\d+),\s(\d+)\)/.exec(rgb)){
    r=parseInt(RegExp.$1,10);
    g=parseInt(RegExp.$2,10);
    b=parseInt(RegExp.$3,10);
    return[r/255,g/255,b/255];
  }
  return rgb;
}

function hslToCSS(hsl){
  return "hsl("+Math.round(hsl[0]*360)+", "+Math.round(hsl[1]*100)+"%, "+Math.round(hsl[2]*100)+"%)";
}

function revl(n){
  var i,x,color,hsl;
  if(n.nodeType==Node.ELEMENT_NODE){
    for(i=0;
        x=n.childNodes[i];
        ++i)revl(x);
    for(i=0;
        x=props[i];
        ++i){
      color=getRGBColor(n,x);
      if(typeof(color)!="string"){
        hsl=RGBtoHSL(color);
        hsl[2]=1-hsl[2];
        n.style[props2[i]]=hslToCSS(hsl);
      }
    }
  }
}

interactive("colorizex",
            "colorizex.",
            function(I) {
              var props=["color","background-color","border-left-color","border-right-color","border-top-color","border-bottom-color"];
              var props2=["color","backgroundColor","borderLeftColor","borderRightColor","borderTopColor","borderBottomColor"];
              if(typeof getRGBColor(document.documentElement,"background-color")=="string")
                document.documentElement.style.backgroundColor="white";
              revl(document.documentElement);
            }
           );



// Open Buffer From Terminal. You can open a website from the command
// line straight forward with conkeror www.debian.org. A new instance
// of conkeror pops up. If you instead want a new buffer in the
// existing conkeror instance, instead use
url_remoting_fn = load_url_in_new_buffer;

// xkcd. Prints out the alternative text below the window. Without
// this changing to conkeror would be unthinkable:
xkcd_add_title = true;

//

// Javascript console
// from: http://bc.tech.coop/blog/060603.html
// function jsconsole(args)
// {
//    var prefix = args[0];
//    open_url_in (prefix, "chrome://global/content/console.xul");
// }
// add_command("jsconsole", jsconsole, [["p"]]);


function loadjscssfile(filename, filetype){
 if (filetype=="js"){ //if filename is a external JavaScript file
  var fileref=document.createElement('script')
  fileref.setAttribute("type","text/javascript")
  fileref.setAttribute("src", filename)
 }
 else if (filetype=="css"){ //if filename is an external CSS file
  var fileref=document.createElement("link")
  fileref.setAttribute("rel", "stylesheet")
  fileref.setAttribute("type", "text/css")
  fileref.setAttribute("href", filename)
 }
 if (typeof fileref!="undefined")
  document.getElementsByTagName("head")[0].appendChild(fileref)
}

// loadjscssfile("myscript.js", "js") //dynamically load and add this .js file
// loadjscssfile("javascript.php", "js") //dynamically load "javascript.php" as a JavaScript file
// loadjscssfile("mystyle.css", "css") ////dynamically load and add this .css file

let (home = get_home_directory()) { // already ion init.js
    user_pref("network.protocol-handler.external.mailto", true);
    user_pref("network.protocol-handler.app.mailto", home.append("bin/gnu-mailto"));
};


// get color at http://www.webmonkey.com/2010/02/color_charts/
user_pref("browser.display.use_system_colors", false);
user_pref("browser.active_color", "#EE0000");
user_pref("browser.anchor_color", "#0000EE");
// user_pref("browser.display.background_color", "#99CC99");
user_pref("browser.display.background_color", "#669933");
// user_pref("browser.display.foreground_color", "#000000");
user_pref("browser.display.foreground_color", "#003333");

// user_pref("browser.visited_color", "#551A8B");
user_pref("browser.visited_color", "#660033");




;;{{
;; A script on this page may be busy, or it may have stopped responding. You can stop the script now, or you can continue to see if the script will complete.
;; Script: resource://gre/modules/XPCOMUtils.jsm:324

;; http://www.google.co.in/search?q=%20firefox%20A%20script%20on%20this%20page%20may%20be%20busy%2C%20or%20it%20may%20have%20stopped%20responding.%20You%20can%20stop%20the%20script%20now%2C%20or%20you%20can%20continue%20to%20see%20if%20the%20script%20will%20complete.&hl=en&source=hp&aq=f&aqi=g10&aql=&oq=&gs_rfai=
;; http://support.mozilla.org/en-US/kb/warning-unresponsive-script

user_pref("dom.max_script_run_time", 7);
user_pref("dom.max_script_run_time", 7);

session_pref("dom.max_script_run_time", 7);
session_pref("dom.max_script_run_time", 7);

;;}}
