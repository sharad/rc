

// 2.1.1. Setup

// After installing MozRepl, add one of the following blocks of code
// to your rc. The preference method will require an additional
// restart. The longer method allows you to easily customize the port.



user_pref('extensions.mozrepl.autoStart', false);
session_pref('extensions.mozrepl.autoStart', false);
// default port value is 4242 but it is used juniper VPN.
// so can not use it.
user_pref('extensions.mozrepl.port', 4747);
session_pref('extensions.mozrepl.port', 4747);

user_pref('extensions.mozrepl.loopbackOnly', true);
session_pref('extensions.mozrepl.loopbackOnly', true);

// Mozrepl
//
if ('@hyperstruct.net/mozlab/mozrepl;1' in Cc) {
  let mozrepl = Cc['@hyperstruct.net/mozlab/mozrepl;1']
    .getService(Ci.nsIMozRepl);

  var topprefs     = Cc["@mozilla.org/preferences-service;1"]
    .getService(Components.interfaces.nsIPrefService);
  var mozpref      = topprefs.getBranch("extensions.mozrepl.");
  var port         = mozpref.getIntPref('port')
  var loopbackOnly = mozpref.getBoolPref('loopbackOnly')

  if (! mozrepl.isActive()) mozrepl.start(port, loopbackOnly);
}

// 2.1.2. Environment Setup

// Here is how to set up mozrepl to start in the Conkeror application
// context. That is the main context that contains all of Conkeror's
// global objects.

// First make a file called ~/.mozrepl.js, which contains the code that follows.

// var conkeror = Cc["@conkeror.mozdev.org/application;1"]
//     .getService().wrappedJSObject;

// this.enter(conkeror);

// Now put the following code into your .conkerorrc:

let (mozrepl_init = get_home_directory()) {
  mozrepl_init.appendRelativePath(".mozrepl.js");
  session_pref('extensions.mozrepl.initUrl', make_uri(mozrepl_init).spec);
}

// 2.1.3. Setting up Convenient Accessors

// Instead of using repl.enter(conkeror), you could make a special
// context which provides convenient accessors like window, buffer,
// and document to access the most recently used of those elements in
// conkeror. In a context like this, you will be able to read all
// conkeror functions and variables, but in order to modify them, you
// must prefix variable names with conkeror..

// function repl_context() {
//     let ctx = {};
//     ctx.__proto__ = conkeror;
//     ctx.conkeror = conkeror;
//     ctx.window = conkeror.get_recent_conkeror_window();
//     ctx.buffer = ctx.window.buffers.current;
//     ctx.document = ctx.buffer.document;
//     return ctx;
// }

// this.enter(repl_context());



////////////////////////////////////////////////////////////////////////////////////
// //_ . «MOZREPL» (to ".MOZREPL")
// if ('@hyperstruct.net/mozlab/mozrepl;1' in Cc) {
//   var mozrepl = Cc['@hyperstruct.net/mozlab/mozrepl;1']
//     .getService(Ci.nsIMozRepl);
//   if (! mozrepl.isActive()) mozrepl.start(4242);
// }

// let (mozrepl_init = get_home_directory()) {
//     mozrepl_init.appendRelativePath(".mozrepl.js");
//     session_pref('extensions.mozrepl.initUrl', make_uri(mozrepl_init).spec);
// }
////////////////////////////////////////////////////////////////////////////////////





////////////////////////////////////////////////////////////////////////////////////
/**
*mozrepl
*/


// if ('@hyperstruct.net/mozlab/mozrepl;1' in Components.classes)
//     {
// 	var mozrepl = Cc['@hyperstruct.net/mozlab/mozrepl;1']
// 	    .getService(Components.interfaces.nsIMozRepl);
// 	if (! mozrepl.isActive())
// 	    mozrepl.start(4242);
//     }


// function repl_context() {
//     var ctx = {};
//     ctx.__proto__ = conkeror;
//     ctx.conkeror = conkeror;
//     ctx.window = window_watcher.activeWindow;
//     ctx.buffer = ctx.window.buffers.current;
//     ctx.document = ctx.buffer.document;
//     return ctx;
// }

//end mozrepl
////////////////////////////////////////////////////////////////////////////////////
