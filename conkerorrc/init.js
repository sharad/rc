// -*- mode: js2 -*-



// {{ from: http://conkeror.org/Tabs
// To add tabs to your Conkeror configuration, add one of the following
// lines to your conkerorrc file (but not both):
// require("tab-bar.js"); // For the old classic tabs.
// require("new-tabs.js"); // For the new tabs
// Known problems
//
//     * Can cause flickering and 100% CPU time when having a certain
//     * number of tabs open. We think we know what causes this.
//     * Difficult or impossible to skin properly at the moment. There
//     * has been some discussion on the causes of this, but it really
//     * bottoms down to the "!important" CSS property.
//
require("tab-bar.js");
//
// }}

// http://conkeror.org/BreakingChanges

var al_load_dir;
var __rcfile = make_file((get_pref("conkeror.rcfile")));
var al_load_dir = make_file(__rcfile.target).parent;

function al_load (relpath) {
    var file = al_load_dir.clone();
    file.appendRelativePath( relpath );
    if (file.exists()) {
        load(file);
    }
    else
        dumpln("WARNING: file '" + file.path + "' does not exist");
};

function load_rc_files() {
    let (conkerorhome = get_home_directory().path + "/.conkerorrc/") {

        var rc_files = ["local/security.js",
                        "local/office.js",
                        // "functionality.js"
                        "custom.js",
                        "display.js",
                        "webjumps.js",
                        "proxy.js",
                        "misc.js",
                        "office.js",
                        "extention.js",
                        "repl.js",
                        "utils.js",
                        "commands.js",
                        "utils1.js",
                        "behaviour.js",
                        "modes.js",
                        "gsearch.js"
                       ]

        for (var f in rc_files) {
            // load(conkerorhome + rc_files[f]);
            dumpln("loading " + rc_files[f]);
            al_load(rc_files[f]);
            dumpln("loaded " + rc_files[f])
        }
    }
};
function load_local_files() {
    var files = ["/home/s/hell/.conkerorrc/local/security.js",
                 "/home/s/hell/.conkerorrc/local/office.js"];
    for(f in files) {
        sec = make_file(files[f])
        load(sec);
    }
};

load_local_files();
load_rc_files();



let (home = get_home_directory()) {
    home.append("foo.html");
    homepage = home.path;
};

define_variable("editor_shell_command", "/usr/bin/sensible-editor",
    "Shell command used to invoke an external editor.\n" +
    "This defaults to the value of the EDITOR environment variable.  If " +
    "`run_external_editor_function' is non-null, it is used instead to " +
    "invoke an external editor and the value of this variable is ignored." +
    "It is used as part of a shell command in the following two ways:\n" +
    "<editor_shell_command> <file>\n" +
    "<editor_shell_command> +<line> <file>");


define_variable("homepage", "http://www.toodledo.com/views/index.php",
                "The url loaded by default for new content buffers. " +
                "My home page. " +
                "Its previous value was chrome://conkeror-help/content/help.html");



//{{{ http://conkeror.org/PasswordManagement
// COMMENT IT IF FREEZ OCCURS.
session_pref("signon.rememberSignons", true);
session_pref("signon.expireMasterPassword", false);
// session_pref("signon.SignonFileName", "signons.txt");

Components.classes["@mozilla.org/login-manager;1"]
    .getService(Components.interfaces.nsILoginManager);
//}}}


//{{
for (var i = 0; i < get_recent_conkeror_window().buffers.count; i++)
{
  stop_loading(get_recent_conkeror_window().buffers.get_buffer(i));
}
//}}


//{{ mailto config
// let (home = get_home_directory().path) {
//     user_pref("network.protocol-handler.external.mailto", true);
//     user_pref("network.protocol-handler.app.mailto", home + "/bin/gnus-mailto");
// }
//}}
