/*
 * (C) Copyright 2008 Alexander Reinwarth
 *
 * Some simple functions for handling conkeror's connection through a
 * proxy. For simplification it is assumed that http, https, ftp and
 * gopher use the same proxy and port.
 *
 * "Works for me"
 */

tor_proxy_server_default  = "127.0.0.1";
tor_proxy_port_default    = 9050;
tor_proxy_profile_default = "tor";

function lwarn(level) {
    return false;
}

/* http://www.mail-archive.com/conkeror@mozdev.org/msg00952.html */

/*  Standard proxy host and port for proxy-host and proxy-port. */


// toggle proxy
interactive("proxy-toggle",
            "Toggle the proxy on and off.",
            function(I) {
              var proxytype = get_pref("network.proxy.type");
              user_pref("network.proxy.type", proxytype == 1 ? 0 : 1);
              I.minibuffer.message("proxy turned " + (proxytype == 1 ? "off" : "on"));
            });

interactive("proxy-host",
            "Change the HTTP proxy host.",
            function (I) {
              var hostnym;
              user_pref("network.proxy.http", hostnym = (yield I.minibuffer.read($prompt = "Host:")));
              I.minibuffer.message("host set to " + hostnym);
            });




define_variable("proxy_host","localhost",
                "Hostname which will be offered as a standard value by proxy-host");
define_variable("proxy_port","8080",
                "Port which will be offered as a standard value by proxy-host");

/* toggle proxy on/off */
interactive("proxy-toggle",
            "Toggle wether to use a proxy or not.",
            function(I) {
                user_pref("network.proxy.type", get_pref("network.proxy.type") == 1 ? 0 : 1);
                proxy_display_settings(I);
                // update the widget
                select_buffer_hook.run(I.buffer);
            });

/* set the proxy host */
function proxy_set_host(host){
    user_pref("network.proxy.http", host);
    user_pref("network.proxy.ssl", host);
    user_pref("network.proxy.ftp", host);
    user_pref("network.proxy.gopher", host);
}

interactive("proxy-host",
            "Change the proxy host.",
            function (I) {
                proxy_set_host((yield I.minibuffer.read($prompt = "Host:",
                                                        $initial_value=proxy_host)));
                proxy_display_settings(I);
                // update the widget
                select_buffer_hook.run(I.buffer);
            });


/* set the proxy port */
function proxy_set_port(port){
    user_pref("network.proxy.http_port",   parseFloat(port));
    user_pref("network.proxy.ssl_port",    parseFloat(port));
    user_pref("network.proxy.ftp_port",    parseFloat(port));
    user_pref("network.proxy.gopher_port", parseFloat(port));
}

interactive("proxy-port",
            "Change the proxy port.",
            function (I){
                proxy_set_port((yield I.minibuffer.read($prompt = "Port:",
                                                        $initial_value=proxy_port))) ;
                proxy_display_settings(I);
                // update the widget
                select_buffer_hook.run(I.buffer);
            });


/* display current proxy settings */

function proxy_display_settings (I){
    return I.window.minibuffer.message ((get_pref("network.proxy.http"))+ ":" +
                                        (get_pref("network.proxy.http_port"))+ ":" +
                                        (get_pref("network.proxy.type")  == 1 ? "on" : "off"));
}

interactive ("proxy-display-settings",
             "Display the current proxy settings",
             function (I){proxy_display_settings(I);}
             );

/* key bindings */

define_key(default_global_keymap, "C-c p t", "proxy-toggle");
define_key(default_global_keymap, "C-c p h", "proxy-host");
define_key(default_global_keymap, "C-c p p", "proxy-port");
define_key(default_global_keymap, "C-c p d", "proxy-display-settings");



/* Tor */
// user_pref('network.proxy.http',  "localhost");
// user_pref('network.proxy.http_port', 8118);
// user_pref('network.proxy.ssl',    "localhost");
// user_pref('network.proxy.ssl_port',    8118);
// user_pref('network.proxy.socks',  "localhost");
// user_pref('network.proxy.socks_port',  9050);
// user_pref('network.proxy.type', 1);
// pref("network.http.keep-alive", false);
// pref("network.http.max-persistent-connections-per-proxy", 0);
// pref("network.http.max-persistent-connections-per-server",i 0);




/*
 * A simple modeline-widget displaying the proxy settings
 */

function proxy_widget(window){
    this.name = "proxy-widget";
    text_widget.call(this, window);

    // update the widget when select_buffer_hook is run
    this.add_hook("select_buffer_hook");
}

proxy_widget.prototype.__proto__ = text_widget.prototype;

proxy_widget.prototype.update = function () {
    this.view.text =
        get_pref("network.proxy.http")      +
        ":"                                 +
        get_pref("network.proxy.http_port") +
        ":"                                 +
        (get_pref("network.proxy.type") == 1 ?
         "on" : "off");
};

/*
 * Add the widget to the modeline
 */

add_hook("mode_line_hook", mode_line_adder(proxy_widget));






//{{
//set the proxy server for this session only
// proxy_server_default = "proxy.server.com";
// proxy_port_default = 80;

// MOVED TO TOP
// tor_proxy_server_default = "127.0.0.1";
// tor_proxy_port_default = 9050;
// tor_proxy_profile_default = "tor";

function set_proxy_session(server, port) {
    var window = get_recent_conkeror_window();
    if (lwarn()) window.alert("begin set_proxy_session");

    if (!server) server = tor_proxy_server_default;
    if (!port)   port   = tor_proxy_port_default;

    if ("" == server) server = tor_proxy_server_default;
    if ("" == port)    port   = tor_proxy_port_default;

    // session_pref('network.proxy.ftp',    server);
    // session_pref('network.proxy.gopher', server);
    // session_pref('network.proxy.http',   server);
    session_pref('network.proxy.socks',  server);
    // session_pref('network.proxy.ssl',    server);

    // session_pref('network.proxy.ftp_port',    port);
    // session_pref('network.proxy.gopher_port', port);
    // session_pref('network.proxy.http_port',   port);

    session_pref('network.proxy.socks_port',  port);
    // session_pref('network.proxy.ssl_port',    port);

    session_pref('network.proxy.share_proxy_settings', false);
    session_pref('network.proxy.type', 1);
    session_pref('network.proxy.socks_version', 5);
    // browser_object_follow(buffer, OPEN_NEW_BUFFER, "https://check.torproject.org/");

    browser_object_follow(window.buffers.get_buffer(0),
                          OPEN_NEW_BUFFER,
                          "https://check.torproject.org/");

    if (lwarn()) window.alert("finish set_proxy_session");
    return true;
}

function unset_proxy_session() {
    var window = get_recent_conkeror_window();
    if (lwarn()) window.alert("unset_proxy_session");
    session_pref('network.proxy.type', 0); //direct connection
    if (lwarn()) window.alert("unset_proxy_session");
    return true;
}

function read_yield(minibuffer, prompt) {
    yield minibuffer.read($prompt = prompt);
}

function read_proxy_session_X (I, server, port) {

    if (lwarn()) I.window.alert("read_proxy_session");

    var window = I.window;
    var buffer = I.buffer;

    // see how to make YIELD working
    // if (!server)
    //     server = (read_yield(I.minibuffer, "server [" + tor_proxy_server_default + "] or N: ").next());
    // if (!port)
    //     port   = (read_yield(I.minibuffer, "port [" + tor_proxy_port_default + "]: ").next());

    if (server == "N") {
        if (unset_proxy_session())
            window.minibuffer.message("Direction connection to the internet enabled for this session");
        else
            window.minibuffer.message("Direction connection to the internet NOT enabled for this session");
    } else {
        if (set_proxy_session(server, port))
            window.minibuffer.message("All protocols using " + server + ":" + port + " for this session");
        else
            window.minibuffer.message("All protocols NOT using " + server + ":" + port + " for this session");
  }
}

interactive("set-proxy-session",
            "set the proxy server for all protocols for this session only",
            function (I) {
              read_proxy_session(
                I// ,
                // (yield I.minibuffer.read($prompt = "server ["+tor_proxy_server_default+"] or N: ")),
                // (yield I.minibuffer.read($prompt = "port ["+tor_proxy_port_default+"]: "))
              );
            });

interactive("tor-enable",
            "set the proxy server for all protocols for this session only",
            function (I) {
                if (lwarn()) I.window.alert("Test");
                read_proxy_session_X(
                    I,
                    (yield I.minibuffer.read($prompt = "server [" + tor_proxy_server_default + "] or N: ")),
                    (yield I.minibuffer.read($prompt = "port ["   + tor_proxy_port_default   + "]: ")));
                if (lwarn()) I.window.alert("Test Done");
            });

function proxy_set (profile) {
    var window = get_recent_conkeror_window();
    if (lwarn()) window.alert("Hello");

    if (!profile) profile = tor_proxy_profile_default;

    if (get_current_profile() == profile)
    {
        if (lwarn()) window.alert("calling set_proxy_session()");
        set_proxy_session();
        if (lwarn()) window.alert("finished calling set_proxy_session()");
    }
}

let _proxy_set = function (window) {
    // remove_hook("make_window_hook", proxy_set);
    proxy_set("tor");
};

// add_hook("make_window_hook", _proxy_set);
add_hook("window_initialize_late_hook", _proxy_set);

// for testing
// load(make_file("/home/s/hell/.conkerorrc/proxy.js"))
//}}
