

// from http://bugs.conkeror.org/issue285

require("cookie.js"); // hopefully this would go in modules/cookie.js so this would not be needed

function for_each_host_cookie(host, fn) {
    var cookies = cookie_manager.getCookiesFromHost(host);
    while (cookies.hasMoreElements()) {
        var cookie = cookies.getNext().QueryInterface(Components.interfaces.nsICookie2);
        fn(cookie);
    }
}

function clear_host_cookies(host) {
    for_each_host_cookie(host,
                         function (cookie) {
                             cookie_manager.remove(cookie.host, cookie.name, cookie.path, false);
                         });
}

interactive("clear-site-cookies", "Delete all cookies for the current site",
            function (I) {
                var host = I.buffer.current_uri.host;
                clear_host_cookies(host);
                I.minibuffer.message("Cookies cleared for " + host);
            });
