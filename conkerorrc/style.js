
//http://www.mail-archive.com/conkeror@mozdev.org/msg01139.html
// re-style google
require('selectively-unstyle.js');

let (mozdev = build_url_regex(
         $domain = "mozdev",
         $allow_www = true,
         $tlds = ["com", "co.uk", "de", "dk", "es",
                  "fr", "it", "no", "se", "uk"])) {
    selectively_unstyle_alist.push(
        [
            mozdev, function (sheet) {
            sheet.disabled 
            = true; }
        ]);
};

// register_agent_stylesheet("file:///path/to/dark-google.css");
