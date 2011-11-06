

function getalllinks(I) {
    var document = I.buffer.document
    var allLinks = document.links;
    for (var i=0; i<allLinks.length; i++) {
        document.write(allLinks[i].href+"<BR/>");
    }
}

interactive("getalllinks", "get all links", getalllinks);
