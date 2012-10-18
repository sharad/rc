
interactive("hide-div",
            "load jquery.js into this page, then your command",
            function(I) {
                var doc = I.buffer.document;
                var divs = doc.getElementsByTagName("div");
                var completions = new Array();
                for (i=0; i< divs.length; i++) {
                    if (divs[i].id !== "") {
                        completions.push(divs[i].id);
                    }
                }
                var completer = prefix_completer($completions = completions);
                var imp = yield I.minibuffer.read($prompt = "div: ", $history = "hide-div",
                                                  $completer = completer);
                                                  // $initial_value = completions[1]);
                jquery_this_doc(I.buffer.document,
                                (yield("$('#" +
                                       // (yield I.minibuffer.read($prompt = "div: ", $history = "hide-div")) +
                                       imp +
                                       "').toggle();")));
                // Providing a $history is what allows editing/replay of earlier commands
            });

interactive("hidex-div",
            "load jquery.js into this page, then your command",
            function(I) {
                bo = yield read_browser_object(I);
                bo.style.display = 'none';
                // I.window.minibuffer.message(bo.id);
                // Providing a $history is what allows editing/replay of earlier commands
            });

interactive("width-div",
            "load jquery.js into this page, then your command",
            function(I) {
                bo = yield read_browser_object(I);
                // I.window.minibuffer.message(bo.style.width);

                var curr_width = parseInt(bo.style.width); // removes the "px" at the end
                bo.style.width = (curr_width + 50) +"px";
                // I.window.minibuffer.message(bo.id);
                // Providing a $history is what allows editing/replay of earlier commands
            });

interactive("handle-link",
            "load jquery.js into this page, then your command",
            function(I) {
                bo = yield read_browser_object(I);
                mylink = load_spec_uri_string(load_spec(encodeURIComponent(bo)));
                check_buffer(I.buffer, content_buffer);
                yield((yield I.minibuffer.read($prompt = "handler: ", $history = "handler", $completer = link-handlers)) +
                      "(" + mylink + ")");
            });

interactive("width-auto-div",
            "load jquery.js into this page, then your command",
            function(I) {
                var doc = I.buffer.document;
                var divs = doc.getElementsByTagName("div");
                var completions = new Array();
                for (i=0; i< divs.length; i++) {
                    if (divs[i].id !== "") {
                        completions.push(divs[i].id);
                    }
                }
                var completer = prefix_completer($completions = completions);
                var imp = yield I.minibuffer.read($prompt = "div: ", $history = "hide-div",
                                                  $completer = completer);
                                                  // $initial_value = completions[1]);
                jquery_this_doc(I.buffer.document,
                                (yield("$('#" +
                                       // (yield I.minibuffer.read($prompt = "div: ", $history = "hide-div")) +
                                       imp +
                                       "').css('width', 'auto');")));
                // Providing a $history is what allows editing/replay of earlier commands
            });

interactive("widthx-auto-div",
            "load jquery.js into this page, then your command",
            function(I) {
                bo = yield read_browser_object(I);
                bo.style.width = 'auto';
                // I.window.minibuffer.message(bo.id);
                // Providing a $history is what allows editing/replay of earlier commands
            });


interactive("reenable-copy",
            "load jquery.js into this page, then your command",
            function(I) {
                // var doc = I.buffer.document;
                // doc.onselectstart = function(){return true;}
                I.buffer.document.onselectstart = function() {return true;} // ie
                I.buffer.document.onmousedown = function() {return true;} // mozilla
                var All = I.buffer.document.getElementsByTagName('*');
                for(i=0; i< All.length; i++) {
                    All[i].onselectstart = function() {return true;} // ie
                    All[i].onmousedown = function() {return true;} // mozilla
                }

            });


