var jsonpCallbacks = {
    cntr: 0
};

function lookup(url, sRequest) {
    var sResponse = Rx.Observable.create(function (observer) {
        return sRequest.subscribe(function(req) {
            var fnName = "fn" + jsonpCallbacks.cntr++,
                script = document.createElement("script");
            script.type = "text/javascript";
            script.src = url+encodeURIComponent(req) +
                                   "&callback=jsonpCallbacks." + fnName;
            jsonpCallbacks[fnName] = function(resp) {
                delete jsonpCallbacks[fnName];
                document.body.removeChild(script);
                observer.onNext([req, resp]);
            };
            document.body.appendChild(script);
        });
    }).publish();
    sResponse.connect();
    return sResponse;
}

function escapeHTML(text) {
    return text.replace(/&/g, '&amp;')
               .replace(/"/g, '&quot;')
			   .replace(/'/g, '&#39;')
			   .replace(/</g, '&lt;')
			   .replace(/>/g, '&gt;');
}

function calm(s) {
    return s.scan([null, null], function(prev_out, thiz) {
        return [thiz, thiz != prev_out[0] ? thiz : null];
    }).map(function(tpl) {
        return tpl[1];
    }).filter(function(a) {
        return a !== null;
    });
}

function currentTextOf(input) {
    var sKeyPresses = Rx.Observable.fromEvent(input, 'keyup'),
        text = new Rx.BehaviorSubject(input.value);
    sKeyPresses.map(function (e) { return input.value; }).subscribe(text);
    return text;
}

function autocomplete(textEdit) {
    var popup = document.createElement('select');
    popup.size = 15;
    popup.style.position = 'absolute';
    popup.style.zIndex = 100;
    popup.style.display = 'none';
    popup.style.width = textEdit.offsetWidth;
    popup.setAttribute('id', 'popup');
    document.body.appendChild(popup);
    var sClicked = Rx.Observable.fromEvent(popup, 'change')
                                .map(function (e) {
        return popup.value;
    });
    sClicked.subscribe(function (text) {
        return textEdit.value = text;
    });
    var editText = currentTextOf(textEdit),
        sKeyPresses = Rx.Observable.fromEvent(textEdit, 'keyup'),
        sDebounced = sKeyPresses.startWith(null).debounce(100),
        sTextUpdate = calm(sDebounced.withLatestFrom(editText,
            function (key, text) { return text; }));
    var sTabKey = sKeyPresses.filter(function(k) {
            return k.keyCode == 9; }),
        sEscapeKey = sKeyPresses.filter(function(k) {
            return k.keyCode == 27; }),
        sEnterKey = sKeyPresses.filter(function(k) {
            return k.keyCode == 13; });
    var sClearPopUp = sEscapeKey.merge(sEnterKey)
                                .merge(sClicked).map(null);
        lookedUp = lookup("http://gd.geobytes.com/AutoCompleteCity?q=",
            sTextUpdate.merge(sTabKey.withLatestFrom(editText,
                function (key, text) {
                    return text;
                }
            ))
        ).map(function (req_resp) {
            var req = req_resp[0],
                resp = req_resp[1];
            return resp.length == 1 && (resp[0] == "%s"
                     || resp[0] == "" || resp[0] == req) ? null : resp;
        }).merge(sClearPopUp).startWith(null);
    lookedUp.subscribe(function(items) {
        if (items !== null) {
            var html = '';
            for (var i = 0; i < items.length; i++) {
                html += '<option>' + escapeHTML(items[i]) + '</option>';
            }
            popup.innerHTML = html;
            if (popup.style.display != 'block') {
                popup.style.left = textEdit.offsetLeft;
                popup.style.top = textEdit.offsetTop +
                                            textEdit.offsetHeight;
                popup.style.display = 'block';
            }
        }
        else {
            popup.style.display = 'none';
        }
    });
    return sEnterKey.withLatestFrom(editText, function (key, text) {
        return text;
    }).merge(sClicked);
}

function init() {
    var cityInput = document.getElementById("city"),
        infoDiv = document.getElementById("info"),
        sEntered = autocomplete(cityInput);
    lookup("http://getcitydetails.geobytes.com/GetCityDetails?fqcn=",
           sEntered).subscribe(function (city_info) {
        var city = city_info[0],
            info = city_info[1];
        var html =  'Information for <b>' + escapeHTML(city) +
                    '</b>' + '<table>';
        for (var key in info) {
            html += '<tr><td>' + escapeHTML(key) + '</td><td>' +
                    escapeHTML(info[key]) + '</td></tr>';
        }
        html += '</table>';
        infoDiv.innerHTML = html;
    });
}
