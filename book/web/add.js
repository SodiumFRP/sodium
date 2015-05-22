function currentTextOf(input) {
    var sKeyPresses = Rx.Observable.fromEvent(input, 'keyup'),
        text = new Rx.BehaviorSubject(input.value);
    sKeyPresses.map(function (e) { return input.value; }).subscribe(text);
    return text;
}
function init() {
    var a = currentTextOf(document.getElementById('a'))
            .map(function(text) { return parseInt(text); }),
        b = currentTextOf(document.getElementById('b'))
            .map(function(text) { return parseInt(text); }),
        cSpan = document.getElementById('c');
    var c = a.combineLatest(b, function(aa, bb) { return aa + bb; });
    c.subscribe(function(cc) { cSpan.innerHTML = cc; });
}
