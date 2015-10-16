function init() {
    var sendAdd;
    var sAdd = Rx.Observable.create(function (observer) {
            sendAdd = function (a) { observer.onNext(a); };
        }).publish();
    sAdd.connect();
    var sendIn;
    var sIn = Rx.Observable.create(function (observer) {
            sendIn = function (a) { observer.onNext(a); };
        }).publish();
    sIn.connect();
    var sEgress = route(sAdd, sIn);
    sEgress.subscribe(function (o) {
        console.log("key: "+o.key+" extra: "+o.extra);
        o.sOut.subscribe(function (value) {
            console.log("["+o.key+"] "+value);
        });
    });
    sendAdd({ key : 10, extra : 'ten' });
    sendAdd({ key : 5, extra : 'five' });
    sendAdd({ key : 15, extra : 'fifteen' });
    sendAdd({ key : 20, extra : 'twenty' });
    sendAdd({ key : 0, extra : 'zero' });
    sendIn({ key : 0, value : 'zero' });
    sendIn({ key : 5, value : 'five' });
    sendIn({ key : 10, value : 'ten' });
    sendIn({ key : 15, value : 'fifteen' });
    sendIn({ key : 20, value : 'twenty' });
    /*
    function tick() {
        sendIn({ key : 20, value : 'twenty' });
        setTimeout(tick, 1000);
    }
    tick();
    */
}

