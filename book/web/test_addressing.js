function init() {
    var sendAdd;
    var sAdd = Rx.Observable.create(function (observer) {
            sendAdd = function (a) { observer.onNext(a); };
        }).publish();
    sAdd.connect();
    var sendRemove;
    var sRemove = Rx.Observable.create(function (observer) {
            sendRemove = function (a) { observer.onNext(a); };
        }).publish();
    sRemove.connect();
    var sendIn;
    var sIn = Rx.Observable.create(function (observer) {
            sendIn = function (a) { observer.onNext(a); };
        }).publish();
    sIn.connect();
    var sCreated = addressing(sAdd, sRemove, sIn);
    sCreated.subscribe(function (o) {
        console.log("key: "+o.key+" extra: "+o.extra);
        o.sAddressee.subscribe(function (value) {
            console.log("["+o.key+"] "+value);
        });
    });
    sendAdd({ key : 10, extra : 'ten' });
    sendAdd({ key : 5, extra : 'five' });
    sendAdd({ key : 15, extra : 'fifteen' });
    sendAdd({ key : 20, extra : 'twenty' });
    sendAdd({ key : 7, extra : 'seven' });
    sendAdd({ key : 8, extra : 'eight' });
    sendAdd({ key : 0, extra : 'zero' });
    sendAdd({ key : 12, extra : 'twelve' });
    sendAdd({ key : 18, extra : 'eighteen' });
    function send_all() {
        sendIn({ key : 0, value : 'zero' });
        sendIn({ key : 5, value : 'five' });
        sendIn({ key : 7, value : 'seven' });
        sendIn({ key : 8, value : 'eight' });
        sendIn({ key : 10, value : 'ten' });
        sendIn({ key : 12, value : 'twelve' });
        sendIn({ key : 15, value : 'fifteen' });
        sendIn({ key : 18, value : 'eighteen' });
        sendIn({ key : 20, value : 'twenty' });
    }
    send_all();
    console.log('REMOVE 7');
    sendRemove(7);  // remove2 case
    send_all();
    console.log('REMOVE 8');
    sendRemove(8);  // remove3 case
    send_all();
    console.log('REMOVE 10');
    sendRemove(10);  // remove case
    send_all();
    console.log('REMOVE 0');
    sendRemove(0);
    send_all();
    console.log('REMOVE 18');
    sendRemove(18);
    send_all();
    console.log('REMOVE 12');
    sendRemove(12);
    send_all();
    console.log('REMOVE 15');
    sendRemove(15);
    send_all();
    console.log('REMOVE 5');
    sendRemove(5);
    send_all();
    console.log('REMOVE 20');
    sendRemove(20);
    send_all();
}

