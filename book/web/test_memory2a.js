function init() {
    var sClock = Rx.Observable.interval(5),
        agent = new Rx.BehaviorSubject( { cText : null, dispose : null } );
    sClock.filter(function (t) { return t % 5 == 0; })
          .withLatestFrom(agent, function (t0, old) {
        var out = new Rx.BehaviorSubject("");
        var subscr1 = sClock.map(function (t) { return t0 + "/" + t; }).subscribe(out);
        if (old.dispose !== null)
            setTimeout(old.dispose, 0);
        return { cText : out, dispose : function () { subscr1.dispose(); } };
    }).subscribe(agent);
    var out = agent.flatMapLatest(function (out) {
        return out.cText === null ? Rx.Observable.of() : out.cText;
    });
    out.subscribe(function (text) { });
}
