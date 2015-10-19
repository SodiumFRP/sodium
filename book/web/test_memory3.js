function init() {
    var sClock = Rx.Observable.interval(5),
        agent = new Rx.BehaviorSubject(null);
    sClock.filter(function (t) { return t % 5 == 0; })
          .withLatestFrom(agent, function (t0, old_out) {
        return sClock.map(function (t) { return t0 + "/" + t; });
    }).subscribe(agent);
    var out = agent.flatMapLatest(function (out) {
        return out === null ? Rx.Observable.of() : out;
    });
    out.subscribe(function (text) { });
}
