function init() {
    var sClock = Rx.Observable.interval(5),
        agents = new Rx.BehaviorSubject([]);
    sClock.filter(function (t) { return t % 5 == 0; })
          .withLatestFrom(agents, function (t0, agents) {
        if (agents.length >= 5)
            agents = agents.slice(agents.length-5);
        else
            agents = agents.slice();
        var out = new Rx.BehaviorSubject("");
        sClock.map(function (t) { return t0 + "/" + t; }).subscribe(out);
        agents.push(out);
        return agents;
    }).subscribe(agents);
    var out = agents.flatMapLatest(function (agents) {
        var out = null;
        for (var i = 0; i < agents.length; i++)
            if (out === null)
                out = agents[i];
            else
                out = out.combineLatest(agents[i],
                    function (t1, t2) { return t1 + " " + t2; }
                    );
        return out === null ? new Rx.BehaviorSubject("") : out;
    });
    out.subscribe(function (text) { });
}
