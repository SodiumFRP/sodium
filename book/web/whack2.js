function mkMole(id, x, y, clock, sClick)
{
    var tRise = 100,
        tWhack = 15,
        tUp = 500;
    function drawFace(ctx, x, y, color) {
        ctx.beginPath();
        ctx.arc(x,y,20,0,2*Math.PI);
        ctx.fillStyle = color;
        ctx.fill();
        ctx.stroke();
        ctx.beginPath();
        ctx.moveTo(x - 10, y - 1); ctx.lineTo(x - 5, y - 3);
        ctx.moveTo(x + 5, y - 3);  ctx.lineTo(x + 10, y - 1);
        ctx.stroke();
    }
    function drawMole(ctx, x, y, up, fracVisible) {
        if (up)
            drawFace(ctx, x, y, '#dfdf00');
        else {
            ctx.save();
            ctx.beginPath();
            ctx.rect(x - 21, y - 21, 42, 42);
            ctx.clip();
            drawFace(ctx, x, y + 40 - fracVisible * 40, '#cf7f00');
            ctx.restore();
            ctx.beginPath();
            ctx.moveTo(x - 26, y + 21);
            ctx.lineTo(x + 26, y + 21);
            ctx.stroke();
        }
    }
    var state = new Rx.BehaviorSubject({ phase : 'rising',
                                         t0 : clock.getValue() }),
        sUp = clock.withLatestFrom(state,
                function (t, state) {
                    return state.phase == 'rising' &&
                               t - state.t0 >= tRise
                       ? { phase : 'up', t0 : t }
                       : null;
                })
            .filter(function (state) { return state !== null; }),
        sWhack = sClick.withLatestFrom(clock, state,
                function (_, t, state) {
                    var dt = t - state.t0;
                    return state.phase == 'rising'
                        ? { phase : 'whacked',
                            t0 : t - (1 - dt / tRise) * tWhack }
                        : null;
                })
            .filter(function (state) { return state !== null; }),
        subscr1 = sUp.merge(sWhack).subscribe(state),
        drawable = state.map(function (state) {
            return state.phase == 'rising' ? function (ctx, t) {
                       var dt = t - state.t0;
                       drawMole(ctx, x, y, false, dt / tRise); } :
                   state.phase == 'up' ? function (ctx, _) {
                       drawMole(ctx, x, y, true, 1); } :
                   function (ctx, t) {
                       var dt = t - state.t0;
                       if (dt < tWhack)
                           drawMole(ctx, x, y, false,
                               1 - dt / tWhack); };
        }),
        sDestroy = clock
            .withLatestFrom(state,
                function (t, st) {
                    var dur = t - st.t0;
                    return (st.phase == 'up' && dur >= tUp)
                        || (st.phase == 'whacked' && dur >= tWhack)
                                     ? id : null;
                })
            .filter(function (id) { return id !== null; });
    return {
        id : id,
        drawable : drawable,
        sDestroy : sDestroy,
        dispose  : function () { subscr1.dispose(); }
    };
}

function sequence(xs)
{
    if (xs.length == 0)
        return new Rx.BehaviorSubject([]);
    else
    if (xs.length == 1)
        return xs[0].map(function(x) { return [x]; });
    else {
        var mid = Math.floor(xs.length/2),
            left = xs.slice(0, mid),
            right = xs.slice(mid);
        return sequence(left).combineLatest(sequence(right),
                function (x1, x2) { return x1.concat(x2); });
    }
}

function init() {
    var canvas = document.getElementById("myCanvas"),
        getXY = function(e) {
            return { x : e.pageX - canvas.offsetLeft,
                     y : e.pageY - canvas.offsetTop }; },
        sMouseDown = Rx.Observable.fromEvent(canvas, 'mousedown')
                                  .map(getXY),
        clock = new Rx.BehaviorSubject(0);
    Rx.Observable.interval(20).subscribe(clock);
    var state = new Rx.BehaviorSubject({ nextID : 0, moles : []}),
        sAddMole = clock
            .filter(function (_) { return Math.random() < 0.02; })
            .withLatestFrom(state, clock,
                function (_, state, t0) {
                    var x = 25+(canvas.width-50) * Math.random();
                    var y = 25+(canvas.height-50) * Math.random();
                    var sClick = sMouseDown.filter(function (pt) {
                        return pt.x >= x - 20 && pt.x <= x + 20 &&
                               pt.y >= y - 20 && pt.y <= y + 30;
                    });
                    var newMoles = state.moles.slice();
                    newMoles.push(mkMole(state.nextID, x, y,
                                           clock, sClick));
                    state = { nextID : state.nextID+1,
                              moles : newMoles };
                    console.log("add mole "+state.nextID+
                        " ("+state.moles.length+")");
                    return state;
                }),
        sDestroy = state.flatMapLatest(
            function (state) {
                var sDestroy = Rx.Observable.of();
                for (var i = 0; i < state.moles.length; i++)
                    sDestroy = sDestroy.merge(state.moles[i].sDestroy);
                return sDestroy;
            });
        sRemoveMole = sDestroy.withLatestFrom(state,
            function (id, state) {
                var newMoles = [];
                for (var i = 0; i < state.moles.length; i++)
                    if (state.moles[i].id !== id)
                        newMoles.push(state.moles[i]);
                    else
                        setTimeout(state.moles[i].dispose, 0);
                console.log("remove mole "+id+" ("+newMoles.length+")");
                return { nextID : state.nextID, moles : newMoles };
            });
    sAddMole.merge(sRemoveMole).subscribe(state);
    var drawables = new Rx.BehaviorSubject([]);
    state.flatMapLatest(
        function (state) {
            var drawables = [];
            for (var i = 0; i < state.moles.length; i++)
                drawables.push(state.moles[i].drawable);
            return sequence(drawables);
        }).subscribe(drawables);
    clock.subscribe(function(t) {
        var ctx = canvas.getContext("2d");
        ctx.fillStyle = '#00af00';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        var ds = drawables.getValue();
        for (var i = 0; i < ds.length; i++)
            ds[i](ctx, t);
    });
}

