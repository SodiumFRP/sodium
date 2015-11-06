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
        subscr1 = sUp.merge(sWhack).subscribe(state);
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
    var slotsX = Math.floor(canvas.width / 60);
        slotsY = Math.floor(canvas.height / 60);
        slots = slotsX * slotsY;
    function slotX(slot) { return (slot%slotsX)*60 + 30; }
    function slotY(slot) { return Math.floor(slot/slotsX)*60 + 30; }
    function slotAt(x, y) {
        return Math.floor(x / 60) + Math.floor(y / 60) * slotsX;
    }
    var sAdd0 = new Rx.Subject(),
        sRemove0 = new Rx.Subject(),
        sIn0 = new Rx.Subject(),
        sCreated = addressing(sAdd0, sRemove0, sIn0),
        moles = new Rx.BehaviorSubject([]);
    clock.filter(function (_) { return Math.random() < 0.02; })
         .map(function (_) {
             var id = Math.floor(slots * Math.random());
             return { key : id, extra : id };
         })
         .subscribe(sAdd0);
    var sAddMole = sCreated
            .withLatestFrom(moles, clock,
                function (created, moles, t0) {
                    var sClick = created.sAddressee,
                        id = created.extra,
                        x = slotX(id), y = slotY(id);
                    console.log("add mole "+id);
                    var mole = mkMole(id, x, y, clock, sClick);
                    moles = moles.slice();
                    moles.push(mole);
                    return moles;
                }),
        sDestroy = moles.flatMapLatest(
            function (moles) {
                var sDestroy = Rx.Observable.of();
                for (var i = 0; i < moles.length; i++)
                    sDestroy = sDestroy.merge(moles[i].sDestroy);
                return sDestroy;
            });
        sRemoveMole = sDestroy.withLatestFrom(moles,
            function (id, moles) {
                var newMoles = [];
                for (var i = 0; i < moles.length; i++)
                    if (moles[i].id != id)
                        newMoles.push(moles[i]);
                    else
                        setTimeout(moles[i].dispose, 0);
                console.log("remove mole "+id);
                return newMoles;
            });
    sDestroy.subscribe(sRemove0);
    sAddMole.merge(sRemoveMole).subscribe(moles);
    var drawables = new Rx.BehaviorSubject([]);
    moles.flatMapLatest(
        function (moles) {
            var drawables = [];
            for (var i = 0; i < moles.length; i++)
                drawables.push(moles[i].drawable);
            return sequence(drawables);
        }).subscribe(drawables);
    sMouseDown.map(function (pt) {
            return { key : slotAt(pt.x, pt.y),
                     value : true };
        }).subscribe(sIn0);
    clock.subscribe(function(t) {
        var ctx = canvas.getContext("2d");
        ctx.fillStyle = '#00af00';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        var ds = drawables.getValue();
        for (var i = 0; i < ds.length; i++)
            ds[i](ctx, t);
    });
}

