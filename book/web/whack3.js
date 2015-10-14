function mkMole(id, x, y, clock, sClick)
{
    var tRise = 100;
    var tWhack = 15;
    var tUp = 500;
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
            .filter(function (state) { return state !== null; });
        sWhack = sClick.withLatestFrom(clock, state,
                function (_, t, state) {
                    var dt = t - state.t0;
                    return state.phase == 'rising'
                        ? { phase : 'whacked',
                            t0 : t - (1 - dt / tRise) * tWhack }
                        : null;
                })
            .filter(function (state) { return state !== null; });
    sUp.merge(sWhack).subscribe(state);
    var drawable = state.map(function (state) {
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
        });
    var sDestroy = clock
            .withLatestFrom(state,
                function (t, st) {
                    var dur = t - st.t0;
                    return (st.phase == 'up' && dur >= tUp)
                        || (st.phase == 'whacked' && dur >= tWhack)
                                     ? id : null;
                })
            .filter(function (id) { return id != null; });
    return {
        id : id,
        drawable : drawable,
        sDestroy : sDestroy
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

function insert(tree, key, value) {
    return tree === null ?   { left : null, right : null,
                               key : key, value : value } :
        key === tree.key ?   { left : tree.left, right : tree.right,
                               key : key, value : value } :
        key < tree.key ?     { left : insert(tree.left, key, value),
                               right : tree.right,
                               key : tree.key, value : tree.value }
                           : { left : tree.left,
                               right : insert(tree.right, key, value),
                               key : tree.key, value : tree.value };
}

function lookup(tree, key) {
    return tree === null ? null :
        key == tree.key ? tree.value :
        key < tree.key  ? lookup(tree.left, key)
                        : lookup(tree.right, key);
}

function remove(tree, key) {
    if (tree === null) return null; else
    if (key === tree.key) {
        if (tree.left === null) return tree.right; else
        if (tree.right === null) return tree.left; else {
            function minimum(tree) {
                return tree.left === null ? tree
                                          : minimum(tree.left);
            }
            var m = minimum(tree.right);
            return { left : tree.left,
                     right : remove(tree.right, m.key),
                     key : m.key, value : m.value };
        }
    }
    else
    if (key < tree.key)
        return { left : remove(tree.left, key),
                 right : tree.right,
                 key : tree.key, value : tree.value };
    else
        return { left : tree.left,
                 right : remove(tree.right, key),
                 key : tree.key, value : tree.value };
}

function valuesToList(tree) {
    var vals = [];
    function traverse(tree) {
        if (tree != null) {
            traverse(tree.left);
            vals.push(tree.value);
            traverse(tree.right);
        }
    }
    traverse(tree);
    return vals;
}

function mkRouter(sSource, dests) {
    return sSource.withLatestFrom(dests, function (msg, dests) {
        return { send : lookup(dests, msg.key),
                 value : msg.value };
    }).subscribe(function (msg) {
        if (msg.send !== null)
            msg.send(msg.value);
    });
}

function addRoute(dests, key) {
    var send;
    var sOut = Rx.Observable.create(function (observer) {
        send = function(a) { observer.onNext(a); }
    }).publish();
    sOut.connect();
    return {
        dests : insert(dests, key, send),
        sOut : sOut
    };
}

function removeRoute(dests, key) {
    return remove(dests, key);
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
    var state = new Rx.BehaviorSubject({ moles : null, dests : null }),
        sAddMole = clock
            .filter(function (_) { return Math.random() < 0.02; })
            .withLatestFrom(state, clock,
                function (_, state, t0) {
                    var id = Math.floor(slots * Math.random()),
                        x = slotX(id), y = slotY(id);
                    var route = addRoute(state.dests, id);
                    var sClick = route.sOut;
                    console.log("add mole "+id);
                    var mole = mkMole(id, x, y, clock, sClick);
                    return { moles : insert(state.moles, id, mole),
                             dests : route.dests };
                }),
        sDestroy = state.flatMapLatest(
            function (state) {
                var sDestroy = Rx.Observable.of();
                var moles = valuesToList(state.moles);
                for (var i = 0; i < moles.length; i++)
                    sDestroy = sDestroy.merge(moles[i].sDestroy);
                return sDestroy;
            });
        sRemoveMole = sDestroy.withLatestFrom(state,
            function (id, state) {
                console.log("remove mole "+id);
                return { moles : remove(state.moles, id),
                         dests : removeRoute(state.dests, id) };
            });
    sAddMole.merge(sRemoveMole).subscribe(state);
    var drawables = new Rx.BehaviorSubject([]);
    state.flatMapLatest(
        function (state) {
            var drawables = [];
            var moles = valuesToList(state.moles);
            for (var i = 0; i < moles.length; i++)
                drawables.push(moles[i].drawable);
            return sequence(drawables);
        }).subscribe(drawables);
    var sClick = sMouseDown.map(function (pt) {
                return { key : slotAt(pt.x, pt.y),
                         value : true };
            }),
        dests = state.map(function (state) { return state.dests; });
    var dispose = mkRouter(sClick, dests);
    clock.subscribe(function(t) {
        var ctx = canvas.getContext("2d");
        ctx.fillStyle = '#00af00';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        var ds = drawables.getValue();
        for (var i = 0; i < ds.length; i++)
            ds[i](ctx, t);
    });
}

