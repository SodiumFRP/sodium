function addressing(sAdd, sRemove, sIn) {
    var emptyTree = { alive : false },
        notNull = function (x) { return x !== null; },
        tree = new Rx.BehaviorSubject(emptyTree);

    var sEgress0 = new Rx.Subject(),
        sLooped = sEgress0.map(function (msg) {
                return msg.type == 'loop' ? msg.msg : null;
            }).filter(notNull),
        sIngress = sIn.map(function (msg) {
                return { type : 'in', key : msg.key, value : msg.value };
            }).merge(
                sAdd.map(function (msg) {
                    return { type : 'add', key : msg.key, extra : msg.extra };
                })
            ).merge(
                sRemove.map(function (key) {
                    return { type : 'remove1', key : key, sPortal : Rx.Observable.of(), stack : null };
                })
            ).merge(
                sLooped
            );

    function route(sIngress, tree) {
        var sLeftIngress = new Rx.Subject(),
            sRightIngress = new Rx.Subject(),
            sPortal = new Rx.Subject(),
            sAll = sIngress.withLatestFrom(tree,
                function (msg, t) {
                    var tree = null, egress = null, portal = null,
                        left = null, right = null;
                    if (msg.type == 'maximum') {
                        if (t.alive)
                            right = { type : msg.type, key : msg.key,
                                      parent : t.key, sPortal : msg.sPortal,
                                      stack : msg.stack };
                        else
                        if (msg.parent != null) {
                            egress = { type : 'loop',
                                       msg : { type : 'remove1',
                                               key : msg.parent,
                                               sPortal : msg.sPortal,
                                               stack : { type : 'paste',
                                                         key : msg.key,
                                                         new_key : msg.parent,
                                                         stack : msg.stack } } };
                        }
                        else
                            egress = { type : 'loop',
                                       msg : { type : 'remove2',
                                               key : msg.key,
                                               stack : msg.stack } };
                    }
                    else
                    if (msg.type == 'minimum') {
                        if (t.alive)
                            left = { type : msg.type, key : msg.key,
                                     parent : t.key, sPortal : msg.sPortal,
                                     stack : msg.stack };
                        else
                        if (msg.parent != null) {
                            egress = { type : 'loop',
                                       msg : { type : 'remove1',
                                               key : msg.parent,
                                               sPortal : msg.sPortal,
                                               stack : { type : 'paste',
                                                         key : msg.key,
                                                         new_key : msg.parent,
                                                         stack : msg.stack } } };
                        }
                        else
                            egress = { type : 'loop',
                                       msg : { type : 'remove3',
                                               key : msg.key,
                                               stack : msg.stack } };
                    }
                    else
                    if (t.alive) {
                        if (msg.key < t.key)
                            left = msg;
                        else
                        if (msg.key > t.key)
                            right = msg;
                        else {
                            if (msg.type == 'in')
                                portal = { type : 'value', value : msg.value };
                            else
                            if (msg.type == 'remove1') {
                                portal = { type : 'redirect', key : msg.key,
                                           sPortal : msg.sPortal };
                                left = { type : 'maximum', key : msg.key,
                                         parent : null, sPortal : t.sPortal,
                                         stack : msg.stack };
                            }
                            else
                            if (msg.type == 'remove2') {
                                right = { type : 'minimum', key : msg.key,
                                          parent : null, sPortal : t.sPortal,
                                          stack : msg.stack };
                            }
                            else
                            if (msg.type == 'remove3') {
                                tree = { alive : false }
                                if (msg.stack !== null)
                                    egress = { type : 'loop', msg : msg.stack };
                            }
                            else
                            if (msg.type == 'paste') {
                                tree = { alive : true,
                                         key : msg.new_key, left : t.left, right : t.right,
                                         sPortal : t.sPortal,
                                         sEgress : t.sEgress };
                                if (msg.stack !== null)
                                    egress = { type : 'loop', msg : msg.stack };
                            }
                        }
                    }
                    else {
                        if (msg.type == 'add') {
                            tree = addNode(sLeftIngress, sRightIngress, sPortal, tree, msg.key, msg.extra);
                            var addressee = new Rx.BehaviorSubject(sPortal);
                                sAddressee = addressee.flatMapLatest(function (sPortal) { return sPortal; }),
                                sAddresseeValue = sAddressee.map(function (msg) {
                                        return msg.type == 'value' ? msg.value : null;
                                    }).filter(notNull);
                            var key = msg.key;
                            sAddressee.map(function (msg) {
                                return msg.type == 'redirect' && msg.key == key ? msg.sPortal : null;
                            }).filter(notNull).subscribe(addressee);
                            egress = { type : 'created', key : msg.key,
                                       sAddressee : sAddresseeValue, extra : msg.extra };
                        }
                    }
                    return { tree : tree, egress : egress, portal : portal,
                             left : left, right : right };
                }
            ).publish();
        sAll.connect();
        sAll.map(function (p) { return p.left; })
            .filter(notNull).subscribe(sLeftIngress);
        sAll.map(function (p) { return p.right; })
            .filter(notNull).subscribe(sRightIngress);
        sAll.map(function (p) { return p.portal; })
            .filter(notNull).subscribe(sPortal);
        return { sTree   : sAll.map(function(p) { return p.tree; })
                                .filter(notNull),
                 sEgress : sAll.map(function(p) { return p.egress; })
                                .filter(notNull) }
    }

    function addNode(sLeftIngress, sRightIngress, sPortal, tree, key, extra) {
        var left = new Rx.BehaviorSubject(emptyTree),
            right = new Rx.BehaviorSubject(emptyTree),
            ul = route(sLeftIngress, left),
            ur = route(sRightIngress, right);
        ul.sTree.subscribe(left);
        ur.sTree.subscribe(right);
        var sLocalEgress = ul.sEgress.merge(ur.sEgress);
            sChildEgress = left.combineLatest(right, function (l, r) {
                    return l.alive ?
                               r.alive ? l.sEgress.merge(r.sEgress)
                                       : l.sEgress :
                               r.alive ? r.sEgress
                                       : Rx.Observable.of();
                }).flatMapLatest(
                    function (s) { return s; }
                );
        var sEgress = sLocalEgress.merge(sChildEgress);
        return { alive : true,
                 key : key, left : left, right : right,
                 sPortal : sPortal,
                 sEgress : sEgress };
    }

    var u = route(sIngress, tree);
    u.sTree.subscribe(tree);
    var sLocalEgress = u.sEgress,
        sChildEgress = tree.flatMapLatest(function (tree) {
            return tree.alive ? tree.sEgress : Rx.Observable.of();
        }),
        sEgress = sLocalEgress.merge(sChildEgress),
        sCreated = sEgress.filter(function (e) {
                return e.type == 'created';
            });
    sEgress.subscribe(sEgress0);
    return sCreated;
}

