
function route(sAdd, sIn) {
    var sIngress = sIn.map(function (msg) {
            return { key : msg.key, type : 'in', value : msg.value };
        }).merge(
            sAdd.map(function (msg) {
                return { key : msg.key, type : 'add', extra : msg.extra };
            })
        );
    function creation(tree, sIngress) {
        return tree.map(function (tree) {
                if (tree !== null) {
                    var key = tree.key,
                        sOut = sIngress.map(function (msg) {
                            return msg.type == 'in' && msg.key == key
                                    ? msg.value
                                    : null;
                        }).filter(function (out) { return out !== null; });
                    return tree.updateType == 'create'
                        ? { key : tree.key, sOut : sOut,
                            extra : tree.updateExtra }
                        : null;
                }
                else
                    return null;
            }).filter(function (out) { return out !== null; });
    }
    var emptyTree = { alive : false };
    function createNode(key, sIngress, extra) {
        var sLeftIngress = sIngress.filter(function (msg) {
                    return msg.key < key;
                }).publish();
        sLeftIngress.connect();
        var sRightIngress = sIngress.filter(function (msg) {
                    return msg.key > key;
                }).publish();
        sRightIngress.connect();
        var left = new Rx.BehaviorSubject(emptyTree),
            right = new Rx.BehaviorSubject(emptyTree);
        sLeftIngress.withLatestFrom(left,
                function (msg, tree) { return processIngress(msg, tree, sLeftIngress); }
            ).filter(function (out) { return out !== null; })
             .subscribe(left);
        sRightIngress.withLatestFrom(right,
            function (msg, tree) { return processIngress(msg, tree, sRightIngress); }
            ).filter(function (out) { return out !== null; })
             .subscribe(right);
        var sLocalEgress = creation(left, sLeftIngress)
                    .merge(creation(right, sRightIngress));
        var sChildEgress = left.combineLatest(right, function (l, r) {
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
                 sEgress : sEgress, updateType : 'create', updateExtra : extra };
    }
    function processIngress(msg, tree, sIngress) {
        return msg.type == 'add' && !tree.alive
            ? createNode(msg.key, sIngress, msg.extra) : null;
    }
    var tree = new Rx.BehaviorSubject(emptyTree);
    sIngress.withLatestFrom(tree,
            function (msg, tree) { return processIngress(msg, tree, sIngress); }
        ).filter(function (out) { return out !== null; })
         .subscribe(tree);
    var sEgress = tree.flatMapLatest(function (tree) {
            return tree.alive ? tree.sEgress : Rx.Observable.of();
        }).merge(creation(tree, sIngress));
    return sEgress;
}

