function imagePromise(url)
{
    var sLoaded = Rx.Observable.create(function (observer) {
        var img = new Image();
        img.onload = function() { observer.onNext(img); };
        img.src = url;
    }).publish();
    sLoaded.connect();
    var image = new Rx.BehaviorSubject(null),
        subscr1 = sLoaded.subscribe(image);
    return { image : image,
             dispose : function() { subscr1.dispose(); } };
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

function dragging(canvas) {
    var getXY = function(e) {
            return { x : e.pageX - canvas.offsetLeft,
                     y : e.pageY - canvas.offsetTop }; },
        sMouseDown = Rx.Observable.fromEvent(canvas, 'mousedown')
                                 .map(getXY),
        sMouseMove = Rx.Observable.fromEvent(canvas, 'mousemove')
                                 .map(getXY),
        sMouseUp = Rx.Observable.fromEvent(canvas, 'mouseup')
                                 .map(getXY),
        scrollOrigin = new Rx.BehaviorSubject({ x : 300, y : 2100 }),
        dragOrigin = new Rx.BehaviorSubject(null);
    sMouseDown.withLatestFrom(scrollOrigin, function (pt, so) {
            return { mouse : pt, scroll : so };
        }).merge(sMouseUp.map(null)).subscribe(dragOrigin);
    sMouseMove.withLatestFrom(dragOrigin, function (pt, dorig) {
            return dorig === null ? null
                  : { x : dorig.scroll.x - pt.x + dorig.mouse.x,
                      y : dorig.scroll.y - pt.y + dorig.mouse.y }; 
        }).filter(function (so) { return so !== null; })
          .subscribe(scrollOrigin);
    return scrollOrigin;
}

var baseURL = 'http://reactiveprogramming.org/~blackh/frp-map/',
    xTiles = 44, yTiles = 34,
    tileWidth = 200, tileHeight = 200,
    noOfTiles = xTiles * yTiles;
function tileX(tile) { return (tile % xTiles) * tileWidth; }
function tileY(tile) { return Math.floor(tile / xTiles) * tileHeight; } 

function init() {
    var canvas = document.getElementById("myCanvas"),
        scrollOrigin = dragging(canvas),
        sTilesNeeded = scrollOrigin.map(function (so) {
            var tiles = [],
                x0 = Math.floor(so.x / tileWidth),
                y0 = Math.floor(so.y / tileHeight),
                wid = canvas.width,
                ht = canvas.height;
            for (var x = x0; ((x) * tileWidth - so.x <= wid); x++)
                for (var y = y0; ((y) * tileHeight - so.y <= ht); y++) {
                    var tile = x + y * xTiles;
                    if (tile >= 0 && tile < noOfTiles)
                        tiles.push(tile);
                }
            return tiles;
        }),
        tilePromises = new Rx.BehaviorSubject([]);
    sTilesNeeded.withLatestFrom(tilePromises,
        function (needed, promises) {
            var newPromises = [],
                promises = promises.slice();
            for (var i = 0; i < needed.length; i++) {
                var tile = needed[i];
                var found = false;
                for (var j = 0; j < promises.length; j++) {
                    if (promises[j].tile == tile) {
                        newPromises.push(promises.splice(j, 1)[0]);
                        found = true;
                        break;
                    }
                }
                if (!found)
                    newPromises.push({
                        tile : tile,
                        promise : imagePromise(
                            baseURL+"tile_"+tile+".png")
                    });
            }
            for (var j = 0; j < promises.length; j++)
                setTimeout(promises[j].promise.dispose, 0);
            return newPromises;
        }).subscribe(tilePromises);
    var scene = tilePromises.flatMapLatest(function (promises) {
        var outImages = [];
        for (var i = 0; i < promises.length; i++) {
            outImages.push(function (tile, image) {
                    return image.map(
                        function (img) {
                            return { tile : tile, image : img };
                        });
                } (promises[i].tile, promises[i].promise.image));
        }
        return sequence(outImages);
    });
    var sTileLoaded = tilePromises.flatMapLatest(function (promises) {
        var sLoaded = Rx.Observable.of();
        for (var i = 0; i < promises.length; i++)
            sLoaded = sLoaded.merge(
                promises[i].promise.image.filter(function (img) {
                    return img !== null; }));
        return sLoaded;
    });

    function draw(toDraw) {
        var so = toDraw.so;
        var scene = toDraw.scene;
        var ctx = canvas.getContext("2d");
        ctx.fillStyle = '#dfafef';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        for (var i = 0; i < scene.length; i++)
            if (scene[i].image !== null) {
                var tile = scene[i].tile,
                    x = tileX(tile)-so.x,
                    y = tileY(tile)-so.y;
                ctx.drawImage(scene[i].image, x, y);
                ctx.beginPath();
                ctx.rect(x, y, tileWidth, tileHeight);
                ctx.stroke();
            }
    }

    sTileLoaded.withLatestFrom(scrollOrigin, scene,
        function (_, so, scene) {
            return { so : so, scene : scene };
        }
    ).subscribe(draw);
    scrollOrigin.withLatestFrom(scene,
        function (so, scene) {
            return { so : so, scene : scene };
        }
    ).subscribe(draw);
}

