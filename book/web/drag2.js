function insidePolygon(pos, poly) {
  var x = pos.x, y = pos.y, coords = poly.coords, inside = false;
  var v = coords[coords.length-1], x1 = v.x, y1 = v.y;
  for( var i = -1;  v = coords[++i]; ) {
    var x2 = v.x, y2 = v.y;
    if( ( y1 < y  &&  y2 >= y ) || ( y2 < y  &&  y1 >= y ) )
      if ( x1 + ( y - y1 ) / ( y2 - y1 ) * ( x2 - x1 ) < x )
        inside = ! inside;
    x1 = x2, y1 = y2;
  }
  return inside;
}
function find(doc, pos) {
  for (var i = 0; i < doc.length; i++)
    if (insidePolygon(pos, doc[i])) return doc[i];
  return null;
}
function insert(doc, shape) {
  doc = doc.slice();
  for (var i = 0; i < doc.length; i++)
    if (doc[i].id == shape.id) doc[i] = shape;
  return doc;
}
function shiftBy(shape, dx, dy) {
  var neu = { id: shape.id, coords : [] };
  for (var i = 0; i < shape.coords.length; i++) {
    var pt = shape.coords[i];
    neu.coords.push( { x : pt.x + dx, y : pt.y + dy } );
  }
  return neu;
}

function init() {
    var canvas = document.getElementById("myCanvas");
    var getXY = function(e) { return { x : e.pageX - canvas.offsetLeft,
                                       y : e.pageY - canvas.offsetTop }; };
    var sMouseDown = Rx.Observable.fromEvent(canvas, 'mousedown').map(getXY);
    var sMouseMove = Rx.Observable.fromEvent(canvas, 'mousemove').map(getXY);
    var sMouseUp = Rx.Observable.fromEvent(canvas, 'mouseup').map(getXY);
    var doc = new Rx.BehaviorSubject([
        { id: "cat", coords: [{ x:55, y:90 },{x:67,y:54},{x:72,y:89},
               {x:99,y:88},{x:106,y:54},{x:115,y:91},{x:123,y:106},
               {x:100,y:134},{x:88,y:130},{x:80,y:134},{x:48,y:108}]},
        { id: "dog", coords: [{x:171,y:58},{x:154,y:80},{x:156,y:120},
               {x:166,y:110},{x:166,y:82},{x:183,y:130},{x:202,y:127},
               {x:221,y:78},{x:225,y:111},{x:237,y:119},{x:231,y:59},
               {x:211,y:66},{x:195,y:60},{x:180,y:72}]}
      ]);
    sMouseDown.withLatestFrom(doc, function(pos, doc) {
          return { startPos : pos, shape : find(doc, pos) };
      }).filter(function(x) { return x.shape !== null; })
        .flatMapLatest(function(x) {
            var startPos = x.startPos;
            var shape = x.shape;
            return sMouseMove.withLatestFrom(doc, function(pos, doc) {
                var dx = pos.x - startPos.x;
                var dy = pos.y - startPos.y;
                return insert(doc, shiftBy(shape, dx, dy));
            }).takeUntil(sMouseUp);
        }).subscribe(doc);
    doc.subscribe(function(doc) {
        var ctx=canvas.getContext("2d");
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        for (var i = 0; i < doc.length; i++) {
            var coords = doc[i].coords;
            ctx.beginPath();
            ctx.moveTo(coords[0].x, coords[0].y);
            for (var j = 0; j < coords.length; j++)
                ctx.lineTo(coords[j].x, coords[j].y);
            ctx.closePath();
            ctx.fillStyle = '#4090ff';
            ctx.fill();
        }
    });
}

