function formula(x, x1, x2, y, y1, y2) {
  return ( ( y1 < y  &&  y2 >= y ) || ( y2 < y  &&  y1 >= y ) ) && ( x1 + ( y - y1 ) / ( y2 - y1 ) * ( x2 - x1 ) < x );
}

function insidePolygon(pos, poly) {
  var coords = poly.coords;
  var x = pos.x;
  var y = pos.y;
  return coords.reduce((isInside, currentPos, index) => {
    var xy1 = coords[index - 1] || coords[coords.length - 1];
    var y1 = xy1.y
    var x1 = xy1.x;
    var y2 = currentPos.y;
    var x2 = currentPos.x;
    return formula(x, x1, x2, y, y1, y2) ? !isInside : isInside;
  }, false);
}
var shapes = [
    { id: "cat", coords: [{ x:55, y:90 },{x:67,y:54},{x:72,y:89},
           {x:99,y:88},{x:106,y:54},{x:115,y:91},{x:123,y:106},
           {x:100,y:134},{x:88,y:130},{x:80,y:134},{x:48,y:108}]},
    { id: "dog", coords: [{x:171,y:58},{x:154,y:80},{x:156,y:120},
           {x:166,y:110},{x:166,y:82},{x:183,y:130},{x:202,y:127},
           {x:221,y:78},{x:225,y:111},{x:237,y:119},{x:231,y:59},
           {x:211,y:66},{x:195,y:60},{x:180,y:72}]}
]

function init() {
    var canvas = document.getElementById("myCanvas");
    var getXY = function(e) { return { x : e.pageX - canvas.offsetLeft,
                                       y : e.pageY - canvas.offsetTop }; };
    var sMouseDown = Rx.Observable.fromEvent(canvas, 'mousedown')
                                  .map(getXY);
    var sSelected = sMouseDown.map(function(pos) {
          for (var i = 0; i < shapes.length; i++)
             if (insidePolygon(pos, shapes[i]))
                 return shapes[i].id;
          return null;
        });
    var selected = new Rx.BehaviorSubject("cat");
    sSelected.subscribe(selected);
    var okButton = document.getElementById('ok');
    var sOK = Rx.Observable.fromEvent(okButton, 'click');
    sOK.withLatestFrom(selected, function(ok, sel) { return sel; })
       .subscribe(function(sel) {
           alert('You selected '+sel);
       });
    selected.subscribe(function(selected) {
        var ctx = canvas.getContext("2d");
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        for (var i = 0; i < shapes.length; i++) {
            var coords = shapes[i].coords;
            ctx.beginPath();
            ctx.moveTo(coords[0].x, coords[0].y);
            for (var j = 0; j < coords.length; j++)
                ctx.lineTo(coords[j].x, coords[j].y);
            ctx.closePath();
            if (selected == shapes[i].id) {
                ctx.fillStyle = '#ff0000';
                ctx.fill();
            }
            ctx.stroke();
        }
    });
}

