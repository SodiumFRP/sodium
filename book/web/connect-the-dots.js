function init() {
    var canvas = document.getElementById("myCanvas");
    var sMouseDown = Rx.Observable.fromEvent(canvas, 'mousedown');
    var initial = { x0 : 0, y0 : 0, x1 : 0, y1 : 0 };
    var sLines = sMouseDown.scan(initial, function(last, e) {
        var x = e.pageX - canvas.offsetLeft;
        var y = e.pageY - canvas.offsetTop;
        return { x0 : last.x1, y0 : last.y1,
                 x1 : x,       y1 : y };
    });
    var subscription = sLines.subscribe(function (l) {
        var ctx = canvas.getContext("2d");
        ctx.beginPath();
        ctx.moveTo(l.x0, l.y0);
        ctx.lineTo(l.x1, l.y1);
        console.log('{ x:'+l.x1+', y:'+l.y1+' },');
        ctx.stroke();
    });
}

