updateCanvasSize = function() {
  
  document.getElementById('myCanvas').width  = document.getElementById('myCanvas').offsetWidth;
  document.getElementById('myCanvas').style.height='100%';
  document.getElementById('myCanvas').height = document.getElementById('myCanvas').offsetHeight;
  document.getElementById('topbar').width = document.getElementById('topbar').offsetWidth;
  document.getElementById('midbar').width = document.getElementById('midbar').offsetWidth;
  document.getElementById('botbar').width = document.getElementById('botbar').offsetWidth;

};

  
Shiny.addCustomMessageHandler('line-1-thick', function(size) {
  var canvas = document.getElementById('myCanvas');
  var ctx=canvas.getContext("2d");
  updateCanvasSize();
  var thickness = size;
 /*
  $('#myCanvas').drawLine({
  strokeStyle: '#000',
  strokeWidth: thickness,
  rounded: true,
  startArrow: true,
  arrowRadius: 15,
  arrowAngle: 90,
  x1: 500, y1: 20,
  x2: canvas.width, y2: 75,
  x3: 50, y3: 75
});
*/
});


