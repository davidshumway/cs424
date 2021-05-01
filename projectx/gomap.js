// Non-functional at present
$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var lat = $el.data("lat");
  var lon = $el.data("lon");
  var side = $el.data("side");
  $($("#nav a")[0]).tab("show");
  Shiny.onInputChange("goto", {
    lat: lat,
    lon: lon,
    side: side,
    nonce: Math.random()
  });
});
