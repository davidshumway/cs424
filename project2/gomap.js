// Non-functional at present
$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var lat = $el.data("lat");
  var long = $el.data("long");
  //~ var zip = $el.data("zip");
  $($("#nav a")[0]).tab("show");
  Shiny.onInputChange("goto", {
    lat: latitude,
    lng: longitude,
    nonce: Math.random()
  });
});
