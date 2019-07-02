var pltDim = [0, 0];
$(document).on("shiny:filedownload", function(e){
  pltDim[0] = document.getElementById("pltmap_wrapper").clientWidth;
  pltDim[1] = document.getElementById("pltmap_wrapper").clientHeight;
  Shiny.onInputChange("pltDim", pltDim);
});
