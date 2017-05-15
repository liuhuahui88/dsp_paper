plot_hill <- function(mode) {
  jpeg("Hill.jpg", quality=100);

  plot(hill(1.5), xlim=c(0,3), ylim=c(0,1.5), xlab='x', ylab='p(x)', xaxt='n', yaxt='n');
  xx = seq(0, 3, 1);
  yy = seq(0, 1.5, 0.5);
  axis(1, at=xx);
  axis(2, at=yy);
  abline(v=xx, h=yy, lty=2, col="darkred");
  text(1.5, 1.0, "(summit, 1)", pos=3);

  dev.off();
}

hill <- function(mode) {
  d = function(v) {
    sapply(v, function(x) {
    if (x <= 0) {
      return(0);
    } else if (x <= mode) {
      return(x / mode);
    } else if (x <= 2) {
      return((2 - x) / (2 - mode));
    } else {
      return(0);
    }
  })};
  return(d);
}
