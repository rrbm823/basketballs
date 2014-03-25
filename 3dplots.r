par(mfrow = c(1, 1))
panelfirst <- function(pmat) {
  zmin <- min(upsets$pred)
  XY <- trans3D(upsets$rpi_a, upsets$rpi_b,
                z = rep(zmin, nrow(upsets)), pmat = pmat)
  scatter2D(XY$x, XY$y, colvar = upsets$actual_result,
            cex = 1, add = TRUE, colkey = FALSE)
  xmin <- min(upsets$rpi_a)
  XY <- trans3D(x = rep(xmin, nrow(upsets)), y = upsets$rpi_b,
                z = upsets$pred, pmat = pmat)
  scatter2D(XY$x, XY$y, colvar = upsets$actual_result,
            cex = 1, add = TRUE, colkey = FALSE)
}
with(upsets, scatter3D(x = rpi_a, y = rpi_b, z = pred, colvar = actual_result,
                       pch = 16, cex = 1.5, xlab = "rpi_a", ylab = "rpi_b",
                       main = "upsets", ticktype = "detailed",
                       panel.first = panelfirst, theta = 80, phi = 80, d = 2,
                       colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75)
                       ))