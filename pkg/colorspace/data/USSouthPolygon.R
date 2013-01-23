library("maps")
USSouthPolygon <- USSouthPolygon0 <- map("county", c("alabama", "georgia", "south carolina"), fill = TRUE, col = "lightgray")

USSouthPolygon <- split(as.data.frame(USSouthPolygon[1:2]),
  cut(seq_along(USSouthPolygon$x), breaks = c(0, which(is.na(USSouthPolygon$x)) - 1, Inf)))
names(USSouthPolygon) <- NULL
USSouthPolygon[[1]] <- rbind(NA, USSouthPolygon[[1]])

set.seed(1071)
for(i in seq_along(USSouthPolygon)) {
  ni <- nrow(USSouthPolygon[[i]]) - 1
  xi <- mean(USSouthPolygon[[i]]$x, na.rm = TRUE)
  yi <- mean(USSouthPolygon[[i]]$y, na.rm = TRUE)
  d <- sqrt((xi + 84)^2 + (yi - 32.6)^2)
  d2 <- pnorm(d, 1.8)^0.6
  d2 <- if((xi > -81.5 & yi > 33.4) | xi < -87.8) {
    runif(1, 0, 1)
  } else {
    pmax(0, pmin(d2^(1 + (d2 <= 0.25)) + d2 * runif(1, -0.15, 0.1), 1))
  }
  USSouthPolygon[[i]]$z <- c(d2, rep(NA, ni))
  # USSouthPolygon[[i]]$z0 <- c(d, rep(NA, ni))
  rownames(USSouthPolygon[[i]]) <- c(USSouthPolygon0$names[i], paste(USSouthPolygon0$names[i], 1:ni, sep = ","))
}
USSouthPolygon <- do.call("rbind", USSouthPolygon)
save(USSouthPolygon, file = "USSouthPolygon.rda")
