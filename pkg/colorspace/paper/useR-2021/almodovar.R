library("colorspace")

## Tacones Lejanos (High Heels, 1991)
## Idea: Bibiana Fernandez (https://www.pinterest.at/pin/396387204673777781/)
## Credit: https://www.alamy.com/stock-photo-pedro-almodovar-victoria-abril-miriam-diaz-aroca-marisa-paredes-high-31039349.html
## Images:
## https://i.pinimg.com/originals/6a/28/4b/6a284b533ac109f9933d2e1dcf63a5d2.jpg
## https://c7.alamy.com/comp/BPDY05/pedro-almodovar-victoria-abril-miriam-diaz-aroca-marisa-paredes-high-BPDY05.jpg
## https://twitter.com/juanferrervila/status/1044483192361807872
tacones0 <- paste0("#", c("334ea9", "5e9aa4", "fceb5f", "423783", "931c14", "b16594"))
tacones  <- paste0("#", c("1e51bc", "61abb6", "ba91bc", "fff353", "42389b", "ab200c"))
specplot(tacones0[c(1:2, 6, 3:5)], tacones)

## Todo Sobre Mi Madre (All About My Mother, 1999)
## Idea: Hadley Mendelsohn (https://www.housebeautiful.com/design-inspiration/a33012615/most-beautiful-movie-color-palettes/)
## Credit: Sony Pictures Classics via MoMA (https://www.moma.org/calendar/events/2544)
## Images:
## https://www.moma.org/d/assets/W1siZiIsIjIwMTYvMTAvMjYvNnNncnR5YmYwX0FsbF9BYm91dF9NeV9Nb3RoZXJfMV9jcm9wcGVkLmpwZyJdLFsicCIsImNvbnZlcnQiLCItcXVhbGl0eSA5MCAtcmVzaXplIDIwMDB4MjAwMFx1MDAzZSJdXQ/All%20About%20My%20Mother_1_cropped.jpg?sha=f46b56461ca7050e
## https://hips.hearstapps.com/hmg-prod.s3.amazonaws.com/images/todo-sobre-mi-madre-2-1594842994.png
madre0 <- paste0("#", c("f6e691", "d5c6b3", "cf9788", "d31509", "9d060d", "660003"))
madre  <- paste0("#", c("fce08c", "cecbb8", "d7977c", "e8240c", "a11613", "5a0604"))
madre2 <- sequential_hcl(6, h1 = 70, h2 = 5, c1 = 20, c2 = 64, cmax = 143, l1 = 91, l2 = 19, p1 = 0.9)
specplot(madre0, madre)
specplot(rev(madre), rev(madre2))


## Atame!
## https://m.media-amazon.com/images/M/MV5BMjExMDM0OTItY2E1YS00N2Y4LWFlNGMtOTgyNmFlMjIyOTgxXkEyXkFqcGdeQXVyMTA0MjU0Ng@@._V1_FMjpg_UX1000_.jpg
atame <- paste0("#", c("2966b6", "f93f90", "edef50", "fc3919"))
swatchplot(atame, cvd = TRUE)

## Mujeres Al Borde De Un Ataque De Nervios
## https://m.media-amazon.com/images/M/MV5BYWM1NWFjMDItODg5OS00MWUwLWFjNWUtOGZkZWM3NmRiMWNjXkEyXkFqcGdeQXVyMTA0MjU0Ng@@._V1_FMjpg_UX1000_.jpg
## colorfindr: https://github.com/zumbov2/colorfindr/ https://CRAN.R-project.org/package=colorfindr
library("colorfindr")
mujeres0 <- get_colors("https://m.media-amazon.com/images/M/MV5BYWM1NWFjMDItODg5OS00MWUwLWFjNWUtOGZkZWM3NmRiMWNjXkEyXkFqcGdeQXVyMTA0MjU0Ng@@._V1_FMjpg_UX1000_.jpg") |>
  make_palette(n = 6)
## https://64.media.tumblr.com/tumblr_lmp9jq9Me21qgd3wro1_640.jpg
mujeres <- paste0("#", c("f4afa9", "99d4f5", "edd183", "e1652a", "d10d34"))

myswatch <- function(p) {
  n <- length(p)
  op <- par(mar = rep(0, 4), oma = rep(0, 4))
  on.exit(par(op))
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  rect(0:(n - 1)/n, 0, 1:n/n, 1, col = p, border = "transparent")
}
