## ----echo=FALSE, message=FALSE-------------------------------------------
library(ggplot2)
library(colorspace)
theme_set(theme_minimal())

## ----fig.width = 6.5, fig.asp = 0.7, fig.align = 'center'----------------
ggplot(iris, aes(x = Sepal.Length, fill = Species)) + geom_density(alpha = 0.7) +
  scale_fill_discrete_qualitative()

## ----fig.width = 6.5, fig.asp = 0.7, fig.align = 'center'----------------
ggplot(iris, aes(x = Species, y = Sepal.Width, color = Sepal.Length)) + geom_jitter(width = 0.2)
# scale_color_continuous_sequential() not yet implemented

## ----fig.width = 6.5, fig.asp = 0.7, fig.align = 'center'----------------
ggplot(iris, aes(x = Sepal.Length, fill = Species)) + geom_density(alpha = 0.7) +
  scale_fill_discrete_qualitative(palette = "Harmonic")

## ----fig.width = 6.5, fig.asp = 0.7, fig.align = 'center'----------------
ggplot(iris, aes(x = Sepal.Length, fill = Species)) + geom_density(alpha = 0.7) +
  scale_fill_discrete_qualitative(palette = "Harmonic", nmax = 5, order = c(5, 1, 2))

## ----fig.width = 6.5, fig.asp = 0.7, fig.align = 'center'----------------
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
gg <- ggplot(dsamp, aes(carat, price, color = clarity)) + geom_point()
gg + scale_color_brewer(palette = "Blues")

## ----fig.width = 6.5, fig.asp = 0.7, fig.align = 'center'----------------
# scale_color_continuous_sequential() not yet implemented
# gg + scale_color_discrete_sequential(palette = "Blues", nmax = 9, order = 8:1)

## ----fig.width = 6.5, fig.asp = 0.7, fig.align = 'center'----------------
ggplot(iris, aes(x = Species, y = Sepal.Width, color = Sepal.Length)) + geom_jitter(width = 0.2)
#  scale_color_continuous_sequential(palette = "Terrain HCL")
# scale_color_continuous_sequential() not yet implemented

## ----fig.width = 6.5, fig.asp = 0.7, fig.align = 'center'----------------
ggplot(iris, aes(x = Species, y = Sepal.Width, color = Sepal.Length)) + geom_jitter(width = 0.2)
#  scale_color_continuous_sequential(palette = "Terrain HCL", H2 = 80)
# scale_color_continuous_sequential() not yet implemented

## ----fig.width = 6.5, fig.asp = 1., fig.align = 'center'-----------------
set.seed(100)
df <- data.frame(country = LETTERS, V = runif(26, -40, 40))
df$country = factor(LETTERS, LETTERS[order(df$V)]) # reorder factors
gg <- ggplot(df, aes(x = country, y = V, fill = V)) +
  geom_bar(stat = "identity") +
  labs(y = "Under/over valuation in %", x = "Country") +
  coord_flip()

#gg + scale_fill_continuous_diverging(palette = "Blue-Red 3")
# scale_fill_continuous_diverging() not yet implemented

## ----fig.width = 6.5, fig.asp = 1., fig.align = 'center'-----------------
#gg + scale_fill_continuous_diverging(palette = "Blue-Red 3", Lend = 20, Lmid = 90, power = 1)
# scale_fill_continuous_diverging() not yet implemented

## ----fig.width = 6.5, fig.asp = 0.7, fig.align = 'center'----------------
ggplot(iris, aes(x = Species, y = Sepal.Width, color = Sepal.Length)) + geom_jitter(width = 0.2)
#  scale_color_continuous_sequential(palette = "Viridis")

## ----fig.width = 6.5, fig.asp = 0.7, fig.align = 'center'----------------
ggplot(iris, aes(x = Species, y = Sepal.Width, color = Sepal.Length)) + geom_jitter(width = 0.2)
#  scale_color_continuous_sequential(palette = "Viridis", begin = 0.15, end = 0.9) 

## ----fig.width = 6.5, fig.asp = 0.7, fig.align = 'center'----------------
ggplot(iris, aes(x = Species, y = Sepal.Width, color = Sepal.Length)) + geom_jitter(width = 0.2)
#  scale_color_continuous_sequential(palette = "Viridis", direction = -1)

## ----echo = FALSE, message = FALSE, warning = FALSE, fig.align = 'left', fig.width = 8, fig.asp = .9----
hcl_palettes("qualitative", plot = TRUE)

## ----echo = FALSE, message = FALSE, warning = FALSE, fig.align = 'left', fig.width = 8, fig.asp = .81----
hcl_palettes("sequential (single-hue)", plot = TRUE)

## ----echo = FALSE, message = FALSE, warning = FALSE, fig.align = 'left', fig.width = 8, fig.asp = 1.17----
hcl_palettes("sequential (multi-hue)", plot = TRUE)

## ----echo = FALSE, message = FALSE, warning = FALSE, fig.align = 'left', fig.width = 8, fig.asp = .9----
hcl_palettes("diverging", plot = TRUE)

## ------------------------------------------------------------------------
cm <- cor(mtcars)
df_wide <- as.data.frame(cm)
df_long <- stack(df_wide)
names(df_long) <- c("cor", "var1")
df_long <- cbind(df_long, var2 = rep(rownames(cm), length(rownames(cm))))
ggplot(df_long, aes(var1, var2, fill=cor)) + 
  geom_tile() + 
  coord_fixed() +
  ylab("") +
  scale_x_discrete(position = "top", name = "") +
  scale_fill_gradient2()

## ------------------------------------------------------------------------
nx = 87
ny = 61
df <- data.frame(height = c(volcano), x = rep(1:nx, ny), y = rep(1:ny, each = nx))
ggplot(df, aes(x, y, fill=height)) + 
  geom_raster() + 
  coord_fixed(expand = FALSE)

