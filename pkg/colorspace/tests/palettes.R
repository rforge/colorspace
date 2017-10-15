library("colorspace")

## qualitative palette
rainbow_hcl(12)

## a few useful diverging HCL palettes
diverge_hcl(7)
diverge_hcl(7, h = c(246, 40), c = 96, l = c(65, 90))
diverge_hcl(7, h = c(130, 43), c = 100, l = c(70, 90))
diverge_hcl(7, h = c(180, 70), c = 70, l = c(90, 95))
diverge_hcl(7, h = c(180, 330), c = 59, l = c(75, 95))
diverge_hcl(7, h = c(128, 330), c = 98, l = c(65, 90))
diverge_hcl(7, h = c(255, 330), l = c(40, 90))
diverge_hcl(7, c = 100, l = c(50, 90), power = 1)

## sequential palettes
sequential_hcl(12)
heat_hcl(12, h = c(0, -100), l = c(75, 40), c = c(40, 80), power = 1)
terrain_hcl(12, c = c(65, 0), l = c(45, 95), power = c(1/3, 1.5))
heat_hcl(12, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5))
rainbow_hcl(12)
desaturate(rainbow_hcl(12))

## diverging red-blue colors
diverge_hsv(7)
diverge_hcl(7, c = 100, l = c(50, 90))
desaturate(diverge_hsv(7))
desaturate(diverge_hcl(7, c = 100, l = c(50, 90)))

## diverging cyan-magenta colors
diverge_hcl(7, h = c(180, 330), c = 59, l = c(75, 95))
desaturate(diverge_hcl(7, h = c(180, 330), c = 59, l = c(75, 95)))

## heat and terrain colors
heat_hcl(12)
desaturate(heat_hcl(12))
terrain_hcl(12)
desaturate(terrain_hcl(12))

## different interfaces
identical(sequential_hcl(7, "Grays"), sequential_hcl(7, palette = "Grays"))
identical(sequential_hcl(7, "Grays"), sequential_hcl(7, h = 0, c = 0, l = c(15, 95), power = 1.3))
identical(sequential_hcl(7, "Grays"), sequential_hcl(7, h = 0, c. = 0, l = c(15, 95), power = 1.3))
identical(sequential_hcl(7, "Grays"), sequential_hcl(7, c1 = 0, c2 = 0, l1 = 15, l2 = 95, p1 = 1.3))
