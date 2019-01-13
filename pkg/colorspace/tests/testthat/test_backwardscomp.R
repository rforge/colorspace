

# Testing backward compatability of the diverge_hcl method.
test_that("testing diverge_hcl default call using different number of colors", {
    expect_identical(diverge_hcl(3), diverging_hcl(3))
    expect_identical(diverge_hcl(7), diverging_hcl(7))
})

test_that("testing diverge_hcl backwards compatability: setting hue", {
    expect_identical(diverge_hcl(5, h = c(10, 80)), diverging_hcl(5, h = c(10, 80)))
    expect_identical(diverge_hcl(5, h = c(10, 80)), diverging_hcl(5, h1 = 10, h2 = 80))
})

test_that("testing diverge_hcl backwards compatability: setting chroma", {
    expect_identical(diverge_hcl(5, c = 60), diverging_hcl(5, c = 60))
    expect_identical(diverge_hcl(5, c = 60), diverging_hcl(5, c1 = 60))
})

test_that("testing diverge_hcl backwards compatability: setting luminance", {
    expect_identical(diverge_hcl(5, l = 80), diverging_hcl(5, l = 80))
    expect_identical(diverge_hcl(5, l = c(30, 80)), diverging_hcl(5, l = c(30, 80)))
    expect_identical(diverge_hcl(5, l = c(30, 80)), diverging_hcl(5, l1 = 30, l2 = 80))
})

test_that("testing diverge_hcl backwards compatability: setting power", {
    expect_identical(diverge_hcl(5, power = 0.5), diverging_hcl(5, power = 0.5))
    expect_identical(diverge_hcl(5, power = 2.0), diverging_hcl(5, power = 2.0))
    expect_identical(diverge_hcl(5, power = 0.5), diverging_hcl(5, p1 = 0.5, p2 = 0.5))
    expect_identical(diverge_hcl(5, power = c(0.5, 2.0)), diverging_hcl(5, power = c(0.5, 2.0)))
    expect_identical(diverge_hcl(5, power = c(2.0, 0.5)), diverging_hcl(5, p1 = 2.0, p2 = 0.5))
})


# Testing backward compatability of the heat_hcl method
# heat_hcl defaults:
# - h = c(0, 90)
# - c. = c(100, 30)
# - l = c(50, 90)
# - power = c(1/5, 1)
test_that("testing heat_hcl default call using different number of colors", {
    expect_identical(heat_hcl(3),
                     sequential_hcl(3, h = c(0, 90), c = c(100, 30),
                                    l = c(50, 90), power = c(1/5, 1)))
    expect_identical(heat_hcl(5),
                     sequential_hcl(5, h = c(0, 90), c = c(100, 30),
                                    l = c(50, 90), power = c(1/5, 1)))
})

test_that("testing heat_hcl backwards compatability: setting hue", {
    expect_identical(heat_hcl(5, h = c(10, -50)),
                     sequential_hcl(5, h = c(10, -50), c = c(100, 30),
                                    l = c(50, 90), power = c(1/5, 1)))
    expect_identical(heat_hcl(5, h = c(10, -50)),
                     sequential_hcl(5, c = c(100, 30),
                                    l = c(50, 90), power = c(1/5, 1),
                                    h1 = 10, h2 = -50))
})

test_that("testing heat_hcl backwards compatability: setting chroma", {
    expect_identical(heat_hcl(5, c = c(80, 10)),
                     sequential_hcl(5, h = c(0, 90), c = c(80, 10),
                                    l = c(50, 90), power = c(1/5, 1)))
    expect_identical(heat_hcl(5, c = c(80, 10)),
                     sequential_hcl(5, h = c(0, 90),
                                    l = c(50, 90), power = c(1/5, 1),
                                    c1 = 80, c2 = 10))
})

test_that("testing heat_hcl backwards compatability: setting luminance", {
    expect_identical(heat_hcl(5, l = c(30, 80)),
                     sequential_hcl(5, h = c(0, 90), c = c(100, 30),
                                    l = c(30, 80), power = c(1/5, 1)))
    expect_identical(heat_hcl(5, l = c(30, 80)),
                     sequential_hcl(5, h = c(0, 90), c = c(100, 30),
                                    power = c(1/5, 1), l1 = 30, l2 = 80))
})

test_that("testing heat_hcl backwards compatability: setting power", {
    expect_identical(heat_hcl(5, power = 2),
                     sequential_hcl(5, h = c(0, 90), c = c(100, 30),
                                    l = c(50, 90), power = 2))
    expect_identical(heat_hcl(5, power = c(1,2)),
                     sequential_hcl(5, h = c(0, 90), c = c(100, 30),
                                    l = c(50, 90), power = c(1,2)))
    expect_identical(heat_hcl(5, power = c(1,2)),
                     sequential_hcl(5, h = c(0, 90), c = c(100, 30),
                                    l = c(50, 90), p1 = 1, p2 = 2))
    expect_identical(heat_hcl(5, power = c(2,1)),
                     sequential_hcl(5, h = c(0, 90), c = c(100, 30),
                                    l = c(50, 90), p1 = 2, p2 = 1))
})


# Testing rainbow_hcl
# Function defaults are:
# - h = c(0, 360) * (n - 1) / n 
# - c = 50
# - l = 70
test_that("testing rainbow_hcl default call using different number of colors", {
    expect_identical(rainbow_hcl(3),
                     qualitative_hcl(3, h = c(0, 360) * 2/3, c = 50, l = 70))
    expect_identical(rainbow_hcl(5),
                     qualitative_hcl(5, h = c(0, 360) * 4/5, c = 50, l = 70))
})

test_that("testing rainbow_hcl backwards compatability: setting hue", {
    expect_identical(rainbow_hcl(5, start = 100),
                     qualitative_hcl(5, h = c(100, 360 * 4/5), c = 50, l = 70))
    expect_identical(rainbow_hcl(5, end = 200),
                     qualitative_hcl(5, h = c(0, 200), c = 50, l = 70))
    expect_identical(rainbow_hcl(5, start = 100, end = 200),
                     qualitative_hcl(5, h = c(100, 200), c = 50, l = 70))
    expect_identical(rainbow_hcl(5, start = 100, end = 200),
                     qualitative_hcl(5, c = 50, l = 70, h1 = 100, h2 = 200))
})

test_that("testing rainbow_hcl backwards compatability: setting chroma", {
    expect_identical(rainbow_hcl(5, c = 40),
                     qualitative_hcl(5, h = c(0, 360) * 4/5, c = 40, l = 70))
    expect_identical(rainbow_hcl(5, c = 40),
                     qualitative_hcl(5, h = c(0, 360) * 4/5, l = 70, c1 = 40))
})

test_that("testing rainbow_hcl backwards compatability: setting luminance", {
    expect_identical(rainbow_hcl(5, l = 90),
                     qualitative_hcl(5, h = c(0, 360) * 4/5, c = 50, l = 90))
    expect_identical(rainbow_hcl(5, l = 90),
                     qualitative_hcl(5, h = c(0, 360) * 4/5, c = 50, l1 = 90))
})


# Testing terrain_hcl
# Function defaults are:
# - h = c(130, 0)
# - c. = c(80, 0)
# - l = c(60, 95)
# - power = c(1/10, 1)

test_that("testing terrain_hcl default call using different number of colors", {
    expect_identical(terrain_hcl(3),
                     sequential_hcl(3, h = c(130, 0), c = c(80, 0),
                                    l = c(60, 95), power = c(1/10, 1)))
    expect_identical(terrain_hcl(5),
                     sequential_hcl(5, h = c(130, 0), c = c(80, 0),
                                    l = c(60, 95), power = c(1/10, 1)))
})

test_that("testing terrain_hcl backwards compatability: setting hue", {
    expect_identical(terrain_hcl(3, h = c(120, 10)),
                     sequential_hcl(3, h = c(120, 10), c = c(80, 0),
                                    l = c(60, 95), power = c(1/10, 1)))
    expect_identical(terrain_hcl(3, h = c(120, 10)),
                     sequential_hcl(3, c = c(80, 0),
                                    l = c(60, 95), power = c(1/10, 1),
                                    h1 = 120, h2 = 10))
})

test_that("testing terrain_hcl backwards compatability: setting chroma", {
    expect_identical(terrain_hcl(5, c = c(70, 10)),
                     sequential_hcl(5, h = c(130, 0), c = c(70, 10),
                                    l = c(60, 95), power = c(1/10, 1)))
    expect_identical(terrain_hcl(5, c = c(70, 10)),
                     sequential_hcl(5, h = c(130, 0),
                                    l = c(60, 95), power = c(1/10, 1),
                                    c1 = 70, c2 = 10))
})

test_that("testing terrain_hcl backwards compatability: setting luminance", {
    expect_identical(terrain_hcl(5, l = c(50, 90)),
                     sequential_hcl(5, h = c(130, 0), c = c(80, 0),
                                    l = c(50, 90), power = c(1/10, 1)))
    expect_identical(terrain_hcl(5, l = c(50, 90)),
                     sequential_hcl(5, h = c(130, 0), c = c(80, 0),
                                    power = c(1/10, 1), l1 = 50, l2 = 90))
})


test_that("testing terrain_hcl backwards compatability: setting power", {
    expect_identical(terrain_hcl(5, power = 0.5),
                     sequential_hcl(5, h = c(130, 0), c = c(80, 0),
                                    l = c(60, 95), power= 0.5))
    expect_identical(terrain_hcl(5, power = 2),
                     sequential_hcl(5, h = c(130, 0), c = c(80, 0),
                                    l = c(60, 95), power = 2))
    expect_identical(terrain_hcl(5, power = 2),
                     sequential_hcl(5, h = c(130, 0), c = c(80, 0),
                                    l = c(60, 95), p1 = 2, p2 = 2))
    expect_identical(terrain_hcl(5, power = c(0.5, 2)),
                     sequential_hcl(5, h = c(130, 0), c = c(80, 0),
                                    l = c(60, 95), p1 = .5, p2 = 2))
})








