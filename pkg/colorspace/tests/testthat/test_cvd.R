# -------------------------------------------------------------------
# Testing CVD simulate functions plus the desaturation function
# -------------------------------------------------------------------

# Default severity
test_that("protan, deutan, tritan, desaturate (default severity)", {
    # Default severity
    colors <- c(NA, NA, substr(rainbow(3), 0, 7))
    expect_equal(deutan(colors), c(NA, NA, "#5D4700", "#DBAB0A", "#000CF7"))
    expect_equal(protan(colors), c(NA, NA, "#261D00", "#FFC800", "#0019FF"))
    expect_equal(tritan(colors), c(NA, NA, "#FF0001", "#00EDB0", "#00254D"))
    expect_equal(desaturate(colors), c(NA, NA, "#7F7F7F", "#DCDCDC", "#4C4C4C"))
})

# Severity of 0.5
test_that("protan, deutan, tritan, desaturate (severity = 0.5)", {
    # Severity of 0.5
    colors <- c(NA, NA, substr(rainbow(3), 0, 7))
    expect_equal(deutan(colors, 0.5), c(NA, NA, "#8B2E00", "#9AC706", "#0009FA"))
    expect_equal(protan(colors, 0.5), c(NA, NA, "#741700", "#ADD700", "#000FFF"))
    expect_equal(tritan(colors, 0.5), c(NA, NA, "#FF0001", "#06F43F", "#000CBD"))
    expect_equal(desaturate(colors, 0.5), c(NA, NA, "#D05959", "#9AF09A", "#424296"))
})

# Severity = 0 should not modify the colors at all
test_that("protan, deutan, tritan, desaturate (severity = 0)", {
    # Severity of 0.5
    colors <- c(NA, NA, substr(rainbow(3), 0, 7))
    expect_equal(deutan(colors, 0), colors)
    expect_equal(protan(colors, 0), colors)
    expect_equal(tritan(colors, 0), colors)
    expect_equal(desaturate(colors, 0), colors)
})

# Matrix conversion (including NA values)
test_that("protan, deutan, tritan, desaturate (default severity)", {
    # Default severity
    colors <- cbind(t(hex2RGB(rainbow(3))@coords*255), matrix(NA, ncol = 2, nrow = 3))
    fun <- function(x)
        round(c(inherits(x, "matrix"), sum(x, na.rm = TRUE), sum(is.na(x))))
    expect_equal(fun(deutan(colors)), c(1, 825, 6))
    expect_equal(fun(protan(colors)), c(1, 804, 6))
    expect_equal(fun(tritan(colors)), c(1, 784, 6))
    expect_equal(fun(desaturate(colors)), c(1, 1269, 6))
})





