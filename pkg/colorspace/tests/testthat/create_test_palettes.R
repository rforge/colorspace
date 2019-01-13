

library("colorspace")


# "res" is the final object which will be written into an R test script.
# List of strings which define the full test (using thestthis).
res <- c()

# Helper function to append string to "res" object.
append <- function(x) res <<- c(res, x)

# Output file
outfile <- "test_palettes.R"

# Number of colors to be tested
N    <- 5

# -------------------------------------------------------------------
# Loading the palettes
# -------------------------------------------------------------------
pals  <- as.data.frame(hcl_palettes())
xpals <- as.data.frame(divergingx_palettes())


# -------------------------------------------------------------------
# Qualitative schemes
# -------------------------------------------------------------------
append("# Testing qualitative color schemes")
append("test_that(\"qualitative hcl palettes\", {")
for ( idx in grep("^Qualitative$", pals$type) ) {
    cols <- qualitative_hcl(N, palette = rownames(pals)[idx])
    cmd <- sprintf("    expect_equal(qualitative_hcl(%d, \"%s\"), c(%s))",
                   N, rownames(pals)[idx], paste(sprintf("\"%s\"", cols), collapse = ", "))
    append(cmd)
}
append("})\n\n")


# -------------------------------------------------------------------
# Sequential schemes
# -------------------------------------------------------------------
append("# Testing sequential color schemes")
append("test_that(\"sequential hcl palettes\", {")
for ( idx in grep("^Sequential", pals$type) ) {
    cols <- sequential_hcl(N, palette = rownames(pals)[idx])
    cmd <- sprintf("    expect_equal(sequential_hcl(%d, \"%s\"), c(%s))",
                   N, rownames(pals)[idx], paste(sprintf("\"%s\"", cols), collapse = ", "))
    append(cmd)
}
append("})\n\n")


# -------------------------------------------------------------------
# Diverging schemes
# -------------------------------------------------------------------
append("# Testing diverging color schemes")
append("test_that(\"diverging hcl palettes\", {")
for ( idx in grep("^Diverging$", pals$type) ) {
    cols <- diverging_hcl(N, palette = rownames(pals)[idx])
    cmd <- sprintf("    expect_equal(diverging_hcl(%d, \"%s\"), c(%s))",
                   N, rownames(pals)[idx], paste(sprintf("\"%s\"", cols), collapse = ", "))
    append(cmd)
}
append("})\n\n")


# -------------------------------------------------------------------
# Testing DivergingX palettes
# -------------------------------------------------------------------
append("# Testing divergingx color schemes")
append("test_that(\"divergingx hcl palettes\", {")
for ( idx in grep("^Diverging\\s\\(flexible\\)$", xpals$type) ) {
    cols <- divergingx_hcl(N, palette = rownames(xpals)[idx])
    cmd <- sprintf("    expect_equal(divergingx_hcl(%d, \"%s\"), c(%s))",
                   N, rownames(xpals)[idx], paste(sprintf("\"%s\"", cols), collapse = ", "))
    append(cmd)
}
append("})\n\n")


# Write test file.
write(res, file = outfile, append = FALSE)



