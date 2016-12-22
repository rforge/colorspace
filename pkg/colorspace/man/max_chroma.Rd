\name{max_chroma}
\alias{max_chroma}
\alias{max_chroma_table}

\title{Compute Maximum Chroma for Given Hue and Luminance in HCL}
\description{
  Compute approximately the maximum chroma possible for a given
  hue and luminance combination in the HCL color space.
}
\usage{
max_chroma(h, l, floor = FALSE)
}
\arguments{
  \item{h}{hue value in the HCL color description, has to be
    in [0, 360].}
  \item{l}{luminance value in the HCL color description, has to
    be in [0, 100].}
  \item{floor}{logical. Should the chroma value be rounded down
    to the next lower integer?}
}
\details{
  As the possible combinations of chroma and luminance depend on
  hue, it is not obvious which maximum chroma can be used for a
  given combination of hue and luminance prior to calling
  \code{\link{polarLUV}}. To avoid having to \code{fixup} the
  color upon conversion to RGB \code{\link{hex}} codes, the
  \code{max_chroma} function computes (approximately) the maximum
  chroma possible. As the computation are based on interpolations
  of pre-computed maxima, the result may sometimes still be
  very slightly larger than the actual maximum.
}

\value{
  A numeric vector with the maximum chroma coordinates.
}

\seealso{
  \code{\link[colorspace]{polarLUV}},
  \code{\link[colorspace]{hex}}
}

\examples{
max_chroma(0:36 * 10, 50)
max_chroma(120, 0:10 * 10)
}
\keyword{color}
