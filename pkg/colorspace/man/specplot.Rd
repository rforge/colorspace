\name{specplot}
\alias{specplot}

\title{Color Dimension Spectrum Plot}

\description{
   Decomposition of hexadecimal (HEX) color palettes
   plotting the spectrum of the corresponding RGB and HCL
   dimensions. Returns decomposed dimensions.
}

\usage{
   specplot(x, cex = 1, plot = TRUE ) 
}

\arguments{
   \item{x}{character vector containing HEX colors.}
   \item{cex}{Optional, numeric. Scaling for figure axis and labels.}
   \item{plot}{Boolean, default \code{TRUE}. Weather to plot the 
      spectrum or not.}
}

\details{
Decomposes a vector of hexadecimal colors (HEX) into the corresponding
red-green-blue (RGB) and hue-chroma-luminance (HCL) dimensions. If
\code{plot=TRUE} a spectral plot will be drawn.
This function allows to examine a certain HEX color map in both
color spaces (RGB and HCL).
}

\value{
Returns a list containing two matrices (\code{HCL}, \code{RGB}) and
a vector containing the initial HEX color map (\code{hex}).

The matrices contain the three dimensions hue-chroma-luminance (HCL)
and red-green-blue (RGB) respectively.
}

\author{Reto Stauffer, Achim Zeileis}

\references{
  Zeileis A, Hornik K, Murrell P (2009).
  Escaping RGBland: Selecting Colors for Statistical Graphics.
  \emph{Computational Statistics & Data Analysis}, \bold{53}, 3259--3270.
  \doi{10.1016/j.csda.2008.11.033}
  Preprint available from \url{https://eeecon.uibk.ac.at/~zeileis/papers/Zeileis+Hornik+Murrell-2009.pdf}.

  Stauffer R, Mayr GJ, Dabernig M, Zeileis A (2015).
  Somewhere over the Rainbow: How to Make Effective Use of Colors in Meteorological Visualizations.
  \emph{Bulletin of the American Meteorological Society}, \bold{96}(2), 203--216.
  \doi{10.1175/BAMS-D-13-00155.1}
}

\seealso{
\code{\link{rainbow_hcl}}
}

\examples{
if(interactive()) {
## Show spectrum of the (in)famous RGB rainbow scheme
specplot( rainbow(100), plot = TRUE )
## Show spectrum of the HCL rainbow scheme provided
## by the colorspace package
specplot( rainbow_hcl(100), plot = TRUE )
## Return decomposed values to console
res <- specplot( rainbow(10), plot = FALSE )
print(res)
}
}

\keyword{misc}
