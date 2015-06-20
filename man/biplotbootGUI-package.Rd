\name{biplotbootGUI-package}
\alias{biplotbootGUI-package}
\alias{biplotbootGUI}
\docType{package}
\title{biplotbootGUI: Bootstrap on Classical Biplots and Clustering Disjoint Biplot}
\description{
The biplotbootGUI package is a graphical user interface to construct and interact
with Classical Biplots and, combined with Bootstrap methods, provides confidence 
intervals based on percentiles, t-bootstrap and BCa to measure the accuracy of the 
estimators of the parameters given by them.
}
\details{
\tabular{ll}{
Package: \tab biplotbootGUI\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2015-06-20\cr
License: \tab GPL>=2\cr
}
}
\author{Ana Belen Nieto Librero \email{ananieto@usal.es}, 
Purificacion Galindo Villardon \email{pgalindo@usal.es}

Maintainer: Ana Belen Nieto Librero \email{ananieto@usal.es}
}
\references{
\itemize{

\item Gabriel, K. R. (1971). The Biplot graphic display of matrices with applications to principal components analysis. Biometrika, 58(3), 453-467.

\item Galindo, M. P. (1986). Una alternativa de representacion simultanea: HJ-Biplot. Questiio, 10(1), 13-23.

\item Efron, B. (1979). Bootstrap methods: Another look at the jackknife. Annals of Statistics, 7, 1-26.

\item Efron, B. (1987). Better bootstrap confidence intervals. Journal of the American Statistical Association, 82, 171-185.

\item Efron, B., & Tibshirani, R. J. (1993). An introduction into the bootstrap. New York: Chapman and Hall.

\item Nieto, A. B., & Galindo, M. P., & Leiva, V., & Vicente-Galindo, P. (2014). A methodology for biplots based on bootstrapping with R. Revista Colombiana de Estadistica, 37(2), 367-397.
}
}

\keyword{ package }
\examples{
data(iris)
biplotboot(iris[,-5])}
