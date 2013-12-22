\name{biplotbootGUI-package}
\alias{biplotbootGUI-package}
\alias{biplotbootGUI}
\docType{package}
\title{biplotbootGUI: Bootstrap methods on Classical Biplots}
\description{
The biplotbootGUI package is a graphical user interface to construct and interact
with Classical Biplots and, combined with Bootstrap methods, provides confidence 
intervals based on percentiles and t-bootstrap to measure the accuracy of the 
estimators of the parameters given by them.
}
\details{
\tabular{ll}{
Package: \tab biplotbootGUI\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-12-22\cr
License: \tab GPL>=2\cr
}
The biplotbootGUI package is a graphical user interface to construct and interact
with Classical Biplots and, combined with Bootstrap methods, provides confidence 
intervals based on percentiles and t-bootstrap to measure the accuracy of the 
estimators of the parameters given by them. biplotbootGUI package is designed to 
run under any of the major plataforms (Windows, Linux and MacOSX). Only is necessary 
to install the toolkit ``bwidget''.

}
\author{Ana Belen Nieto Librero \email{ananieto@usal.es}, 
Purificacion Galindo Villardon \email{pgalindo@usal.es}

Maintainer: Ana Belen Nieto Librero \email{ananieto@usal.es}
}
\references{
Gabriel, K. R. (1971). The Biplot graphic display of matrices with applications to principal components analysis. Biometrika, 58(3), 453-467.
Galindo, M. P. (1986). Una alternativa de representacion simultanea: HJ-Biplot. Questiio, 10(1), 13-23.
Efron, B. (1979). Bootstrap methods: Another look at the jackknife. Annals of Statistics, 7, 1-26.
Efron, B. (1987). Better bootstrap confidence intervals. Journal of the American Statistical Association, 82, 171-185.
Efron, B., & Tibshirani, R. J. (1993). An introduction into the bootstrap. New York: Chapman and Hall.
}

\keyword{ package }
\examples{
data(iris)
biplotboot(iris[,-5])}
