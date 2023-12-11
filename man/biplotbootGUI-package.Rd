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
Version: \tab 1.3\cr
Date: \tab 2023-12-11\cr
License: \tab GPL>=2\cr
}
}
\author{Ana Belen Nieto Librero \email{ananieto@usal.es}, 
Adelaide Freitas \email{adelaide@ua.pt}

Maintainer: Ana Belen Nieto Librero \email{ananieto@usal.es}
}
\references{
\itemize{

\item Gabriel, K. R. (1971). The Biplot graphic display of matrices with applications to principal components analysis. Biometrika, 58(3), 453-467.

\item Galindo, M. P. (1986). Una alternativa de representacion simultanea: HJ-Biplot. Questiio, 10(1), 13-23.

\item Efron, B. (1979). Bootstrap methods: Another look at the jackknife. Annals of Statistics, 7, 1-26.

\item Efron, B. (1987). Better bootstrap confidence intervals. Journal of the American Statistical Association, 82, 171-185.

\item Efron, B., & Tibshirani, R. J. (1993). An introduction into the bootstrap. New York: Chapman and Hall.

\item Macedo, E. and Freitas, A. (2015). The alternating least-squares algorithm for CDPCA.
\emph{Communications in Computer and Information Science (CCIS), Springer
Verlag pp. 173-191}.

\item Nieto, A. B., & Galindo, M. P., & Leiva, V., & Vicente-Galindo, P. (2014). A methodology for biplots based on bootstrapping with R. Revista Colombiana de Estadistica, 37(2), 367-397.

\item Vichi, M and Saporta, G. (2009). Clustering and disjoint principal component analysis.
\emph{Computational Statistics and Data Analysis}, 53, 3194-3208.

}
}

\keyword{ package }
\examples{
data(iris)
biplotboot(iris[,-5])}
