\name{biplotboot}
\alias{biplotboot}
\title{biplotbootGUI: Bootstrap methods on Classical Biplots}
\description{
The biplotbootGUI package is a graphical user interface to construct and interact
with Classical Biplots and, combined with Bootstrap methods, provides confidence 
intervals based on percentiles and t-bootstrap to measure the accuracy of the 
estimators of the parameters given by them.
}
\usage{
biplotboot(x)
}
\arguments{
  \item{x}{A data frame with the information to be analyzed}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\references{
Gabriel, K. R. (1971). The Biplot graphic display of matrices with applications to principal components analysis. Biometrika, 58(3), 453-467.
Galindo, M. P. (1986). Una alternativa de representacion simultanea: HJ-Biplot. Questiio, 10(1), 13-23.
Efron, B. (1979). Bootstrap methods: Another look at the jackknife. Annals of Statistics, 7, 1-26.
Efron, B. (1987). Better bootstrap confidence intervals. Journal of the American Statistical Association, 82, 171-185.
Efron, B., & Tibshirani, R. J. (1993). An introduction into the bootstrap. New York: Chapman and Hall.
}
\author{Ana Belen Nieto Librero \email{ananieto@usal.es}, 
Purificacion Galindo Villardon \email{pgalindo@usal.es}}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
data(iris)
biplotboot(iris[,-5])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
