\name{CDpcaSummary}
\alias{CDpcaSummary}
\title{
Clustering and Disjoint Principal Component Analysis Summary}

\description{
Produce a list of summary measures to evaluate the result of the CDPCA
}
\usage{
CDpcaSummary(obj)
}

\arguments{
  \item{obj}{An object of the type produced by CDpca}
}

\value{
CDpcaSummary returns the following values associated to the loop where the best result was produced:

Number of the loops

Number of iterations

Value of the objective function F

Frobenius norm of the error matrix

Between cluster deviance (percentage)

Explained variance by CDpca components (percentage)

Pseudo Confusion Matrix (if available)
}
\references{
\itemize{

\item Vichi, M and Saporta, G. (2009). Clustering and disjoint principal component analysis.
\emph{Computational Statistics and Data Analysis}, 53, 3194-3208.

\item Macedo, E. and Freitas, A. (2015). The alternating least-squares algorithm for CDPCA.
\emph{Communications in Computer and Information Science (CCIS), Springer
Verlag pp. 173-191}.
}
}

\author{
Eloisa Macedo \email{macedo@ua.pt}, 
Adelaide Freitas \email{adelaide@ua.pt},
Maurizio Vichi \email{maurizio.vichi@uniroma1.it}
}

\keyword{multivariate}
