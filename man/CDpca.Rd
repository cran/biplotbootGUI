\name{CDpca}
\alias{CDpca}

\title{Clustering and Disjoint Principal Component Analysis}
\description{
CDpca performs a clustering and disjoint principal components analysis (CDPCA) on the given numeric data matrix and returns a list of results
Given a (IxJ) real data matrix X = [xij], the CDPCA methodology is allowed  to cluster the I objects into P nonempty and nonoverlapping clusters Cp, p = 1,...,P, which are identified by theirs centroids, and, simultaneously, to partitioning the J attributes into Q disjoint components, PCq, q = 1,...,Q.
The CDpca function models X estimating the parameter of the model using an Alternating Least Square (ALS) procedure originally proposed by Vichi and Saport (2009) and described in two steps by Macedo and Freitas (2015).
}
\usage{
CDpca (data, class=NULL, P, Q, SDPinitial=FALSE, tol= 10^(-5), maxit, r, cdpcaplot=TRUE)
}

\arguments{
\item{data}{A numeric matrix or data frame which provides the data for the CDPCA}
\item{class}{A numeric vector containing the real classification of the objects in the data, or NULL  if the class of objects is unknown}       
\item{P}{An integer value indicating the number of clusters of objects}
\item{Q}{An integer value indicating the number of clusters of variables}
\item{SDPinitial}{A logical value indicating whether the initial assignment matrices U and V are randomly generated (by default) or an algorithmic framework based on a semidefinite programming approach is preferred (TRUE)}
\item{tol}{A positive (low) value indicating the maximum term for the difference between two consecutives values of the objective function. A tolerance value of 10^(-5) is indicated by default}
\item{maxit}{The maximum number of iterations of one run of the ALS algorithm}
\item{r}{Number of runs of the ALS algorithm for the final solution}
\item{cdpcaplot}{A logical value indicating whether an additional graphic is created (showing the data projected on the first two CDPCA principal components)}
}


\value{
Cdpca returns a list of results containing the following components: 

\item{Iter}{The total number of iterations used in the best loop for computing the best solution}
\item{loop}{The best loop number}
\item{timebestloop}{The computation time on the best loop}
\item{timeallloops}{The computation time for all loops}
\item{Y}{The component score matrix}
\item{Ybar}{The object centroids matrix in the reduced space}
\item{A}{The component loading matrix}
\item{U}{The partition of objects}
\item{V}{The partition of variables}
\item{F}{The value of the objective function to maximize}
\item{bcdev}{The between cluster deviance}
\item{bcdevTotal}{The between cluster deviance over the total variability}
\item{tableclass}{The cdpca classification}
\item{pseudocm}{The pseudo confusion matrix concerning the true (given by class) and cdpca classifications}
\item{Enorm}{The error norm for the obtained cdpca model} 

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
