\name{CDBiplot}
\alias{CDBiplot}
\title{Clustering and/or Disjoint Biplot
}
\description{The CDBiplot function is a graphical user interface to construct and interact
with Clustering and/or Disjoint Biplot.
}
\usage{
CDBiplot(data, clase)
}

\arguments{
  \item{data}{A data frame with the information to be analyzed}
  \item{clase}{A vector containing the real classification of the objects in the data}
}
\details{
When the function is launched,  
firstly, it is necessary to select the kind of analysis to be used on the data. Then, a window to select the number of clusters, components, the tolerance, the number of iterations and the repetitions of the algorithm. Press the OK button and the graph will be shown. Press the left mouse button and a window will be displayed to select one option:
Change the position label, Remove label or Do nothing.
It is also possible to select the dimensions shown in the graph and to change the limits of the axes.
In the window there are four menus:

\itemize{
        \item File
        \itemize{
        	\item Copy image
		\item Save image
		\itemize{
			\item PDF file
			\item Eps file
			\item Png file
			\item Jpg/Jpeg file
		}
		\item Exit
	}
	\item 3D
	\itemize{
		\item 3D
	}
	\item Options
	\itemize{
		\item Change title
		\item Show/Hide axes
        	\item Show/Hide variables
        	\item Show/Hide row labels
	}	
	\item Cluster
	\itemize{
		\item Convex-hull
	}
}

The File menu provides different options to save the graph and permits to exit the program. The second menu shows the graph in 3 dimensions. The third menu allows the user to change the title and to show/hide the axes, the variables and the row labels in the graph. The last menu permits the user to draw (filled or empty) convex-hull on each cluster. 
The program saves a file containing the main results of the analysis.
}
\value{
A graph showing the data representation and an output file containing the 
information about the results.
}

\references{
\itemize{

\item Gabriel, K. R. (1971). The Biplot graphic display of matrices with applications to principal components analysis. Biometrika, 58(3), 453-467.

\item Galindo, M. P. (1986). Una alternativa de representacion simultanea: HJ-Biplot. Questiio, 10(1), 13-23.

\item Vichi, M and Saporta, G. (2009). Clustering and disjoint principal component analysis.
\emph{Computational Statistics and Data Analysis}, 53, 3194-3208.

\item Macedo, E. and Freitas, A. (2015). The alternating least-squares algorithm for CDPCA.
\emph{Communications in Computer and Information Science (CCIS), Springer
Verlag pp. 173-191}.
}
}

\author{Ana Belen Nieto Librero \email{ananieto@usal.es}, 
Purificacion Vicente Galindo \email{purivg@usal.es}, 
Purificacion Galindo Villardon \email{pgalindo@usal.es}}

\examples{
data(iris)
CDBiplot(iris[,-5], iris[,5])
}
\keyword{ multivariate }
