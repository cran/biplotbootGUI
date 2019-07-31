#############################################
#           Function CDpcaSummary           #
#############################################
CDpcaSummary <- function(obj) {
        cat("Summary of Cluster and Disjoint Principal Component Analysis:\n")
        print(rbind("Loop with best result:" = obj$loop, "Number of iterations:" = obj$iter,
                    "F:" = round(obj$F, 5), "Error:" = round(obj$Enorm, 5),
                    "Between cluster deviance (%):" = round(obj$bcdev, 2)))
        print(rbind("Explained variance by CDpca components" = obj$dorder)) 
        cat("Pseudo Confusion Matrix:\n")
        print(obj$pseudocm)
        invisible(obj)
}  # end CDpcaSummary function
