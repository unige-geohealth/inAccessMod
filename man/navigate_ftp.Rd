% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/navigate_ftp.R
\name{navigate_ftp}
\alias{navigate_ftp}
\title{Navigate Wolrd Pop FTP}
\usage{
navigate_ftp(folderLst, iso, pathFTP, pathFTP0, testMode)
}
\arguments{
\item{folderLst}{character vector; folder names within the main World Pop FTP directory}

\item{iso}{character; the country ISO alpha-3 code}

\item{pathFTP}{character; the path of the main World Pop FTP directory. The variable IS updated during the process.}

\item{pathFTP0}{character; the path of the main World Pop FTP directory. The variable IS NOT updated during the process.}
}
\value{
a list of length 2; the first element is a character vector that contains the available files in the targeted folder
and the second element is the path of the targeted folder.
}
\description{
Internal function used to navigate through the World Pop FTP's folders.
}
\details{
The function keeps running while no folder whose name is the country ISO code is reached.
}
\keyword{internal}
