% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{setFeatureIDs}
\alias{setFeatureIDs}
\title{Helper function: Defines feature ids for gis objectes}
\usage{
setFeatureIDs(x)
}
\arguments{
\item{x}{data.frame with gis features as retrieved by importShapefile()}
}
\value{
input data.frame and new column feature id (fID)
}
\description{
Aggregates gis features by shape.name & id add adds new column fID
}
