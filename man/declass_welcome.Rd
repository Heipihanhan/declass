% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/declass_welcome.R
\name{declass_welcome}
\alias{declass_welcome}
\title{Dispaly the welcome page.}
\usage{
declass_welcome(return_type = "string")
}
\arguments{
\item{return_type}{The format of content displayed, in string or html}
}
\value{
The content of welcome page, in either string  or html format.
}
\description{
This function displays the welcome page of Declassification Engine API.
}
\examples{
declass_welcome("html")
declass_welcome("string")
}
