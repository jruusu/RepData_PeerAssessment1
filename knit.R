# A little utility script for building Rmd into a html document
library(knitr)
knit2html("PA1_template.Rmd", options="")
browseURL("PA1_template.html")