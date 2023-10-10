-------------------------------------------------

			# R-WORK #

-------------------------------------------------

########################################################
### R

https://www.r-project.org/

https://posit.co/ (Rstudio)

########################################################
### Reference

https://wikidocs.net/book/4315


########################################################
### Package Install

install.packages("psych")
install.packages("descr")
install.packages("gmodels")
install.packages("MASS")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("fBasics")
install.packages("QuantPsyc")
install.packages("prettyR")
install.packages("e1071")
install.packages('abind')
install.packages('zoo')
install.packages('xts')
install.packages('quantmod')
install.packages('ROCR')
install.packages("https://cran.r-project.org/src/contrib/Archive/DMwR/DMwR_0.4.1.tar.gz", repos = NULL, type="source")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("party")
install.packages("nnet")
install.packages("downloader")
install.packages("neuralnet")
install.packages("tree")
install.packages("caret")
install.packages("arules")
install.packages("arulesViz")
install.packages("lattice")
install.packages("topicmodels")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("sna")
install.packages("igraph")
install.packages("rgl")
install.packages("rvest")
install.packages("stringr")
install.packages("XML")
install.packages("readxl")
install.packages("WriteXLS")
# rJava 설치
install.packages("multilinguer")	
library(multilinguer)
install_jdk()
# KoNLP 패키지 설치시 참조 또는 이용되는 패키지 설치
install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary")
# 깃허브의 KoNLP 패키지 설치
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
# 텍스트마이닝을 위한 tm 패키지 설치
require(remotes)
install_version("tm", version = "0.7-5", repos = "http://cran.us.r-project.org")
install.packages("ggplot2")
install.packages("treemap")
install.packages("aplpack")
install.packages("raster")
install.packages("rgeos")
install.packages("rgdal")
install.packages("ggmap")

