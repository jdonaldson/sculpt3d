R CMD CHECK sculpt3d
R CMD BUILD sculpt3d 
R CMD INSTALL sculpt3d 
# ftp -s:`ls -t| grep tar.gz | head -n1` ftp://CRAN.R-project.org/incoming


