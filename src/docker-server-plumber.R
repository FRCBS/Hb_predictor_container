suppressPackageStartupMessages(library(plumber))
suppressPackageStartupMessages(library(Rook))  # For parsing multipart http requests
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(knitr))


pr <- plumber::plumb("src/docker-apps-plumber.R")

cat("Open address http://localhost:8080/hb-predictor in your browser.\n")
cat("Press control-c to kill the server\n")

pr$run(host='0.0.0.0', port=8080) # Listen to the specified port on all interfaces
