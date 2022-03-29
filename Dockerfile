# Create the application image using the following command:
# docker build -f Dockerfile -t toivoja/hb-predictor .

#FROM vinkenoogm/r-base-tidyverse:latest
FROM toivoja/r-base-tidyverse:latest

#RUN apt-get update -qq && apt-get -y --no-install-recommends install \
#r-cran-bh

## create directories
RUN mkdir -p /src
RUN mkdir -p /src/stan
RUN mkdir -p /output
RUN mkdir -p /static
RUN mkdir -p /data/rdata /data/rdump /data/stan_fits /data/raw_results

## copy files


COPY /src/stan/container.stan  /src/stan/container_heckman_consts.stan \
     /src/stan/container_heckman.stan /src/stan/container_consts.stan \
     /src/stan/oos.stan  /src/stan/oos_heckman_consts.stan \
     /src/stan/oos_heckman.stan /src/stan/oos_consts.stan \
     /src/stan/



COPY /static/style.css /static/script.js /static/bootstrap.min.css \
     /static/bloodservice_logo.png /static/FundedbyEBA.jpg \
     /static/


COPY /src/docker-server-plumber.R /src/docker-apps-plumber.R \
     /src/enrich_deferrals_rf.R /src/new_preprocess.R \
     /src/sanquin_preprocess.R /src/helper_functions.R \
     /src/validate_stan_fit.R /src/create_stan_datasets.R \
     /src/hb-predictor.sh /src/hb-predictor-helper.R /src/linear_models.Rmd \
     /src/template.Rmd /src/random_forest.Rmd /src/Makefile /src/parse.cpp \
     /src/common.R /src/svm.Rmd /src/baseline.Rmd \
     /src/

## Build the 'parse' executable, which parses the multipart html form
## parameters.
RUN make -C src

WORKDIR /src
## run the script
CMD Rscript docker-server-plumber.R
