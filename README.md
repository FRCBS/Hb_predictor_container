# Hb_predictor_container

Hemoglobin predictor wrapped in a Docker container. Download
the docker image with command `docker pull toivoja/hb-predictor`.
A specific version (0.23) can be downloaded with `docker pull toivoja/hb-predictor:0.23`.

## Running the container

### Running from browser

You can run the final image using

```docker container run -it --rm -p 8080:8080 toivoja/hb-predictor```  

Then go to the following address in the browser:

http://localhost:8080/hb-predictor

Kill the server (and the server) by pressing control-c.

### Running from command line

Note, this section contains obsolete information! The option to do
the analyses from command line are not currently supported. Use the browser user interface instead.

docker container run -i --rm -v /whicheverfolderinputisin:/input toivoja/hb-predictor src/hb-predictor.sh date=2020-07-15 gender=female sample_fraction=0.002 method=icp-fix hlen=1 input_file=/input/inputfilename.rdata

Options:

- gender is either 'male', 'female', or 'both'
- method is either 'no-fix', 'icp-fix', or 'both'
- The input file can be specified with input_file option

The results will be stored to file `results.tar.gz`.

## Creating the docker image from source

You can also build the docker image from source code yourself.
One can compose a new docker image from several docker images building on top
of each other. Building images in this layered fashion eases the development.
There exists a ready-made docker image containing a recent base version of R
called `rocker/r-ubuntu`.

Using command

```docker build -f Dockerfile-r-base-tidyverse -t toivoja/r-base-tidyverse .```

we build an intermediate image that adds many additional packages to the above image.
Some R packages are also installed from sources.
These packages are listed in file `src/docker-install-packages2.R`.
This second phase takes about 40 minutes on my machine.

The third docker image adds our web application built with the Plumber library.
Use command

```docker build -f Dockerfile -t toivoja/hb-predictor .```

to build it. This phase is very quick (currently 3 minutes 14 seconds), which makes it easy to test changes to the script.
You can list the docker images on this machine using the command
docker images


## Exporting or importing a container to or from a file

The docker images are normally stored somewhere on the file system
or on docker hub in an unspecified form.
You can however export a docker image into a single tar file with

```docker save toivoja/hb-predictor > hb-predictor.tar```
   
Importing an image from a tar file is done by

```docker load -i hb-predictor.tar```

## References

- The following page contains examples on building R script based docker applications:
  https://www.r-bloggers.com/running-your-r-script-in-docker/
- And the following page shows how a simple web application can be build with R package Rook:
  https://anythingbutrbitrary.blogspot.com/2012/12/a-simple-web-application-using-rook.html
- Why Rook does not work well with docker:
  https://pythonspeed.com/articles/docker-connection-refused/
- Plumber for creating a webserver that can listen to all interfaces, instead of just localhost:
  https://www.rplumber.io/docs/index.html
- Installing rstan:
  https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Linux
