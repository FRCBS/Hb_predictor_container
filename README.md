# Hb_predictor_container

Hemoglobin predictor wrapped in a Docker container. Download
the docker image with command `docker pull toivoja/hb-predictor-plumber`.


# Creating the docker image from source

You can also build the docker image from source code yourself.
One can compose a new docker image from several docker images building on top
of each other. Building images in this layered fashion eases the development.
There exists a ready-made docker image containing a recent base version of R
called `rocker/r-ubuntu`.

Using command

```docker build -f Dockerfile-r-base-tidyverse-binary-ubuntu -t toivoja/r-base-tidyverse .```

we build an intermediate image that adds many additional packages to the above image.
Some R packages are also installed from sources.
These packages are listed in file `src/docker-install-packages2.R`.
This second phase takes about 40 minutes on my machine.

The third docker image adds our web application built with the Plumber library.
Use command

```docker build -f Dockerfile-plumber -t toivoja/hb-predictor-plumber .```

to build it. This phase is very quick (currently 3 minutes 14 seconds), which makes it easy to test changes to the script.
You can list the docker images on this machine using the command
docker images

# Running the container

You can run the final image using

```docker container run -it --rm -p 8080:8080 toivoja/hb-predictor-plumber```  

Then go to the following address in the browser:

http://localhost:8080/hb-predictor

Kill the server (and the server) by pressing control-c.

The docker images are normally stored somewhere on the file system
or on docker hub in an unspecified form.
You can however export a docker image into a single tar file with

```docker save toivoja/hb-predictor-plumber > hb-predictor-plumber.tar```
   
Importing an image from a tar file is done by

```docker load -i hb-predictor-plumber.tar```

# References

The following page contains examples on building R script based docker
applications:
https://www.r-bloggers.com/running-your-r-script-in-docker/
And the following page shows how a simple web application can be build with
R package Rook:
https://anythingbutrbitrary.blogspot.com/2012/12/a-simple-web-application-using-rook.html
Why Rook does not work well with docker:
https://pythonspeed.com/articles/docker-connection-refused/
Plumber for creating a webserver that can listen to all interfaces, instead of just localhost:
https://www.rplumber.io/docs/index.html
Installing rstan:
https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Linux
