FROM rocker/r-ver:latest

RUN apt-get update && apt-get install -y \
      libcurl4-openssl-dev \
      libv8-dev

RUN install2.r --error \
        R6 \
        jsonlite \
        jsonvalidate \
        plumber \
        remotes \
        testthat

RUN installGithub.r ropensci/jsonvalidate

COPY . /src
RUN R CMD INSTALL /src
