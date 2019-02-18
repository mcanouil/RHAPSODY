#!/bin/bash

docker run \
  --name rhapsody \
  --cpus 20 \
  --detach \
  --volume /media/vcfs:/media/vcf \
  --volume /media/docker_analysis:/media/docker_analysis \
  --volume /media/rhapsody_output:/media/rhapsody_output \
  --rm \
  umr8199/rhapsody:latest Rscript /media/docker_analysis/rhapsody.R COHORT 7 TRUE 
