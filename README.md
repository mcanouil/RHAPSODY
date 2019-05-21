RHAPSODY
========

## Docker

``` sh
docker run --rm --volume /media/Project/RHAPSODY/Scripts/utils:/media/utils umr8199/rhapsody:latest Rscript -e 'rmarkdown::render("/media/utils/howto.Rmd", encoding = "UTF-8")'
```

``` sh
VERSION=1.2.15

## Build image
docker build \
  --tag umr8199/rhapsody:$VERSION \
  --compress \
  --file /media/Project/RHAPSODY/Scripts/docker/Dockerfile_update \
  /media/Project/RHAPSODY/Scripts/docker/

## Tag image with name and version
docker tag umr8199/rhapsody:$VERSION umr8199/rhapsody:latest

## Push image to Docker Hub
docker push umr8199/rhapsody:$VERSION
docker push umr8199/rhapsody:latest

## Clean temporary image
docker system prune --all
```
 
``` sh
docker run \
  --name stat-rhapsody \
  --hostname stat-rhapsody \
  --detach \
  --volume /media/Data:/media/DATA \
  --volume /media/Project:/media/PROJECT \
  --volume /media/Datatmp:/media/DATATMP \
  --publish 9999:8787 \
  umr8199/rhapsody:latest
```
