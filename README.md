# <a href = "https://imi-rhapsody.eu/"><img src="docs/RHAPSODY_Logo_WEB_Color.png" width="300" /></a>

<!-- badges: start -->
![License](https://img.shields.io/github/license/mcanouil/RHAPSODY)
<!-- badges: end -->

Website: https://imi-rhapsody.eu/

## Render `docs/howto.html`
``` sh
docker run --rm --volume /media/Project/RHAPSODY/Scripts/docs:/media/docs umr1283/rhapsody:latest Rscript -e 'rmarkdown::render("/media/docs/howto.Rmd", output_file = "index.html", encoding = "UTF-8")'
```

## Build/Update Docker image
``` sh
VERSION=1.3.0

## Build image
docker build \
  --tag umr1283/rhapsody:$VERSION \
  --compress \
  --file /media/Project/RHAPSODY/Scripts/docker/Dockerfile \
  /media/Project/RHAPSODY/Scripts/docker/

## Tag image with name and version
docker tag umr1283/rhapsody:$VERSION umr1283/rhapsody:latest

## Push image to Docker Hub
docker push umr1283/rhapsody:$VERSION
docker push umr1283/rhapsody:latest

## Clean temporary image
docker system prune --all
```
 
## Build Docker container
``` sh
docker run \
  --name rhapsody \
  --hostname rhapsody \
  --detach \
  --volume /media/Data:/media/DATA \
  --volume /media/Project:/media/PROJECT \
  --volume /media/Datatmp:/media/DATATMP \
  --publish 9999:8787 \
  umr1283/rhapsody:latest
```
