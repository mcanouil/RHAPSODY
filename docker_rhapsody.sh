#!/bin/sh

# Set home directory
# nohup sh /media/Project/RHAPSODY/Scripts/docker/docker_rhapsody.sh >> /media/Project/RHAPSODY/Scripts/docker/docker_rhapsody.log &

# Login with docker ID (docker hub or docker cloud)
# docker login

# Build image
docker build --tag biostatgood/rhapsody:1.0.5 --compress /media/Project/RHAPSODY/Scripts/docker

# Tag image with name and version
docker tag biostatgood/rhapsody:1.0.5 biostatgood/rhapsody:latest

# Push image to docker hub
docker push biostatgood/rhapsody:1.0.5
docker push biostatgood/rhapsody:latest

# docker system prune --all

# Run image
# docker run \
# --name stat-rhapsody \
# --hostname stat-rhapsody \
# --detach \
# --volume /from/local/directory/:/to/docker/directory/target \
# --publish 22:22 \
# --publish 8787:8787 \
# biostatgood/rhapsody:1.0.0



# docker stop stat-rhapsody && docker rm stat-rhapsody
# docker build --tag rhapsody:$VER --rm --compress ./docker
# docker run \
# --name stat-rhapsody \
# --hostname stat-rhapsody \
# --detach \
# --volume /media/Data:/media/DATA \
# --volume /media/Project:/media/PROJECT \
# --volume /media/Datatmp:/media/DATATMP \
# --publish 8791:8787 \
# --publish 2291:22 \
# biostatgood/rhapsody:1.0.0
# docker ps -a
