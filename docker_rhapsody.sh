#!/bin/sh

# Set home directory
# nohup sh /media/Project/RHAPSODY/Scripts/docker/docker_rhapsody.sh >> /media/Project/RHAPSODY/Scripts/docker/docker_rhapsody.log &

# Login with docker ID (docker hub or docker cloud)
# docker login

# Build image
docker build --tag umr8199/rhapsody:1.1.0 --compress /media/Project/RHAPSODY/Scripts/docker

# Tag image with name and version
docker tag umr8199/rhapsody:1.1.0 biostatgood/rhapsody:latest

# Push image to docker hub
docker push umr8199/rhapsody:1.1.0
docker push umr8199/rhapsody:latest

# docker system prune --all

cd /disks/PROJECT/RHAPSODY/Data/
git clone https://gist.github.com/15362a96c1561bb51af98760b41c478e.git v1.1.0
cd v1.1.0
git clone https://gist.github.com/f3e2fdc59757fd8577abfe233854580a.git utils
git clone https://gist.github.com/de2b8293fbb1d0d950881cf6290e78c4.git docker
tar zcvf RAPSODY_WP3_Pre-Diabetes_v1.1.0.tar.gz *
/disks/DATA/Softwares/FEX/fexsend RAPSODY_WP3_Pre-Diabetes_v1.1.0.tar.gz mickael.canouil@cnrs.fr
cd /disks/PROJECT/RHAPSODY/Data/
rm -Rf v1.1.0


# Run image
# docker run \
# --name stat-rhapsody \
# --hostname stat-rhapsody \
# --detach \
# --volume /from/local/directory/:/to/docker/directory/target \
# --publish 22:22 \
# --publish 8787:8787 \
# umr8199/rhapsody:latest



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
# umr8199/rhapsody:latest
# docker ps -a
