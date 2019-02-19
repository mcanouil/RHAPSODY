RHAPSODY
========

Latest version: `VERSION=1.2.0`

## Merge all Gist into one repository

### Init main repository

```
mkdir -p /disks/PROJECT/RHAPSODY/github
cd /disks/PROJECT/RHAPSODY/github
git init 
touch README.md
git add --all
git commit -m 'initial commit'
git remote add origin https://github.com/mcanouil/RHAPSODY.git
```

### Add `main` as a subtree

```
git remote add -f main https://gist.github.com/15362a96c1561bb51af98760b41c478e.git
git merge -s ours --no-commit --allow-unrelated-histories main/master
git read-tree --prefix=/ -u main/master
git commit -m 'merge with main'
```

### Add `utils` as a subtree

```
git remote add -f utils https://gist.github.com/f3e2fdc59757fd8577abfe233854580a.git
git merge -s ours --no-commit --allow-unrelated-histories utils/master
git read-tree --prefix=utils/ -u utils/master
git commit -m 'merge with utils'
```

### Add `docker` as a subtree

```
git remote add -f docker https://gist.github.com/de2b8293fbb1d0d950881cf6290e78c4.git
git merge -s ours --no-commit --allow-unrelated-histories docker/master
git read-tree --prefix=docker/ -u docker/master
git commit -m 'merge with docker'
```

### Add `docker_analysis` as a subtree

```
git remote add -f docker_analysis https://gist.github.com/1e2ee952f1cf05e9b6e91c58f2861bc8.git
git merge -s ours --no-commit --allow-unrelated-histories docker_analysis/master
git read-tree --prefix=docker_analysis/ -u docker_analysis/master
git commit -m 'merge with docker_analysis'
```

### Push everything

```
git push origin master
git push --tags
```

## Update all scripts from each Gist

```
git subtree pull --prefix ./ main master
git commit -am 'update main'

git subtree pull --prefix utils/ utils master
git commit -am 'update utils'

git subtree pull --prefix docker/ docker master
git commit -am 'update docker'

git subtree pull --prefix docker_analysis/ docker_analysis master
git commit -am 'update docker_analysis'

git push origin master
git tag VERSION -m 'Update all scripts'
git push --tags
```

## Docker

### Build image

```
docker build \
  --tag umr8199/rhapsody:VERSION \
  --compress \
  --file /media/Project/RHAPSODY/Scripts/docker/Dockerfile_update \
  /media/Project/RHAPSODY/Scripts/docker/
```

### Tag image with name and version

```
docker tag umr8199/rhapsody:VERSION umr8199/rhapsody:latest
```

### Push image to Docker Hub

```
docker push umr8199/rhapsody:VERSION
docker push umr8199/rhapsody:latest
```

### Clean temporary image

```
docker system prune --all
```
