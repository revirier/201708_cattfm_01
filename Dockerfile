########################################################################
# Dockerfile for Oracle JDK 8 on Ubuntu 16.04
########################################################################

# pull base image
FROM rocker/tidyverse:3.4.1

# maintainer details
MAINTAINER revirier "revirier"

# add a post-invoke hook to dpkg which deletes cached deb files
# update the sources.list
# update/dist-upgrade
# clear the caches

RUN \
  apt-get update -q -y && \
  apt-get dist-upgrade -y && \
  apt-get clean && \
  rm -rf /var/cache/apt/* && \

# Install packages
  DEBIAN_FRONTEND=noninteractive apt-get install -y wget unzip apt-utils python-pip software-properties-common  && \

# Install Oracle Java 8
  DEBIAN_FRONTEND=noninteractive apt-get install -y wget unzip && \
  add-apt-repository -y ppa:webupd8team/java && \
  apt-get update -q && \
  echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && \
  echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections && \
  DEBIAN_FRONTEND=noninteractive apt-get install -y  --allow-unauthenticated oracle-java8-installer && \
  apt-get clean 

# Fetch h2o R latest_stable

# install packages
RUN echo 'if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }' > /tmp/packages.R \
    && Rscript /tmp/packages.R

RUN echo 'if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }' > /tmp/packages.R \
    && Rscript /tmp/packages.R

RUN echo 'pkgs <- c("statmod","RCurl","jsonlite") \n for (pkg in pkgs) { if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }}' > /tmp/packages.R \
    && Rscript /tmp/packages.R

RUN echo 'install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-vajda/4/R")' > /tmp/packages.R \
    && Rscript /tmp/packages.R

RUN echo 'install.packages("caret")' > /tmp/packages.R \
    && Rscript /tmp/packages.R

RUN echo 'install.packages("pROC")' > /tmp/packages.R \
    && Rscript /tmp/packages.R


# h2o ports
EXPOSE 54321
EXPOSE 54322

#CMD ["/init"]
