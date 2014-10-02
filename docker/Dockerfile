FROM ubuntu:14.04

# Ensure UTF-8
RUN locale-gen en_US.UTF-8
ENV LANG       en_US.UTF-8
ENV LC_ALL     en_US.UTF-8

RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install software-properties-common
RUN DEBIAN_FRONTEND=noninteractive add-apt-repository ppa:webupd8team/java -y
RUN DEBIAN_FRONTEND=noninteractive apt-get update

# automatically accept oracle license
RUN echo oracle-java7-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections

RUN DEBIAN_FRONTEND=noninteractive apt-get -y install unzip s3cmd git curl oracle-java7-installer
RUN update-alternatives --display java
# set the java environment variables for when you "bash -l"
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install oracle-java7-set-default && apt-get clean

# Setup the app
EXPOSE 80
ADD bip70sender-1.0-SNAPSHOT-shaded.jar /
CMD ["/bin/sh", "-c", "java $JAVA_OPTS -jar /bip70sender-1.0-SNAPSHOT-shaded.jar"]