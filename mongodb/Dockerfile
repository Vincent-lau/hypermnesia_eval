FROM mongo

RUN apt-get update && apt-get install -y openjdk-11-jre wget python2 && \
    wget https://dlcdn.apache.org/maven/maven-3/3.9.4/binaries/apache-maven-3.9.4-bin.tar.gz && \
    tar -xvf apache-maven-3.9.4-bin.tar.gz -C /usr/local && \
    cd /usr/local && \
    ln -s apache-maven-3.9.4 maven && \
    export PATH=/usr/local/maven/bin:$PATH && \
    wget https://github.com/brianfrankcooper/YCSB/releases/download/0.17.0/ycsb-0.17.0.tar.gz && \
    tar -xvf ycsb-0.17.0.tar.gz && \
    cd ycsb-0.17.0 && \
    sed -i 's/python/python2/g' bin/ycsb
