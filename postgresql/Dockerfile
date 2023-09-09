FROM postgres

COPY ./init-scripts/init.sql /docker-entrypoint-initdb.d/init.sql

RUN apt-get update && apt-get install -y git openjdk-17-jdk wget vim build-essential libsqlite3-dev zlib1g-dev libncurses5-dev libgdbm-dev libssl-dev libdb-dev && \
    wget https://dlcdn.apache.org/maven/maven-3/3.9.4/binaries/apache-maven-3.9.4-bin.tar.gz && \
    tar -xvf apache-maven-3.9.4-bin.tar.gz -C ./ && \
    rm -f /usr/bin/mvn && \
    ln -s /apache-maven-3.9.4/bin/mvn /usr/bin/mvn && \
    wget https://www.python.org/ftp/python/2.7.2/Python-2.7.2.tgz && \
    tar -xzf Python-2.7.2.tgz && \
    cd Python-2.7.2 && \
    ./configure --prefix=/usr --enable-shared && \
    make clean && \
    make install && \ 
    cd .. && \
    # wget https://github.com/brianfrankcooper/YCSB/releases/download/0.17.0/ycsb-0.17.0.tar.gz && \
    # tar -xvf ycsb-0.17.0.tar.gz && \
    rm -rf YCSB && \
    git clone https://github.com/brianfrankcooper/YCSB.git && \
    cd YCSB && \
    sed -i 's/9\.4\.1212\.jre7/42.2.24/g' postgrenosql/pom.xml && \
    mvn -pl site.ycsb:postgrenosql-binding -am clean package
