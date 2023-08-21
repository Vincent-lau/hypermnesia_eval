#!/usr/bin/env bash

cmd='./bin/ycsb run mongodb -s -P workloads/workloada -p mongodb.url="mongodb://mongodb-enterprise-0.mongodb-enterprise-svc.mongodb.svc.cluster.local:27017,mongodb-enterprise-1.mongodb-enterprise-svc.mongodb.svc.cluster.local:27017,mongodb-enterprise-2.mongodb-enterprise-svc.mongodb.svc.cluster.local:27017/?connectTimeoutMS=20000&replicaSet=mongodb-enterprise&serverSelectionTimeoutMS=20000" > outputRun.txt'

kubectl exec -it mongosh-pod -- bash -c "cd /usr/local/ycsb-0.17.0 && $cmd"
kubectl cp mongosh-pod:/usr/local/ycsb-0.17.0/outputRun.txt logs/logs1.txt
cat logs/logs1.txt | tee -a logs/outputRun.txt

echo "------------------------------------------------------------------------" >> logs/outputRun.txt
