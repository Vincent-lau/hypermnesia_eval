#!/usr/bin/env python3

import argparse
import os
from jinja2 import Environment, FileSystemLoader
from kubernetes import client, config
from termcolor import colored
import time

conn_str = "mongodb://mongodb-enterprise-0.mongodb-enterprise-svc.mongodb.svc.cluster.local:27017,mongodb-enterprise-1.mongodb-enterprise-svc.mongodb.svc.cluster.local:27017,mongodb-enterprise-2.mongodb-enterprise-svc.mongodb.svc.cluster.local:27017/?connectTimeoutMS=20000&replicaSet=mongodb-enterprise&serverSelectionTimeoutMS=20000"
config.load_kube_config()

def sleep(sleep_time: int, msg=""):
    try:
        for i in range(sleep_time):
            print(colored(
                f"Sleeping for {sleep_time - i} seconds while waiting for {msg}", "green"), end='\r', flush=True)
            time.sleep(1)
    except KeyboardInterrupt:
        print(colored("Keyboard interrupt detected, waking up", "red"))

def render_mongodb(members: int):
    environment = Environment(loader=FileSystemLoader(f"enterprise/templates/"))
    template = environment.get_template("mongodb-enterprise.yaml.jinja2")

    filename = f"enterprise/mongodb-enterprise.yaml"
    content = template.render(
        members=members,
    )
    with open(filename, mode="w", encoding="utf-8") as message:
        message.write(content)
    print(f"wrote {filename}")


def deploy(wait_time = 1000):
    os.system('kubectl apply -f enterprise/mongodb-enterprise.yaml')
    sleep(wait_time, "mongodb-enterprise to deploy")


def remove(wait_time = 5):
    os.system('kubectl delete -f enterprise/mongodb-enterprise.yaml')
    sleep(wait_time, "mongodb-enterprise to delete")

def print_nodes(log_file: str):
    v1 = client.CoreV1Api()
    log_file = f"logs/{log_file}"
    pods = v1.list_namespaced_pod("mongodb")
    cnt = 0
    for p in pods.items:
        if p.metadata.name.startswith("mongodb-enterprise-"):
            cnt += 1
            with open(log_file, "a") as outfile:
                outfile.write(f"{p.metadata.name}\n")
    cnt -= 1 # remove enterprise operator
    with open(log_file, "a") as outfile:
        print(f"Total nodes: {cnt}")
        outfile.write(f"Total nodes: {cnt}\n\n")


def run(args, cmd: str):
    os.system(
        f'kubectl exec -it mongosh-pod -it -- bash -c \'cd /usr/local/ycsb-0.17.0 && {cmd}\'')
    os.system(
        f'kubectl cp mongosh-pod:/usr/local/ycsb-0.17.0/outputRun.txt logs/logs1.txt')

    with open(f"logs/{args.logs}", "a") as outfile, open("logs/logs1.txt", "r") as infile:
        s = infile.read()
        print(s)
        outfile.write(f'Command: {cmd}')
        outfile.write(s)
        outfile.write("\n\n")


def main():
    parser = argparse.ArgumentParser(description='Run YCSB')
    parser.add_argument('-b', '--benchmark', type=str, choices=[
                        'workload', 'threads', 'member'], required=True, help='benchmark to run')
    parser.add_argument('-w', '--workload', nargs='+', type=str, choices=[
                        'a', 'b', 'c', 'd', 'e', 'f'], default=['a'], help='workload to run')
    parser.add_argument('-t', '--threads', nargs='+', type=int,
                        default=[1], help='number of threads')
    parser.add_argument('-n', '--times', type=int,
                        default=1, help='number of times to run')
    parser.add_argument('-l', '--logs', type=str,
                        default='outputRun.txt', help='log file path')
    parser.add_argument('-m', '--members', type=int, default=[3], nargs='+',
                        help='number of members in the replica set')

    args = parser.parse_args()

    os.system('kubectl config set-context --current --namespace=mongodb')

    for m in args.members:
        render_mongodb(m)
        deploy() 
        print_nodes(args.logs)
        for _ in range(args.times):
            for w in args.workload:
                for t in args.threads:
                    cmd = f'./bin/ycsb run mongodb -s -threads {t} -P workloads/workload{w} -p mongodb.url="{conn_str}" > outputRun.txt 2>&1'
                    run(args, cmd)


if __name__ == "__main__":
    main()
