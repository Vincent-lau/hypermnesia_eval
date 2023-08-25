#!/usr/bin/env python3

import argparse
import os
from jinja2 import Environment, FileSystemLoader
from kubernetes import client, config
from termcolor import colored
import time


config.load_kube_config()
fn = "deploy/my-postgres.yaml"

def sleep(sleep_time: int, msg=""):
    try:
        for i in range(sleep_time):
            print(colored(
                f"Sleeping for {sleep_time - i} seconds while waiting for {msg}", "green"), end='\r', flush=True)
            time.sleep(1)
    except KeyboardInterrupt:
        print(colored("Keyboard interrupt detected, waking up", "red"))


def render_postgres(replicas: int):
    environment = Environment(
        loader=FileSystemLoader(f"deploy/templates/"))
    template = environment.get_template("my-postgres.yaml.jinja2")

    filename = fn
    content = template.render(
        replicas=replicas
    )
    with open(filename, mode="w", encoding="utf-8") as message:
        message.write(content)
    print(f"wrote {filename}")


def deploy(wait_time=100):
    os.system(f'kubectl apply -f {fn}')
    sleep(wait_time, "postgresql to deploy")


def remove(wait_time=5):
    os.system(f'kubectl delete -f {fn}')
    sleep(wait_time, "postgresql to delete")


def print_nodes(log_file: str):
    v1 = client.CoreV1Api()
    log_file = f"logs/{log_file}"
    pods = v1.list_namespaced_pod("postgresql")
    cnt = 0
    for p in pods.items:
        if p.metadata.name.startswith("postgresql-ha"):
            cnt += 1
            with open(log_file, "a") as outfile:
                outfile.write(f"{p.metadata.name}\n")
    cnt -= 1
    with open(log_file, "a") as outfile:
        print(f"Total nodes: {cnt}")
        outfile.write(f"Total nodes: {cnt}\n\n")

def copyin():
    os.system('kubectl cp postgrenosql.properties postgres-sh:YCSB/postgrenosql/conf/postgrenosql.properties')
    os.system('kubectl cp workloads postgres-sh:YCSB/')

def run(args, cmd: str):
    os.system(
        f'kubectl exec -it postgres-sh -it -- bash -c \'cd /YCSB && {cmd}\'')
    os.system(
        f'kubectl cp postgres-sh:/YCSB/outputRun.txt logs/logs1.txt')

    with open(f"logs/{args.logs}", "a") as outfile, open("logs/logs1.txt", "r") as infile:
        s = infile.read()
        print(s)
        outfile.write(f'Command: {cmd}')
        outfile.write(s)
        outfile.write("\n\n")


def main():
    parser = argparse.ArgumentParser(description='Run YCSB')
    parser.add_argument('-b', '--benchmark', type=str, choices=[
                        'workload', 'threads', 'replicas'], required=True, help='benchmark to run')
    parser.add_argument('-w', '--workload', nargs='+', type=str, choices=[
                        'a', 'b', 'c', 'd', 'e', 'f'], default=['a'], help='workload to run')
    parser.add_argument('-t', '--threads', nargs='+', type=int,
                        default=[1], help='number of threads')
    parser.add_argument('-n', '--times', type=int,
                        default=1, help='number of times to run')
    parser.add_argument('-l', '--logs', type=str,
                        default='outputRun.txt', help='log file path')
    parser.add_argument('-r', '--replicas', type=int, default=[3], nargs='+',
                        help='number of replicas in the replica set')

    args = parser.parse_args()

    os.system('kubectl config set-context --current --namespace=postgresql')

    for m in args.replicas:
        render_postgres(m)
        if args.benchmark == 'replicas':
            deploy()
        copyin()
        print_nodes(args.logs)
        for _ in range(args.times):
            for w in args.workload:
                for t in args.threads:
                    cmd = f'./bin/ycsb run postgrenosql -s -threads {t} -P workloads/workload{w} -P postgrenosql/conf/postgrenosql.properties  > outputRun.txt 2>&1'
                    run(args, cmd)


if __name__ == "__main__":
    main()
