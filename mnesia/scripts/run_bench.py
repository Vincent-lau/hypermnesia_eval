#!/usr/bin/env python3

from jinja2 import Environment, FileSystemLoader
import os
import argparse
import time
from enum import Enum
from termcolor import colored


# ======================== bench config ========================

n_replicas = 3
bench_top = "lib/mnesia/examples/bench"

table_nodes = [
    f'bench@erl-cluster-{i}.erl-cluster-svc.hypermnesia.svc.cluster.local' for i in range(0, n_replicas)]
default_params = {
    'start_module': 'pod',
    'partition_time': '0',
    'activity': 'async_ec',
    'generator_profile': 'random',
    'rw_ratio': 0.5,
    'statistics_detail': 'debug',
    'generator_warmup': 12000,
    'generator_duration': 90000,
    'generator_cooldown': 12000,
    'generator_nodes': table_nodes,
    'n_generators_per_node': 1,
    'table_nodes': table_nodes,
    'n_replicas': n_replicas,
    'n_subscribers': 500,
}


def change_nodes(n_replicas: int, n_generator_nodes: int) -> dict():
    table_nodes = [
        f'bench@erl-cluster-{i}.erl-cluster-svc.hypermnesia.svc.cluster.local' for i in range(0, n_replicas)]
    params = default_params.copy()
    params['table_nodes'] = table_nodes
    params['generator_nodes'] = table_nodes[:n_generator_nodes]
    params['n_replicas'] = n_replicas

    return params


def change_generators(n_replicas: int, n_generators: int) -> dict():
    params = default_params.copy()
    params['n_generators_per_node'] = n_generators

    table_nodes = [
        f'bench@erl-cluster-{i}.erl-cluster-svc.hypermnesia.svc.cluster.local' for i in range(0, n_replicas)]
    generator_nodes = []
    n_generators_per_node = 1
    if n_generators < n_replicas:
        generator_nodes = table_nodes[:n_generators]
    elif n_generators % n_replicas == 0:
        n_generators_per_node = n_generators // n_replicas
        generator_nodes = table_nodes
    else:
        generator_nodes = [table_nodes[0]]
        n_generators_per_node = 1
    
    params['table_nodes'] = table_nodes
    params['generator_nodes'] = generator_nodes
    params['n_generators_per_node'] = n_generators_per_node

    return params

def change_activity(activity: str) -> dict():
    params = default_params.copy()
    params['activity'] = activity

    return params


def change_subscribers(n_subscribers: int) -> dict():
    params = default_params.copy()
    params['n_subscribers'] = n_subscribers

    return params


def change_profile(profile: str, ratio=0.0) -> dict():
    params = default_params.copy()
    params['generator_profile'] = profile
    if profile == 'rw_ratio':
        params['rw_ratio'] = ratio
    return params


def gen_config(params):

    environment = Environment(loader=FileSystemLoader(f"deploy/templates/"))
    template = environment.get_template("bench.config.jinja2")

    filename = f"deploy/bench0.config"
    content = template.render(
        start_module=params['start_module'],
        partition_time=params['partition_time'],
        activity=params['activity'],
        generator_profile=params['generator_profile'],
        rw_ratio=params['rw_ratio'],
        generator_warmup=params['generator_warmup'],
        generator_duration=params['generator_duration'],
        generator_cooldown=params['generator_cooldown'],
        statistics_detail=params['statistics_detail'],
        generator_nodes=params['generator_nodes'],
        n_generators_per_node=params['n_generators_per_node'],
        table_nodes=params['table_nodes'],
        n_replicas=params['n_replicas'],
        n_subscribers=params['n_subscribers'],
    )
    with open(filename, mode="w", encoding="utf-8") as message:
        message.write(content)
        print(f"... wrote {filename}")

# ======================== cluster config ========================


def render_cluster_config(replicas: int):
    environment = Environment(loader=FileSystemLoader("deploy/templates"))
    template = environment.get_template("erl-cluster.yaml.jinja2")
    with open('deploy/erl-cluster.yaml', 'w') as out_file:
        content = template.render(
            replicas=replicas,
        )
        out_file.write(content)


def remove(wait_time=5):
    os.system('kubectl delete -f deploy/erl-cluster.yaml --ignore-not-found')
    print('Waiting for cluster to be deleted')
    time.sleep(wait_time)


def deploy_cluster(wait_time=30):
    os.system('kubectl apply -f deploy/erl-cluster.yaml')
    print('Waiting for cluster to be deployed')
    time.sleep(wait_time)


def run_in_cluster(args: argparse.Namespace):
    exe_str = f'{bench_top}/bench.sh bench0.config | tee -a logs/{args.log}'
    os.system(f'kubectl exec erl-cluster-0 -it -- {exe_str}')


def copyout(args: argparse.Namespace):
    filename = args.log
    os.system(
        f'kubectl cp erl-cluster-0:{bench_top}/{filename} logs/{filename}')


def copyin():
    # copy the bench script to the cluster
    os.system(f'kubectl cp deploy/bench.sh erl-cluster-0:{bench_top}/bench.sh')
    # copy the bench config to the cluster
    os.system(
        f'kubectl cp deploy/bench0.config erl-cluster-0:{bench_top}/bench0.config')


class Benchmark(Enum):
    WORKLOAD = 1
    ACTIVITY = 2
    SUBSCRIBERS = 3
    NODES = 4
    GENERATORS = 5


def render_bench_config(bench_type: Benchmark, *vals):
    if bench_type == Benchmark.NODES:
        params = change_nodes(vals[0], vals[1])
    elif bench_type == Benchmark.ACTIVITY:
        params = change_activity(vals[0])
    elif bench_type == Benchmark.SUBSCRIBERS:
        params = change_subscribers(vals[0])
    elif bench_type == Benchmark.WORKLOAD:
        params = change_profile('rw_ratio', vals[0])
    elif bench_type == Benchmark.GENERATORS:
        params = change_generators(vals[0], vals[1])
    gen_config(params)


def sleep(sleep_time: int, msg=""):
    try:
        for i in range(sleep_time):
            print(colored(
                f"Sleeping for {sleep_time - i} seconds while waiting for {msg}", "green"), end='\r', flush=True)
            time.sleep(1)
    except KeyboardInterrupt:
        print(colored("Keyboard interrupt detected, waking up", "red"))


def run(args):
    # now render the corresponding cluster config, e.g. number of nodes need to match
    render_cluster_config(replicas=3)

    # need to deploy the cluster now
    remove()
    deploy_cluster(60)

    # then copy in bench.sh and bench0.config
    copyin()

    run_in_cluster(args)

    sleep(5, "cooling down")


def main():
    parser = argparse.ArgumentParser(
        prog='benchmark',
        description='Run mnesia benchmark inside k8s cluster',
        epilog='epilog'
    )

    parser.add_argument('-l', '--log', help='log file name',
                        default='log.txt', type=str)
    parser.add_argument('-b', '--benchmark', help='different types of benchmark',
                        type=str, choices=[
                            'workload', 'activity', 'nodes', 'generators', 'subscribers'],
                        required=True)
    parser.add_argument('-bc', '--benchmark-value', help='value for benchmark',
                        nargs='+', type=int, required=True)
    args = parser.parse_args()

    if args.benchmark == 'workload':
        for i in args.benchmark_value:
            render_bench_config(Benchmark.WORKLOAD, i * 0.1)
    elif args.benchmark == 'nodes':
        for i in args.benchmark_value:
            render_bench_config(Benchmark.NODES, i, 1)
    elif args.benchmark == 'generators':
        for i in args.benchmark_value:
            render_bench_config(Benchmark.NODES, 3, i)
    elif args.benchmark == 'subscribers':
        for i in args.benchmark_value:
            render_bench_config(Benchmark.SUBSCRIBERS, i)
    elif args.benchmark == 'activity':
        for i in args.benchmark_value:
            render_bench_config(Benchmark.ACTIVITY, i)

    run(args)


if __name__ == "__main__":
    main()
