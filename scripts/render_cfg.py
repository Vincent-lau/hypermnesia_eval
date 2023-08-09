#!/usr/bin/env python3


from jinja2 import Environment, FileSystemLoader
import os



def render_cfg(num_nodes: int):
  environment = Environment(loader=FileSystemLoader("otp/lib/mnesia/examples/bench/"))
  template = environment.get_template("bench.config.jinja2")
  with open('otp/lib/mnesia/examples/bench/bench.config', 'w') as f:
      table_nodes = [f'bench@erl-cluster-{i}.erl-cluster-svc.hypermnesia.svc.cluster.local' for i in range(num_nodes)]
      content = template.render(
        table_nodes = table_nodes,
      )

      f.write(content)

def render_manifest(num_nodes: int):
  env = Environment(loader=FileSystemLoader("mnesia_bench/k8s/"))
  template = env.get_template("erl-cluster.yaml.jinja2")
  with open('mnesia_bench/k8s/erl-cluster.yaml', 'w') as f:
      content = template.render(
        repliacs = num_nodes,
      )
      f.write(content)




render_cfg(13)
render_manifest(13)
