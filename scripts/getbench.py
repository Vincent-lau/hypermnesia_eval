#!/usr/bin/env python3

from kubernetes import client, config
import os

# Configs can be set in Configuration class directly or using helper utility
config.load_kube_config()

v1 = client.CoreV1Api()
print("Listing pods with their IPs:")
ret = v1.list_namespaced_pod(watch=False, namespace='resolverl')
for i in ret.items:
    if i.metadata.name.startswith("mnesia-bench"):
      print(f"{i.metadata.name} {i.status.pod_ip}")
      os.system(f"kubectl logs -f {i.metadata.name} -n resolverl | tee -a mnesia_bench/scratch2.txt")
