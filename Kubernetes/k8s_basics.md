## Kubectl

Checking: `$ kubectl version`, should have versions for both client and server.

### run, create and apply

There are three ways to create pods from the kubectl CLI.

- kubectl run
  - Create a Deployment before 1.18 (which creates a ReplicaSet, which creates a Pod)
  - Changing to be only create a single Pod after v1.18
- kubectl create (create some resources via CLI or YAML)
- kubectl apply (create/ update anything via YAML)

### Creating Pods with kubectl

Two ways:

- Via commands
- or via YAML

#### Via commands

``` sh
# before 1.18
$ kubectl run my-nginx --image nginx
# Creating a Deployment in 1.18
$ kubectl create deployment nginx --image nginx
$ kubectl get pods
$ kubectl get all
$ kubectl delete deployment my-nginx
```

### Scaling ReplicaSets

``` sh
# Start a new deployment for one replica/pod
$ kubectl run my-apache --image httpd
$ kubectl scale deploy/my-apache --replicas 2
# or (deploy = deployment = deployments)
$ kubectl scale deployment my-apache --replicas 2
```

so what happened is:

1. Deployment updated to 2 replicas
2. ReplicaSet Controller sets pod count to 2
3. Control Plane Assigns node to pod
4. Kubelet sees pod is needed, starts container

### Inspecting Deployment Objects

``` sh
$ kubectl get pods

# Get container logs
$ kubectl logs deploy/my-apache 										# can only return logs of 1 pod
$ kubectl logs deploy/my-apache --follow --tail 1   # return last one's logs

# Get a bunch of details about an object, including events filtered by label
$ kubectl logs -l run=my-apache		# or app=my-apache

$ kubectl describe pod/my-apache-xxxx-yyyy

# Watch a command (without needing watch)
$ kubectl get pods -w
# In a seperate tab/window
$ kubectl delete pod/my-apache-xxxx-yyyy

# Cleanup
$ kubectl delete deploy/my-apache
```

Note: Lookup the Stern tool for better log tailing

## Services

### Exposing Containers

- ` kubectl expose` creates a **service** for existing pods.
- A **service** is a stable address for pod(s).
- CoreDNS allows us to resolve services by name
- Different types of services
  - ClusterIP
  - NodePort
  - LoadBalancer
  - ExternalName

### Basic Service Types

- ClusterIP (default)
  - Single, internal virtual IP allocated
  - Only reachable from within cluster (nodes and pods)
  - Pods can reach service on apps port number
- NodePort
  - High port allocated on each node
  - Port is open on every node's IP
  - Anyone can connect (if they can reach node)
  - Other pods need to be updated to this port
- LoadBalancer
  - Controls a LB endpoint external to the cluster
  - Only available when infra provider gives you a LB (AWS ELB, etc)
  - Creates NodePort+ClusterIP services, tells LB to send to NodePort
- ExternalName
  - Adds CNAME DNS record to CoreDNS only
  - Not used for Pods, but for giving pods a DNS name to use for something outside Kubernetes

#### Creating a ClusterIP Service