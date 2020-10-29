## Kubernetes

Problem Space:

- Modern Application Architecture
- Loss of microservices
- Lots and lots of containers
- Interconnected services
- Distributed Operations and security
- Under utilised infrastructure

### Terms

- Kubernetes: The whole orchestration system
  - K8s "k-eights" or Kube for short
- Kubectl: CLI to configure Kubernetes and manage apps
  - Using "cube control" official pronunciation
- Node: Single server in the Kubernetes cluster
  - Runs Kubelet and Kube-proxy inside
- Kubelet: Kubernetes agent running on nodes
- Control Plane: Set of containers that manage the cluster
  - Sometimes called the "master"
  - Includes API server, scheduler, controller manager, etcd, and more

### Concepts

- Pod
  - One or more containers running together on one node
  - Basic unit of deployment. Containers are always in pods
- Controller
  - For creating/ updating pods and other objects
  - Many types of Controllers inc. Deployment, ReplicaSet, StatefulSet, DaemonSet, Job, CronJob, etc.
- Service
  - Network endpoint to connect to a pod
  - Virtual IP for pods (containers of the service spreads in multiple pods)
  - Load balancing
  - Service discovery via DNS
  - Find pods by label selectors
- Ingress
  - Expose HTTP, HTTPS routes
  - TLS termination
  - Path based matches
  - Load balancing
  - Requires Ingress Controller
- Namespaces
  - Virtual cluster on actual cluster/ Filtered group of objects in cluster
  - Resource isolation via quotas and limits
  - Ability to divide a cluster amongst multiple teams
  - Policy enforcement point
- RBAC
  - AWS IAM like controls
  - Authorization framework
  - Facilitates least privilege principle
  - Namespace + RBAC = team sandboxes