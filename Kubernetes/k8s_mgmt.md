## Kubernetes Management Techniques

Generators

Imperative vs. Declarative

### Run, Create and Expose Generators

- These commands use helper templates called "generators".

- Every resource in Kubernetes has a specification or "spec"

  - ``` sh
    $ kubectl create deployment sample --image nginx --dry-run -o yaml
    ```

  - You can use those YAML defaults as a starting point

- Generators are "opinionated defaults"

#### Generator Examples

``` sh
$ kubectl create deployment test --image nginx --dry-run # output type of resource will be created
$ kubectl create deployment test --image nginx --dry-run -o yaml
$ kubectl create job test --image nginx --dry-run -o yaml # used when you want pod to run once
$ kubectl expose deployment/test --port 80 --dry-run -o yaml # You need the deployment to exist before this works
```

### Imperative vs. Declarative

Imperative: Focus on how a program operates

Declarative: Focus on what a program should accomplish

#### Kubernetes Imperative

- Examples: `Kubectl run`, `kubectl create deployment`, `kubectl update`
  - We start with a state we know (no deployment exists)
  - We ask kubectl run to create a deployment
- Different commands are required to change that deployment
- Different commands are required per object
- Imperative is easier when you know the state
- Imperative is easier to get started
- Imperative is easier for humans at the CLI
- Imperative is NOT easy to automate

#### Kubernetes Declarative

- Example: `kubectl aply -f my-resources.yaml`
  - We don't know the state
  - We only know what we want the end result to be (yaml contents)
- Same command each time (tiny exception for delete)
- Resources can be all in a file, or many files (apply a whole dir)
- Requires understanding the YAML keys and values
- More work than `kubectl run` for just starting a pod
- The easiest way to automate

### Three Management Approaches

1. Imperative commands: `run`, `expose`, `scale`, `edit`, `create deployment`
   - Best for dev/learning/personal projects
   - Easy to learn, hardest to manage over time
2. Imperative objects: `create -f file.yml`, `replace -f file.yml`, `delete`...
   - Good for prod of small environments, single file per command
   - Store your changes in git-based yaml files
   - Hard to automate
3. Declarative objects: `apply -f file.yml` or `dir\`, `diff`
   - Best for prod, easier to automate
   - Harder to understand and predict changes

#### Most Important Rule

- Don't mix the three approaches
- Recommendations
  - Learn the Imperative CLI for easy control of local and test setups
  - Move to `apply -f file.yml` and `apply -f directory\` for prod
  - Store yaml in git, git commit each change before you apply
  - This trains you for later doing GitOps (where git commits are automatically applied to clusters)

