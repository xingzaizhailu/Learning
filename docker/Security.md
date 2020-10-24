## Security

### [Top 10 tips](https://github.com/BretFisher/ama/issues/17)

1. Just use Docker

2. Scan your hosts for proper Docker config

3. Don't Expose the Docker TCP Socket to the Internet

4. Don't run apps in containers as root. (Often there is already a `USER` created for you.)

   ``` dockerfile
   RUN groupadd --gid 1000 groupName \
   	&& useradd --uid 1000 --gid groupName --shell /bin/bash --create-home groupName
   ```

5. Enable "user namespaces". Run docker with a user instead of root on host.

6. Code Repo scanning as early as possible.

   - Snyk

7. Image scanning

   - Trivy
   
8. Runtime Bad Behaviors

   - Sysdig Falco

9. Content Trust

   - Only images signed by your teams are allowed

10. Later, check out AppArmor, SELinux, Seccomp, and Linux "capabilities"

11. Docker root-less

    - An option for running the docker daemon as a normal user on the host
    - https://get.docker.com/rootless
