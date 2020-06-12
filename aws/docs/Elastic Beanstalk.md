## Elastic Beanstalk

Beanstalk handles instance configuration and OS. The Beanstalk itself is free, you pay for resources created behind it.

Three architecture models:

- Single instance deployment: good for dev
- Load balancer + Auto-scaling Group for production or pre-production web apps
- ASG only: great for non-web apps in production (workers, etc...)

### Components

1. Application
2. Application Version
3. Environment name (dev, test, prod): free naming

You deploy application versions to environments and can promote application versions to the next environment.

