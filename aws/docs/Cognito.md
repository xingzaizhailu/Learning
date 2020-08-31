## Amazon Cognito Section
Give users an identity so that they can interact with our application.

- cogonito User Pools:
    - Sign in functionality for app users
    - Integrate with API Gateway & Application Load Balancer
- Cognito Identity Pools (Federated Identity):
    - Provided AWS credentials to users so they can access AWS resources directly
    - Integrate with Cognito User Pools as an identity provider
- Cognito Sync:
    - Synchronise data from device to Cognito
    - Is deprecated and replaced by AppSync
- Cognito vs IAM: "hundreds of users", "mobile users", "authenticate with SAML"

### Cognito User Pools (CUP)

#### User Features

Create a serverless database of user for your web & mobile apps.

- Simple login: Username (or email) / password combination
- Password reset
- Email & Phone Number Verification
- Multi-factor authentication (MFA)
- Federated Identities: users from Facebook, Google, SAML...
- Feature: block users if their credentials are compromised elsewhere
- Login sends back a JSON Web Token (JWT)

#### Integrations

CUP integrates with API Gateway and Application Load Balancer.

#### Lambda Triggers

CUP can invoke a Lambda function synchronously on [many triggers](https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html).

#### Hosted Authentication UI

Cognito has a hosted authentication UI that can add to your app to handle sign-up and sign-in workflows.

- Using the hosted UI, you have a foundation for integration with social logins, OIDC or SAML.
- Can customize with a custom logo and custom CSS

### Cognito Identity Pools (Federated Identities)

Get identities for "users" so they obtain temporary AWS credentials.

- You identity pool (e.g. identity source) can include:
  - Public Providers (Login with Amazon, FB, Google, Apple)
  - Users in an Amazon Cognito user pool
  - OpenID Connect Providers & SAML Identity Providers
  - Developer Authenticated Identities (custom login server)
  - Cognito Identity Pools allow for unauthenticated (guest) access
- Users can then access AWS services directly or through API Gateway
  - The IAM policies applied to the credentials are defined in Cognito

#### IAM Roles

- Default IAM roles for authenticated and guest users
- Define rules to choose the role for each user based on the user's ID
- You can partition your users' access using policy variables
- IAM credentials are obtained by Cognito Identity Pools through STS
- The roles must have a "trust" policy of Cognito Identity Pools

##### Guest User example

``` json
{
  "Version": "2012-10-17",
  "Stagement": [
    {
      "Action": [
        "s3:GetObject"
      ],
      "Effect": "Allow",
      "Resource": [
        "arn:aws:s3:::mybucket/assets/my_picture.jpg"
      ]
    }
  ]
}
```

##### Policy variables on S3

```json
{
  "Version": "2012-10-17",
  "Stagement": [
    {
      "Action": ["s3:ListBucket"],
      "Effect": "Allow",
      "Resource": ["arn:aws:s3:::mybucket"],
      "Condition": {"StringLike": {"s3:prefix": ["${cognito-identity.amazonaws.com:sub}/*"]}}
    },
    {
      "Action": [
        "s3:GetObject",
        "s3:PubObject"
      ],
      "Effect": "Allow",
      "Resource": ["arn:aws:s3:::mybucket/${cognito-identity.amazonaws.com:sub}/*"]  
    }
  ]
}
```

### Cognito User Pools vs Cognito Identity Pools

- Cognito User Pools
  - Database of users for your web and mobile application
  - Allows to federate logins through Public Social, OIDC, SAML...
  - Can customize the hosted UI for authentication (including the logo)
  - Has triggers with AWS Lambda during the authentication flow
- Cognito Identity Pools:
  - Obtain AWS credentials for your users
  - Users can login through Public Social, OIDC, SAML & Cognito User Pools
  - Users can be unauthenticated (guests)
  - Users are mapped to IAM roles & policies, can leverage policy varialbles
- CUP + CIP = manager user / password + access AWS services

### Cognito Sync

- **Deprecated** - use AWS AppSync now
- Store preferences, configuration, state of app
- Cross device synchronization (any platform - iOS, Android, etc...)
- Offline capability (synchronization when back online)
- Store data in datasets (up to 1 MB), up to 20 datasets to synchronize
- Push Sync: silently notify across all devices when identity data changes
- Cognito Stream: stream data from Cognito into Kinesis
- Cognito Events: execute Lambda functions in response to events