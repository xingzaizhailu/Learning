## AWS CloudFormation

### Template

#### Template components:

- Version
- Description
- Metadata: a JSON or YAML file
- Parameters: The dynamic inputs for the templates/ stacks
- Mapping: The static variables for the template (like map a region to it's key-pair)
- **Resources:** Your AWS resources declared in the template (Mandatory)
- Condition: List of conditions to perform resource creation (like dev, test)
- Transform
- Outputs: References to what was created (for logging or for other stacks to use)

#### Template helpers

- References
- Functions:

### YAML

- Key value pairs
- Nested objects: indentation
- Arrays supported: `-` symbol
- Multi line strings: `|` symbol
- Comments: `#` symbol

### Resources

Resource types identifiers are of the form: `AWS::aws-product-name::data-type-name`.

[All AWS resources](https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html)

#### Resource Reference and Dependency

- `DependsOn` -  one will resource be created after another

- `Fn::Ref` function can be leveraged to reference parameters or resources

  - The shorthand for this in YAML is `!Ref`

  - ```
    DbSubnet1:
    	Type: AWS::EC2::Subnet
    	Properties:
    		VpcId: !Ref MyVPC
    ```

- Fn::GetAttr

### Parameters Settings

Use parameters when the value is user specific or values can't know before hand.

Parameters can be controlled by all these settings:

- Type:
  - string
  - Number
  - CommaDelimitedList
  - List\<Type\>
  - AWS Parameter (to help catch invalid values - match against existing values in the AWS Account)
- Description
- Constraints
- ConstraintDescription (String)
- Min/ MaxLength
- Min/ MaxValue
- Defaults
- AllowedValues (array)
- AllowedPattern (regexp)
- NoEcho (Boolean)

### Mappings

Very handy to differentiate between different environments (dev, prod), region, AMI types, etc.

Format:

```
Mappings:
	Mapping01:
		Key01:
			Name: Value01
		Key02:
			Name: Value02
		Key03:
			Name: Value03
```

#### Use `Fn::FindInMap` to return a named value from a specific key

`!FindInMap [MapName, TopLevelKey, SecondLevelKey]`

### Outputs

You can't delete a CloudFormation Stack if its outputs are being referenced by another CloudFormation stack.

e.g. Creating a SSH Security Group as part of one template:

```
Outputs:
	StackSSHSecurityGroup:
		Description: The SSH Security Group for our Company
		Value: !Ref MyCompanyWideSSHSecurityGroup
		Export:
			Name: SSHSecurityGroup
```

#### Cross Stack Reference - `Fn::ImportValue` function 

 Create another template that leverages that security group:

```
Resources:
	MySecurityInstance:
		Type: AWS::EC2::Instance
		Properties:
			AvailabilityZone: us-east-1a
			ImageId: ami-a4c7edb2
			InstanceType: t2.micro
			SecurityGroups:
				- !ImportValue SSHSecurityGroup
```

### Conditions

To control the creation of resources or outputs based on a condition. They can be:

- Environment (dev/ test/ prod)
- AWS Region
- Any parameter value

Each condition can reference another condition, parameter value or mapping.

#### Define a codition

```
Conditions:
	# `CreateProdResources` will be true if `EnvType` is equal to 'prod'
	CreateProdResources: !Equals [!Ref EnvType, prod]
```

The intrinsic function (logical) can be:

- Fn::And
- Fn::Equals
- Fn::If
- Fn::Not
- Fn::Or

#### use a condition

```
Resources:
	MountPoint:
		Type: "AWS::EC2::VolumeAttachment"
		Condition: CreateProdResources
```

### Must know intrinsic functions

- Ref - Reference parameters, resources
- Fn::GetAtt
  - Attributes are attached to any resources you created
  - Could be used like: create a volume at the same region as an instance by get its `AZ` attribute
- Fn::FindInMap
  - `!FindInMap [MapName, TopLevelKey, SecondLevelKey]`
- Fn::ImportValue
- Fn::Join - Join values with a delimiter
  - Format: `!Join [delimiter, [comma-delimited list of values]]`
  - e.g. creates "a:b:c": `!Join [ ":", [ a, b, c ] ]`
- Fn::Sub - Substitute variables from a text
- Condition Functions (Fn::If, Fn::Not, Fn::Equals, etc...)

