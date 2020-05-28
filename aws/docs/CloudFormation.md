## AWS CloudFormation

### Template

Keywords:

- Version
- Description
- Metadata: a JSON or YAML file
- Parameters: parameters for stacks
- Mapping: like map a region to it's key-pair
- **Resource**
- Condition: like dev, test
- Transform
- Outputs: for logging or for other stacks to use

### Resource Reference and Dependency

- Depends on -  one will resource be created after another
- Ref
- Fn::GetAttr

### 