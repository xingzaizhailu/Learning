## VPC

VPC: Virtual private cloud

Subnets: Tied to an AZ, network partition of the VPC

Internet Gateway: at the VPC level, provide to private subnets

NAT Gateway / Instances: give internet access to private subnets

NACL: Stateless, subnet rules for inbound and outbound

Security Groups: Stateful, operate at the EC2 instance level or ENI

VPC peering: connect two VPC with non overlapping IP ranges, non transitive

VPC Endpoints: Provide private access to AWS Services within VPC

VPC Flow Logs: network traffic logs

Site to Site VPN: VPN over public internet between on-premises DC and AWS

Direct Connect: direct private connections to a AWS