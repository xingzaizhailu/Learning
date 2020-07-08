## Elastic Block Store (EBS) Volume

An EC2 machine loses its root volume when it is terminated manually or unexpectedly. An EBS Volume is a **network** drive you can attach to your instances while they run. It allows your instances to persist data.

- Network drive (not physical)
  - Uses the network to communicate the instance - a bit of latency
  - Can be detached from an EC2 instance and attached to another one quickly
- It locked to an AZ
  - To move a volume across, you first need to snapshot it and restore it in another AZ
  - EBS backups use IO and you shouldn't run them while your application is handling a lot of traffic.
- Have a provisioned capacity (size in GBs, and IOPS)
  - Get billed for all the provisioned capacity
  - Capacity can be increased over time
- Root EBS Volumes of instances get terminated by default if the EC2 instance get terminated.
  - You can disable that

### EBS Volume Types

EBS Volumes are characterised in Size/ Throughput / IOPS (I/O Ops Per Sec)

- GP2 (SSD): ***General purpose** SSD* volume that balances price and performance for a wide variety of workloads
  - Recommended for most workloads
    - System boot volumes
    - Virtual desktops
    - Low-latency interactive apps
    - Development and test envs
  - 1 GiB - 16 TiB
  - Min IOPS is 100, Max IOPS is 16, 000 (3 IOPS per GB, mens at 5334GB we are at the max IOPS)
  - Small GP2 volumes (< 1 TiB) can burst IOPS to 3000
- IOI (SSD): *Highest-performance SSD* volume for mission-critical low-latency or high-throughput workloads
  - Critical business applications that require sustained IOPS performance, or more than 16,000 IOPS per volume (GP2 limit)
    - Large database workloads
  - 4 GiB - 16 TiB
  - IOPS is provisioned (PIOPS) - Min 100 - Max 64,000 (Nitro instances) else Max 32,000 (other instances)
  - The max ratio of provisioned IOPS to requested volume size (in GiB) is 50:1
- STI (SSD): *Low cost HDD* volume designed for frequently accessed, throughput-intensive workloads
  - Streaming workloads requiring consistent, fast throughput at a low price.
    - Big data, Data warehouses, Log processing
    - Apache Kafka
  - Cannot be a boot volume
  - 500 GiB - 16 TiB
  - Max IOPS is 500
  - Max throughput of 500 MiB/s - can burst
- SCI (SSD): *Lowest cost HDD* volume designed for less frequently accessed workloads
  - Throughput-oriented storage for large volumes of data that is infrequently accessed
    - Scenarios where the lowest storage cost is important
  - Cannot be a boot volume
  - 500 GiB - 16 TiB
  - Max IOPS is 250
  - Max throughput of 250 MiB/s - can burst

**Note:** Only GP2 and IOI can be used as boot volumes

#### EBS vs Instance Store

Some instance don't come with Root EBS volumes. Instead, they come with "Instance Store" (= ephemeral storage). Instance store is *physically* attached to the machine. (EBS is a network drive)

- Disks up to 7.5 TiB
- Block Storage (just like EBS)
- Risk of data loss if hardware fails

**Pros:**

- Better I/O performance (very high IOPS)
- Good for buffer/ cache/ scratch data/ temporary content
- Data survives reboots

**Cons:**

- On stop or termination, the instance store is lost
- You can't resize the instance store
- Backups must be operated by the user

## EFS - Elastic File System

Managed NFS (Network File System) that can be mounted on many EC2 even accross multi-AZ. It is highly available, scalable, expensive (3x gp2), pay per use.

- Use cases: Content management, web serving, data sharing.

- Uses NFSv4.1 protocol
- Uses security group to control access to EFS
- Compatible with Linux based AMI (not Windows)
- Encryption at rest using KMS
- POSIX file system (~Linux) that has a standard file API
- File system scales automatically, pay-per-use, no capacity planning

### EFS - Performance & Storage Classes

#### EFS Scale

- 1000s of concurrent NFS clients, 10 GB+/s throughput
- Grow to Petabyte-scale network file system, automatically

#### Performance mode

Set at EFS creation time:

- General purpose (default): latency-sensitive use cases (web server, CMS, etc...)
- Max I/O - high latency, throughput, highly parallel (big data, media processing)

#### Storage Tiers (lifecycle management feature - move file after N days)

- Standard: for frequently accessed files
- Infrequent access (EFS-IA): cost to retrieve files, lower price to store