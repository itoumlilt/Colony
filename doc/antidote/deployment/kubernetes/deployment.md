# General

Examples on how to deploy Antidote via Kubernetes can be found [here](examples.md).

## Minikube

Minikube simulates a local kubernetes-cluster containing a single physical node. It is therefore good for testing purposes.

1. Install [Kubectl](https://kubernetes.io/docs/tasks/tools/install-kubectl/)
1. Install [Minikube](https://kubernetes.io/docs/tasks/tools/install-minikube/)

### Configuration Kubectl

Configure kubectl to use minikube as its context, if minikube did not already do it automatically. Use `kubectl config view` to lookup the config file. The file itself can by default be found at:
- linux: ~/.kube/config
- windows: C:/user/\<userX\>/.kube/config

### Configuration Minikube
The configuration file can by default be found here:
- linux: ~/.minikube/config/config.json
- windows: C:/user/\<userX\>/.minikube/config/config.json  

Configure minikube to make it more suitable for testing to deploy Antidote. The following flags might be useful:
- insecure registries: start minikube with the insecure-registry flag, e.g.: `minikube start --insecure-registry="hostname:port"`.
- give minikube more cpus and memory to use, by using the flags --cpus, --memory, e.g.: `minikube start --cpus 6 --memory 8192`. You can also edit the configuration file to add these flags to every `minikube start` command.

Note that you have to restart (delete, start) minikube to make these configuration changes valid. `minikube stop` is not sufficient.

## Deployment Schema: Example

**Requirements:**
- Kubernetes Version 1.16 and higher (The startup probe is only available for versions 1.16 and higher).
- Antidote Version 0.2.0 and higher (This schema was tested for versions 0.2.0 and 0.2.2; refer to the example in the example section).

In this deployment schema for Antidote on a kubernetes-cluster we need the following Kubernetes objects:

For the kubernetes objects that will define the antidote data center: 
- [HeadlessService](#headlessService)
- [StatefulSet](#statefulSet)
    - [ContainerTemplate](#containerTemplate)
    - [VolumeClaimTemplates](#volumeClaimTemplates)
- [Service](#services) for exposing the datacenter to outside of the cluster

And some jobs:
- [Jobs](#jobs)  

This jobs will be used for example to issue respectively the calls `createDC` and `connectDCs` to the antidote cluster.

The following stated **properties** are mostly only basic properties needed to deploy antidote. There are many more ways to configure/plugin kubernetes to fit someones needs.

### HeadlessService

A [StatefulSet](https://kubernetes.io/docs/concepts/workloads/controllers/statefulset/) needs to define a [HeadlessService](https://kubernetes.io/docs/concepts/services-networking/service/#headless-services) to work properly.  
An example how a yaml specification may look like can be found here: [headlessService.yaml](example_yaml_templates/headlessService.yaml).
The HeadlessService provides the StatefulSet's Pods with a Subdomain. Each Pod may be accessible inside the cluster through this domain.

Properties of the *HeadlessService*:
- The **clusterIP** field is always specified as **None**.
- A **name** should be specified in the **metadata**.
- The **selector** should select the pods of refered *StatefulSet*.

### StatefulSet

The [StatefulSet](https://kubernetes.io/docs/concepts/workloads/controllers/statefulset/) gives some guarantees such that after a restart, [Pods](https://kubernetes.io/docs/concepts/workloads/pods/pod/) will still have the same network identifier.
This is a property you would like to have for stateful applications. As Antidote is a stateful application we will choose the *StatefulSet* as our replication controller.  
An example how a yaml specification may look like can be found here: [statefulSet.yaml](example_yaml_templates/statefulSet.yaml).  

Here we will model each StatefulSet such that it consists of some pods, which are actually all antidote instances of one data center.
First the StatefulSet might be applied to the cluster, then scaled to the specified number of replicas and then we migth create a job, that calls the antidote api to create a data center from all pods contained in this *StatefulSet*.

Properties of the *StatefulSet*:
- The **name** of the *StatefulSet* in **metadata**.
- The **serviceName** field has to be the **name** specified in the *HeadlessService*.
- The number of **replicas** generated and governed by this *StatefulSet*
- The **selector** should select all pods of antidote instances that you would like to have in one data center.
- The **template** as specified in [ContainerTemplate](#containerTemplate).
- The **volumeClaimTemplates** as specified in [VolumeClaimTemplates](#volumeClaimTemplates)

#### ContainerTemplate
In the **template** spec you may define how the pods, that are part of the *StatefulSet* shall look like.
In our case we want it to contain at least the antidote container.

Properties of the *template*:
- Some **labels** to make sure we can select the here created pods somewhere else (e.g. in the selector of the *StatefulSet* above).
- A **terminationGracePeriodSeconds** to make kubernetes wait the specified time before shutting down a pod, to allow a implemented graceful termination of each pod.
- Some **containers:**
	- A container with the **image** of an antidote release.
	- Open some **ports** that are needed from the antidote image internally (which means inside the kubernetes cluster they will then be accessible; generally only the protobuffer port at 8087 is needed).
	- Some **resources** that the pod may use. Make sure to give the pod access to enough resources, so antidote may work properly.
	- A **startupProbe**. The one specified in the [example](example_yaml_templates/statefulSet.yaml) checks wether the port at antidote-pb is opened for tcp or not. For the probe to be successful, the port has to be opened for two consecutive calls. After 60 failures the pod will be marked as 'not ready' and the probe will not be continued.   
	- Some **env** variables. In the [example](example_yaml_templates/statefulSet.yaml) the NODE_NAME is set to "antidote@$(HOST_NAME).serviceName" (where serviceName is the specified name of the headlessService as in the spec-field 'serviceName'). That will allow the creation of an antidote data center, as it allows for cluster internal communication between antidote instances.
	- Some **volumeMounts** for data. In the [example](example_yaml_templates/statefulSet.yaml) the **mountPath** is specified as "/antidote-data" which is the same as in the produced antidote docker images from [docker-antidote](https://github.com/AntidoteDB/docker-antidote).

#### VolumeClaimTemplates
*VolumeClaimTemplates* tell kubernetes which storage each pod of this *StatefulSet* shall bind. 
For antidote we would like each pod (antidote instance) to bind a persistent local storage here, since we want the storage to be on the same node as the instance.
This can be done in kubernetes through [LocalPersistentVolumes](https://kubernetes.io/blog/2019/04/04/kubernetes-1.14-local-persistent-volumes-ga/).

The solution provided in the link above in general needs an administrator to manually provide this storage. An provisioner can be configured such that each of this provided storage can be accessed through a volumeClaimTemplate again. This means there is no change to the *StatefulSet*.yaml but the **storageClassName** in the claim template.

### Services
Accessing a data center is done with a [Service](https://kubernetes.io/docs/concepts/services-networking/service/), that has a *LoadBalancer*-property. If used in combination with a cloud provider that supports this feature, this services will be provided with an **ExternalIPAdress**, which will allow for traffic from outside the cluster. Also this Service is provided a ClusterIpAddress which is cluster internal. This may other applications to access the data center inside the cluster more easily. 

### StorageClass
The storage class provided in the example template [storageClass.yaml](example_yaml_templates/storageClass.yaml) does only provide local storage since minikube operates on only a single node.
If you have a cluster consiting of several nodes, you should read up on the 'local persistent volume ga' mentioned above.

### Jobs

For now, we need two different kinds of jobs. First, to create an antidote data center. Second, to connect different antidote datacenters.
In the example this is done with the *peterzel/antidote-connect* container from [here](https://github.com/peterzeller/antidote-connect), which uses the [antidote-go-client](https://github.com/AntidoteDB/antidote-go-client).

Basically the container is run with different arguments:
- to create a data center: `--createDC "datacenter:port" "antidote@node1", ..., "antidote@nodeN"`
- to connect different data centers: `--connectDcs "datacenter1:port", ..., "datacenterN:port"`
As you can see in the example for [createDC.yaml](example_yaml_templates/createDC.yaml) and [connectDCs.yaml](example_yaml_templates/connectDCs.yaml.template)

**Note:** 
- Here "datacenter:port" refers to the "pod_ip:antidote-pb-port" or "hostname.subdomain:antidote-pb-port" of the **first** pod from the *StatefulSet* that will then also be the manager-node for each data center and their underlying erlang clusters. (It is sufficient to access a node with hostname.subdomain, since the job should be inside the same cluster and inside the same namespace as the StatefulSet)
- And "antidote@node" refers to "antidote@pod_ip" (or "antidote@hostname.subdomain" if dns is provided).

## Deployment on Google Cloud: Example

When you run the above example on Google Cloud Platform (GCP) and you are using Google Kubernetes Engine (GKE), you do not have to install or manage kubernetes by yourself. Google handles it for you 😉

However, you have to add and modify some files to run successfully.

After creating a google kubernetes cluster, using the following command to check if you have a storage class (by default, Google will created it automatically):
```
kubectl get storageclasses
```
1. If you already have a storage class, you only need to change `storageClassName` field in the [statefulSet.yaml](example_yaml_templates/statefulSet.yaml) file:
```
storageClassName: <your_google_storage_class_name> 
```
2. If you do not have any storage class or you want to create your own, edit the [StorageClass.yaml](example_yaml_templates/storageClass.yaml) file:
```
kind: StorageClass
apiVersion: storage.k8s.io/v1
metadata:
  name: mystorageclass
provisioner: kubernetes.io/gce-pd 
parameters:
  type: pd-standard
  fstype: ext4
volumeBindingMode: WaitForFirstConsumer
```
and then change `storageClassName` field in the [statefulSet.yaml](example_yaml_templates/statefulSet.yaml) file:
```
storageClassName: mystorageclass
```
