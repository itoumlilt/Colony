# Examples

The provided folder with some [templates](example_yaml_templates/) can be applied to a kubernetes cluster directly.
It will set up 1 Antidote datacenter with 2 instances.
```
kubectl apply -f example_yaml_templates
```
This will do the job. Note that by applying the whole folder directly no ordering of the creation of the resources is included. The datacenter will none the less eventually be up and ready.

There are various other ways to use kubernetes.

## Nativ
An example how to deploy antidote via kubernetes by using only bash-scripts and the command line tool [kubectl](https://kubernetes.io/docs/reference/kubectl/kubectl/) of kubernetes can be found [here](https://github.com/Yannick-W/antidote/tree/master/deployment/kubernetes/Nativ).