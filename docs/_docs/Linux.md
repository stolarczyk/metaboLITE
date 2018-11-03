---
title: Linux
permalink: /docs/Linux/
---

# Installation - Docker container

The most reliable, safest and **easiest installation approach** is to use the [Docker](https://www.docker.com/what-docker) platform due to its intrisic characterisitics. In order to do it user needs to run the pre-built Docker image (provided in the official Docker [repository](https://hub.docker.com/r/mstolarczyk/metabolite/)).

## Install the Docker CE (community edition) software on your machine

The installation instructions can be found on the [Docker website](https://docs.docker.com/install/):

* [Debian](https://docs.docker.com/install/linux/docker-ce/debian/)
* [Fedora](https://docs.docker.com/install/linux/docker-ce/fedora/)
* [Ubuntu](https://docs.docker.com/install/linux/docker-ce/ubuntu/)


## Downloading and launching the application

Generally, running the commands presented in **3.2** will download and launch the application for every operating system. However, in case of MacOS and Windows, there is an option to use a graphical interface to avoid working in terminal, for details see the respective pages.


### Command line Docker image management

Depending on your Docker installation, the superuser privilages may be required, which can be acquired with `sudo`.

Open the command window (terminal) and download the docker image from the repository with a following [command](https://docs.docker.com/engine/reference/commandline/pull):

```docker pull [OPTIONS]```

e.g copy and paste:

``` (sudo) docker pull mstolarczyk/metabolite:latest ``` (this may take some time depending on your connection)

#### Check the image status:

``` (sudo) docker images```

The output of the command above should resemble the following:

``` 
REPOSITORY              TAG                 IMAGE ID            CREATED             SIZE 
mstolarczyk/metabolite   latest              45f03ae5cff4        30 minutes ago      2.35GB 
``` 

#### Run the application using the ``` IMAGE ID ``` with a following [command](https://docs.docker.com/engine/reference/commandline/run/):
    
```docker run [OPTIONS] IMAGE [COMMAND] [ARG...] ```

e.g copy and paste

```(sudo) docker run -p 8080:8080 45f03ae5cff4 ``` (note that the ``` IMAGE ID ``` will be different in your case)

The command above will allow you to connect to the application from your web browser.

#### Copy and paste this address into your web browser

``` localhost:8080 ```

