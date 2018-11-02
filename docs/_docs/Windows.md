---
title: Windows
permalink: /docs/Windows/
---


# Installation - Docker container

The most reliable, safest and **easiest installation approach** is to use the [Docker](https://www.docker.com/what-docker) platform due to its intrisic characterisitics. In order to do it user needs to run the pre-built Docker image (provided in the official Docker [repository](https://hub.docker.com/r/mstolarczyk/toyconapp/)).

## 1. Install the Docker CE (community edition) software on your machine

The installation instructions can be found on the [Docker website](https://docs.docker.com/install/):

* [Windows 10 Pro/Enterprise/Education](https://docs.docker.com/docker-for-windows/install/)
* [Older Windows](https://docs.docker.com/toolbox/toolbox_install_windows/)

## 2. Open the Docker application to ensure it installed correctly

## 3. Downloading and launching the application

Generally, running the commands presented in **3.2** will download and launch the application for every operating system. However, in case of MacOS and Windows, there is an option to use a graphical interface to avoid working in terminal, for details see below.

### 3.1 Kitematic interface installation (Windows and MacOS only)

There is a simple, yet powerful graphical user (GUI) interface available for Windows and MacOS - [Kitematic](https://kitematic.com/). Kitematic’s one click install gets Docker running on your computer and lets you control your app containers from a GUI.
Kitematic is provided together with the Docker for Windows and should be ready to use on this stage of installation. 
On MacOS to get Kitematic just expand the Docker menu when it is running, select Kitematic and you will be presented with the instructions on how to install it.

PIC

_In case of both operating systems_: once the Kitematic is installed, you can search for any Docker image deposited to the Docker Hub. To download the application, just search the `metabolite` phrase and "create" the container deposited by the user: `mstolarczyk`. 

PIC

Once the download process is completed, the container will run automatically and you can launch the application by clicking on the web preview section in Kitematic, as shown below:

PIC

