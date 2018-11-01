---
title: Windows
permalink: /docs/Windows/
---


# Installation - Docker container<a name="Installation"></a>
The most reliable, safest and **easiest installation approach** is to use the [Docker](https://www.docker.com/what-docker) platform due to its intrisic characterisitics. In order to do it user needs to run the pre-built Docker image (provided in the official Docker [repository](https://hub.docker.com/r/mstolarczyk/toyconapp/)).

#### 1. Install the Docker CE (community edition) software on your machine

The installation instructions can be found on the [Docker website](https://docs.docker.com/install/):

* Linux
    * [Debian](https://docs.docker.com/install/linux/docker-ce/debian/)
    * [Fedora](https://docs.docker.com/install/linux/docker-ce/fedora/)
    * [Ubuntu](https://docs.docker.com/install/linux/docker-ce/ubuntu/)
* Windows
    * [Windows 10 Pro/Enterprise/Education](https://docs.docker.com/docker-for-windows/install/)
    * [Older Windows](https://docs.docker.com/toolbox/toolbox_install_windows/)
* [MacOS](https://docs.docker.com/docker-for-mac/install/)

#### 2. Open the Docker application to ensure it installed correctly

- On MacOS the green dot and "Docker is running" status should be displayed in the menu after clicking the Docker icon

<br/>
<img src="./docs_pics/icon_running.png" width="20%" > 
<figcaption><b>Image 1.</b> LEFT: MacOS - the dot shows the Docker software status. RIGHT: Windows - ...</figcaption>
<br/>
<br/>

#### 3. Downloading and launching the application

Generally, running the commands presented in **3.2** will download and launch the application for every operating system. However, in case of MacOS and Windows, there is an option to use a graphical interface to avoid working in terminal, for details see below.

**3.1 Kitematic interface installation (Windows and MacOS only)**

There is a simple, yet powerful graphical user (GUI) interface available for Windows and MacOS - [Kitematic](https://kitematic.com/). Kitematic’s one click install gets Docker running on your computer and lets you control your app containers from a GUI.
Kitematic is provided together with the Docker for Windows and should be ready to use on this stage of installation. 
On MacOS to get Kitematic just expand the Docker menu when it is running, select Kitematic and you will be presented with the instructions on how to install it.

<br/>
<img src="./docs_pics/icon_kitematic.png" width="25%" class="center">
<figcaption><b>Image 2.</b> Kitematic can be launched/installed from the Docker manu.</figcaption>
<br/>
<br/>

_In case of both operating systems_: once the Kitematic is installed, you can search for any Docker image deposited to the Docker Hub. To download the shinyapp, just search the shinyapp phrase and "create" the container deposited by the user: mstolarczyk. 

<br/>
<img src="./docs_pics/search_image_kitematic.png" width="70%" class="center">
<figcaption><b>Image 3.</b> Any Docker image deposited to the Docker Hub can be downloaded with Kitematic.</figcaption>
<br/>
<br/>
Once the download process is completed, the container will run automatically and you can launch the application by clicking on the web preview section in Kitematic, as shown below:

<br/>
<img src="./docs_pics/launch_app_kitematic.png" width="70%" class="center">
<figcaption><b>Image 4.</b> The application can be launched by clicking on the web preview in the Kitematic software.</figcaption>
<br/>
<br/>

**3.2 Command line Docker image management (Linux only)**

Depending on your Docker installation, the superuser privilages may be required, which can be acquired with `sudo`.

Open the command window (terminal) and download the docker image from the repository with a following [command](https://docs.docker.com/engine/reference/commandline/pull):

```docker pull [OPTIONS]```

e.g copy and paste:

``` (sudo) docker pull mstolarczyk/shinyapp:latest ``` (this may take some time depending on your connection)

**3.2.1. Check the image status**:

``` (sudo) docker images```

The output of the command above should resemble the following:

``` 
REPOSITORY              TAG                 IMAGE ID            CREATED             SIZE 
mstolarczyk/toyconapp   latest              45f03ae5cff4        30 minutes ago      2.35GB 
``` 

**3.2.2. Run the application** using the ``` IMAGE ID ``` with a following [command](https://docs.docker.com/engine/reference/commandline/run/):
    
```docker run [OPTIONS] IMAGE [COMMAND] [ARG...] ```

e.g copy and paste

```(sudo) docker run -p 8080:8080 45f03ae5cff4 ``` (note that the ``` IMAGE ID ``` will be different in your case)

The command above will allow you to connect to the application from your web browser.

**3.2.3. Copy and paste this address into your web browser**

``` localhost:8080 ```

