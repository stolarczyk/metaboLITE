---
title: Windows other
permalink: /docs/Windows_other/
---

# Prerequisites

Don't know which version of Windows you are running? This is [how to check it](https://support.microsoft.com/en-us/help/13443/windows-which-operating-system).

Make sure you have the **virtualization** enabled in your system settings. This is [how to do it](https://docs.docker.com/toolbox/toolbox_install_windows/#step-1-check-your-version). If the virtualization is not enabled, this [video](https://www.youtube.com/watch?v=zIm7f5Epd9U) will show you how to do it.

# Installation - Docker

## Download the *Docker* installer on your machine

Download the [Docker toolbox](https://docs.docker.com/toolbox/overview/#ready-to-get-started) for Windows 10 **other than** Pro/Enterprise/Education.
The detailed installation instructions by Docker can be found on the [Docker website](https://docs.docker.com/toolbox/toolbox_install_windows/). See these in case you encounter any problems with the installation steps listed here!

Direct link to the [file](https://download.docker.com/win/stable/DockerToolbox.exe)

## Install Docker on your machine

1. Double-click the installer `DockerToolbox.exe` to run it
2. Follow the steps in the user friendly installation wizard
as usually, you'll be asked about the software destination location
	1. When you get prompted to select the the Docker components that should be installed, make sure to select *Kitematic for Windows* and *Docker for Windows* as shown on the screenshot below (the boxes should be checked by default):

	![](https://github.com/michalstolarczyk/metaboLITE/tree/master/docs/docs_pics/installComponentsWindowsOld.PNG)

	2. Accept the defaults if you wish in the further steps
3. Finalize the installation

## Check if the Dokcer was installed correctly

Find the `Docker Quickstart Terminal` (that you've just downloaded) program and run it. It will get the job done for you.

## Run Kitematic

1. It can take couple minutes to setup for the first time

![run Kitematic](https://github.com/michalstolarczyk/metaboLITE/tree/master/docs/docs_pics/runKitenaticWindows.PNG)

2. Click *skip for now* in the right lower corner if you do not wish to create the account

And you're all set! 

## Find and run *metaboLITE*

* click create in the box with the proper *metabolite* Docker image. It should be created by the user `mstolarczyk`

![select metaboLITE](https://github.com/michalstolarczyk/metaboLITE/tree/master/docs/docs_pics/selectMetaboliteWindows.PNG)

* the application will start and all you need to do is click on the web preview, this will launch it in your web browser

![launch metaboLITE](https://github.com/michalstolarczyk/metaboLITE/tree/master/docs/docs_pics/launchMetaboliteWindows.PNG)

