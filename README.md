empatica
=======

`empatica` is an [R-package](https://www.r-project.org/) for reading physiologic data recorded with an [Empatica](https://www.empatica.com/) device.

Installation
------------
To install the `empatica` package in R, proceed as follows in R.

First install the `devtools`-package and load it:
```
   install.packages("devtools")
   library(devtools)
```

You can now install the `empatica` package:
```
   install_github("bwrc/empatica")
```

Usage
-----
1. After having recorded some data with the Empatica device, upload the data normally to Empatica cloud service Empatica Connect.
2. After this, download the data as a zip-file.
3. To read data recorded using the Empatica, proceed as follows:

```
   library(empatica)
   datafile  <- "/tmp/my_empatica_data.zip"
   recording <- read.empatica(datafile)
```
