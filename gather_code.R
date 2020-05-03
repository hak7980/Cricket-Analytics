library(yaml)
library(tidyverse)
library(janitor)
library(yorkr)
library(yaml)
library(base)

## This Rscript files automates the data gathering 
## Process for my final project by downaloading 
## cleaning and storing the data I use from the web


# Storing all the T20 files as a list in the 
# raw-data files 

files <- list.files(path="raw-data/t20", pattern="*.yaml",
                    
                    full.names=TRUE, recursive=FALSE)

# Using the convertYaml2Rdata frame to convert the
# yaml list of lists of T20 matches into a Rdata frame

lapply(files, function(x) {
  
  try(
    
    convertYaml2RDataframeT20(x, ".", "./output/t20")
  )
  
})

```

```{r converting ODI, eval=FALSE, include=FALSE}

# Storing all the ODI files as a list in the 
# raw-data files 

files <- list.files(path="raw-data/odi", pattern="*.yaml",
                    
                    full.names=TRUE, recursive=FALSE)

# Using the convertYaml2Rdata frame to convert the
# yaml list of lists of T20 matches into a Rdata frame

lapply(files, function(x) {
  
  try(
    
    convertYaml2RDataframeT20(x, ".", "./output/odi")
  )
  
})

```



```{r creating T20 meta file, include=FALSE}

# Storing the dir of the t20 RData files as path
# and then as a list called files

path <- "output/t20/"

files <- list.files(path=path)

# Changing working directory to output directory

setwd(path)

# Loading all the T20 RData sets into RStudio and
# then binding them

results <- 
  
  sapply(files, function(x) mget(load(x)), simplify = TRUE) 

results <- bind_rows(results, .id = "column_label")

# Saving the meta file as t20metadata

save(results, file =  "raw-data/t20metadata.RData")

```

```{r creating ODI meta file, include=FALSE}


# Storing the dir of the ODI RData files as path
# and then as a list called files

path <- "output/odi/"

files <- list.files(path=path)

# Changing working directory to output directory

# setwd(path)

# Loading all the ODI RData sets into RStudio and
# then binding them

results <- 
  
  sapply(files, function(x) mget(load(x)), simplify = TRUE) 

results <- bind_rows(results, .id = "column_label")

# Saving the meta file as odimetadata

save(results, file =  "raw-data/odimetadata.RData")

```
