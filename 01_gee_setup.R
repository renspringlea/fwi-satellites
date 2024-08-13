# https://github.com/r-spatial/rgee
# install.packages(c("remotes", "googledrive"))
# remotes::install_github("r-spatial/rgee")
library(rgee)

# Get the username
HOME <- Sys.getenv("HOME")

# 1. Install miniconda
reticulate::install_miniconda(force=T)

# 2. Install Google Cloud SDK
system("curl -sSL https://sdk.cloud.google.com | bash")

# 3 Set global parameters
#Sys.setenv("RETICULATE_PYTHON" = sprintf("%s/.local/share/r-miniconda/bin/python3", HOME))
Sys.setenv("EARTHENGINE_GCLOUD" = sprintf("%s/google-cloud-sdk/bin/", HOME))
Sys.setenv("RETICULATE_PYTHON" = "/usr/bin/python3")

# 4 Install rgee Python dependencies
ee_install()
ee_check()
rgee::ee_install_upgrade()
ee_check()

# 5. Authenticate and init your EE session
# Attempt to authenticate. If credentials are found, nothing will happen except
# a return of TRUE. If credentials are not found, it'll take you through an auth
# flow and save the credentials. If you want to force reauthentication, include
# `force=TRUE` in the call. This is generally a one-time setup step.
ee$Authenticate(auth_mode='notebook')

# Initialize - this will connect to a project. You should always call this
# before working with rgee. It is IMPORTANT THAT YOU SPECIFY A PROJECT using
# the project parameter. If you forget what project IDs you have access to, find them
# here: console.cloud.google.com/project
ee$Initialize(project='ee-fwi-satellites')  # <-- EDIT THIS FOR YOUR PROJECT

rgee::ee_Initialize() #https://github.com/r-spatial/rgee/issues/355

# Optionally make a request to verify you are connected.
ee$String('Hello from the Earth Engine servers!')$getInfo()

