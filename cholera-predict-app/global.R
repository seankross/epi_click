library(googleAuthR)
library(googleID)

opts = list()
force = FALSE

glogin = FALSE
if (file.exists("epi-click-auth.R")) {
  opts = list(port = 1221)
  
  source("epi-click-auth.R")
  options(googleAuthR.scopes.selected = 
            c("https://www.googleapis.com/auth/userinfo.email",
              "https://www.googleapis.com/auth/userinfo.profile"))
  options("googleAuthR.webapp.client_id" = gclient_id)
  options("googleAuthR.webapp.client_secret" = gsecret_id)
  glogin = TRUE
}
