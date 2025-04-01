# download git
# https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r

# configure git
library(usethis)

use_git_config(user.name = "rangerjohn99", user.email = "rangerjohn99@gmail.com")

use_git()

create_github_token()

# need to request github token

library(gitcreds)

gitcreds_set()

use_github()