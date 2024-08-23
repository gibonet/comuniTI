
library(devtools)

load_all()
document()
check_man()
check()

build()

install()

# Copia in 00_copia_sorgenti_gib_packs -----
file.copy(
  from = "../comuniTI_1.3.7.tar.gz",
  to = "../00_copia_sorgenti_gib_packs",
  copy.date = TRUE,
  copy.mode = TRUE
)


# git push both on gitlab and github ------------
# From the terminal:
# git remote add gitlab https://gitlab.com/gibonet/comuniTI.git
# git remote -v
# origin is github, gitlab is gitlab

# To push on both remote repositories:
# git push -u origin master    # push on gitlab
# git push -u gitlab master    # push on github

