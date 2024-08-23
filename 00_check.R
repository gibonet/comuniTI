
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

