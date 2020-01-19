# Install packages --------------------------------------------------------

remotes::install_github("aswansyahputra/bandungjuara")
install.packages("here")
install.packages("tidyverse")

# Load packages -----------------------------------------------------------

library(bandungjuara)
library(here)
library(tidyverse)
library(tidytext)
library(gutenbergr)

# Anggaran Dinas Kesehatan Kota Bandung -----------------------------------

anggaran_dinkes_raw <-
  cari("anggaran dinas kesehatan") %>%
  impor() %>%
  bind_rows()

anggaran_dinkes <-
  anggaran_dinkes_raw %>%
  mutate(
    program = str_remove_all(program, "Program |\\n") %>% str_to_title(),
    program = str_replace_all(program, "Peningkatanpelayanan", "Peningkatan Pelayanan"),
    program = str_replace_all(program, "Pelayanankesehatan", "Pelayanan Kesehatan"),
    program = str_replace_all(program, "Peningkatandan", "Peningkatan Dan "),
    program = str_replace_all(program, "Danpemberdayaan", "Dan Pemberdayaan"),
    program = str_replace_all(program, "Puskemas", "Puskesmas"),
    program = str_replace_all(program, "Klb", "Kejadian Luar Biasa"),
    program = str_replace_all(program, "Blud", "Badan Layanan Usaha Daerah"),
    program = str_replace_all(program, "Sdm", "SDM"),
    program = str_replace_all(program, "Dan", "dan"),
    program = str_replace_all(program, "\\s-\\s\\s", ", "),
    program = str_replace_all(program, "\\s\\s", " "),
    program = str_replace_all(program, "/\\s", "/"),
    serapan = realisasi/anggaran
  ) %>%
  select(tahun, everything(), -skpd, -catatan, -persentase_serapan) %>%
  arrange(tahun, program)

anggaran_dinkes %>%
  mutate(id = paste0("anggaran-dinkes-", tahun)) %>%
  group_by(id) %>%
  group_walk(~write_csv(.x, paste0(here("data-raw"), "/", .y$id, ".csv")))

# Kualitas udara Kota Bandung ---------------------------------------------

udara_bandung_raw <-
  cari("udara") %>%
  impor() %>%
  bind_rows()

udara_bandung_raw %>%
  mutate(station = coalesce(stasiun, statsiun)) %>%
  rename(day = waktu) %>%
  select(-stasiun, -statsiun) %>%
  write_csv("data-raw/udara_bandung.csv")

udara_bandung_raw %>%
  mutate(station = coalesce(stasiun, statsiun)) %>%
  rename(day = waktu) %>%
  select(-stasiun, -statsiun) %>%
  group_by(station) %>%
  group_walk(~write_csv(.x, paste0(here("data-raw"), "/", .y$station, ".csv")))

# Dirty excel file (from janitor) -----------------------------------------

download.file("https://github.com/sfirke/janitor/raw/master/dirty_data.xlsx", here("data-raw", "roster.xlsx"))


# UN SMP Kota Bandung -----------------------------------------------------

un_smp_raw <- cari("UN SMP") %>%
  impor()

un_smp <-
  un_smp_raw %>%
  bind_rows(.id = "id") %>%
  transmute(
    tahun = str_extract(id, "\\d{4}"),
    status = if_else(str_detect(id, "Swasta"), "Swasta", "Negeri"),
    nama_sekolah = coalesce(nama_satuan_pendidikan, nama_satuan),
    jumlah_peserta = jumlah_peserta,
    bahasa_indonesia = bahasa_indonesia,
    bahasa_inggris = bahasa_inggris,
    matematika = matematika,
    ipa = ipa
  )

un_smp %>%
  write_csv(here("data-raw", "un_smp.csv"))

# Sherlock Holmes ---------------------------------------------------------
sherlock_raw <- gutenberg_download(1661)

sherlock <- sherlock_raw %>%
  mutate(
    story = if_else(str_detect(text, "ADVENTURE"),
                    text,
                    NA_character_),
    text = na_if(text, "")
  ) %>%
  fill(story) %>%
  filter(story != "THE ADVENTURES OF SHERLOCK HOLMES") %>%
  select(story, text)

sherlock %>%
  write_csv(here("data-raw", "sherlock.csv"))
