# Pendahuluan ----
## This time, I will try to analyze and visualize datasets related to poverty in  West Java Province. 
## I will try to explore gt() dan gtExtras() packages to produce "tabel" as a visualization alternatives.

# Load packages ----
library(tidyverse) ## data wangling and exploration
library(gt) ## table visualization
library(gtExtras) ## table visualization
library(paletteer) ## provides color palette

# Load datasets ----

pop_jbr <- read_csv("data_raw/kemiskinan/jumlah_penduduk_berdasarkan_jenis_kelamin_data.csv") %>% janitor::clean_names()

pov_pop <- read_csv("data_raw/kemiskinan/jumlah_penduduk_miskin_berdasarkan_kabupatenkota_data.csv") %>% janitor::clean_names()

pov_exp <- read_csv("data_raw/kemiskinan/jumlah_pengeluaran_per_kapita__kabupatenkota_data.csv") %>% janitor::clean_names()

pov_liner <- read_csv("data_raw/kemiskinan/angka_garis_kemiskinan_per_kapita_per_bulan__kabupaten_data.csv") %>% janitor::clean_names()

pov_linep <- readxl::read_xlsx("data_raw/kemiskinan/angka_garis_kemiskinan_per_kapita_per_bulan_provinsi.xlsx")%>% janitor::clean_names()

## we will focus our analysis for 2020 as it's in the middle of West Java Governance reign and one year after COVID-19 pandemic

# prepare datasets ----

## we will analyse some topics such as percentage of poverty population, the trend of poverty population, and the comparison between poverty line and monthly expenditure

# > poverty population percentage in 2020 
pov_pop_pct <- pop_jbr %>% 
  dplyr::filter(tahun == 2020) %>% 
  group_by(nama_kabupaten_kota, tahun) %>% 
  summarise(populasi = sum(jumlah_penduduk)) %>%
  ungroup() %>% 
  left_join(
    pov_pop %>% select(5:8),
    by = c("nama_kabupaten_kota", "tahun")
  ) %>% 
  select(-5) %>% 
  rename(
    daerah = nama_kabupaten_kota,
    pov_pop = jumlah_penduduk_miskin
  ) %>% 
  mutate(
    populasi = populasi/1000,
    pov_pct = pov_pop * 100 / populasi,
    ) %>% 
  arrange(desc(pov_pct)) %>% 
  select(-2,-4)

# try to visualize
pov_pop_pct %>% 
  gt() %>% 
  # gt_theme_espn() %>% 
  cols_align(
    daerah,
    align = "left"
  ) %>% 
  gt_plt_dot(
    pov_pct, daerah,
    palette = "rcartocolor::BluYl",
    max_value = 12
  ) %>% 
  fmt_symbol_first(
    column = pov_pct,
    suffix = "%"
  ) %>% 
  fmt_symbol_first(
    column = populasi,
    decimals = 0,
    suffix = "ribu"
  )

# > poverty population trend 2016 - 2020
pov_trend <- pov_pop %>% 
  dplyr::filter(tahun %in% c(2011:2020)) %>%
  rename(daerah = nama_kabupaten_kota) %>% 
  group_by(daerah) %>% 
  summarise(trend = list(jumlah_penduduk_miskin))

# try to visualize
pov_trend %>% 
  gt() %>% 
  gtExtras::gt_sparkline(
    trend,
    range_colors = c("#1976D2", "#E53935"),
    line_color = "#212121",
    label = FALSE
  )

# > comparing monthly expenditure per capita and province's poverty line
pov_xline <- pov_exp %>% 
  dplyr::filter(tahun == 2020) %>%
  select(5,6,8) %>% 
  mutate(monthly_exp = pengeluaran_per_kapita/12,
         ) %>%
  left_join(
    pov_linep,
    by = "tahun"
  ) %>% 
  select(-2, -3) %>% 
  rename(
    daerah = nama_kabupaten_kota,
    pov_linep = garis_kemiskinan) %>% 
  mutate(pov_linep = pov_linep/1000,
  )

# try to visualize
pov_xline %>% 
  gt() %>% 
  gt_plt_bullet(
    column = monthly_exp,
    target = pov_linep,
    width = 50,
    colors = c("#069550", "#FFC800")
  )

# > comparing regional and province poverty line in 2020
pov_linec <- pov_liner %>% 
  dplyr::filter(tahun == 2020) %>% 
  select(5,6,8) %>% 
  rename(daerah = nama_kabupaten_kota) %>% 
  left_join(
    pov_linep,
    by = "tahun") %>% 
  rename(
    pov_linep = garis_kemiskinan,
    pov_liner = garis_kemiskinan_perkapita
    ) %>% 
  select(-3) %>% 
  pivot_longer(
    cols = c(2,3),
    names_to = "pov_line_type",
    values_to = "pov_line_value"
  ) %>% 
  group_by(daerah) %>% 
  summarise(pov_linec = list(pov_line_value))

# try to visualize
clr_cmp <- c("#1976D2", "orange")

pov_linec %>% 
  gt() %>% 
  gt_plt_bar_stack(
    column = pov_linec,
    position = "stack",
    labels = c("Kab/Kota", "Provinsi"),
    palette = c("#069550", "#FFC800"),
    width = 60)

# > joined df
pov_joined <- pov_pop_pct %>% 
  left_join(pov_trend) %>% 
  left_join(pov_xline) %>% 
  mutate(daerah = tools::toTitleCase(tolower(daerah))) %>% 
  mutate(" " = monthly_exp) %>% 
  mutate(daerah = str_replace(daerah, "Kabupaten", "Kab."))

# visualization ----
pov_tab <- pov_joined %>% 
  gt() %>% 
  cols_align(
    daerah,
    align = "left"
  ) %>% 
  gt_plt_dot(
    pov_pct, daerah,
    palette = "rcartocolor::BluYl",
    max_value = 12
  ) %>% 
  gtExtras::gt_sparkline(
    trend,
    range_colors = c("#069550", "#FFC800"),
    line_color = "#212121",
    label = FALSE
  ) %>% 
  gt_plt_bullet(
    column = " ",
    target = pov_linep,
    keep_column = FALSE,
    width = 50,
    colors = c("#069550", "#FFC800")
  ) %>% 
# formatting
  # fmt_symbol_first(populasi, decimals = 0, use_seps = FALSE) %>% 
  # fmt_number(monthly_exp, decimals = 0, use_seps = FALSE) %>% 
  fmt_symbol_first(
    populasi,
    decimals = 0,
    suffix = " ribu",
    gfont = "Fira Code"
  ) %>% 
  fmt_symbol_first(
    pov_pct,
    decimals = 2,
    symbol = "&#37;",
    gfont = "Fira Code"
  ) %>% 
  fmt_symbol_first(
    monthly_exp,
    decimals = 0,
    suffix = " ribu",
    gfont = "Fira Code"
  ) %>% 
# writing column names, title, subtitle, footnote, source note
  cols_label(
    daerah = "Daerah",
    populasi = md("Populasi Penduduk<br/>(jiwa)"),
    pov_pct = md("Persentase<br/>Penduduk Miskin"),
    trend = md("Tren Jumlah<br/>Penduduk Miskin"),
    monthly_exp = md("Pengeluaran Bulanan<br/>(rupiah)")
  ) %>%
  tab_header(
    title = md("Selayang Pandang Kemiskinan di Jawa Barat Tahun 2020"),
    subtitle = md("Kemiskinan menjadi salah satu aspek yang terus diusahakan untuk dientaskan oleh Pemerintah Provinsi Jawa Barat. Pada tahun 2020, **Kota Tasikmalaya** memiliki persentase jumlah penduduk miskin tertinggi, yaitu sebesar **11.87% dari 726 ribu jiwa**, meskipun angka ini selalu **menurun sejak 5 tahun terakhir**. Di sisi lain, seluruh Kota dan Kabupaten di Jawa Barat tetap mampu bertahan **di atas Garis Kemiskinan** walaupun diterpa pandemi COVID-19.")
  )%>%
  tab_source_note(
    source_note = md("**Data:** Open Data Jawa Barat & Badan Pusat Statistik | **Tabel:** R. Ramadhan for Jabar Digital Service")
  )%>%
  tab_footnote(
    footnote = md("**Garis Kemiskinan** (GK) dapat diterjemahkan sebagai pengeluaran minimum untuk memenuhi kebutuhan gizi 2100 kilokalori per hari serta kebutuhan lainnya seperti tempat tinggal, sandang, pendidikan, dan kesehatan. Warga dengan rata-rata pengeluaran per bulan di bawah GK akan dikategorikan sebagai penduduk miskin."),
    locations = cells_title(groups = "subtitle")
  ) %>% 
  tab_footnote(
    footnote = md("Pengeluaran bulanan per kapita tiap Kota dan Kabupaten pada tahun 2020 (*garis hijau*) dibandingkan terhadap GK Provinsi Jawa Barat sebesar **Rp410.988,00** (*garis kuning*)."),
    locations = cells_column_labels(
      columns = monthly_exp
  )) %>% 
  cols_align(
    align = "right",
    columns = c(populasi, pov_pct, monthly_exp)
  ) %>% 
  cols_align(
    align = "center",
    columns = c(trend)
  ) %>% 
  # Title
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Playfair Display"), 
        size = 'xx-large',
        align = "left",
        color='#006430')),
    locations = cells_title(groups = "title")
  )%>%
  # Subtitle
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Roboto Condensed"),
        color = "#212121",
        align = "left")
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  # Header
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Roboto Condensed"), 
        align = "left",
        v_align = "middle",
        color = "#212121")),
    locations = cells_column_labels(
      columns = c(
        daerah)
    )
  )%>%
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Roboto Condensed"), 
        align = "center",
        v_align = "middle",
        color = "#212121")),
    locations = cells_column_labels(
      columns = c(populasi, pov_pct, trend, monthly_exp,
                  " ")
    )
  )%>%
  # Body
  tab_style(
    style = list(
      cell_text(font=google_font(name = "Muli"),
                align = "left")),
    locations = cells_body(
      columns = c(daerah)
    )
  )%>%
  tab_style(
    style = list(
      cell_text(font=google_font(name = "Muli"),
                align = "center")),
    locations = cells_body(
      columns = c(trend)
    )
  )%>%
  # Footnote
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Roboto Condensed"),
        style = "italic",
        size = "small")),
    locations = cells_footnotes()
  ) %>%
  # source note
  tab_style(
    style = list(
      cell_text(font=google_font(name = "Roboto Condensed"))),
    locations = cells_source_notes()
  )%>%
  # Borders
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden"
  )
  
gtsave_extra(
  data = pov_tab,
  filename = "Outfile/202110_kemiskinan_kemiskinan2020.png"
)

# references ----
# https://opendata.jabarprov.go.id/id/dataset/jumlah-penduduk-berdasarkan-jenis-kelamin-di-jawa-barat
# https://opendata.jabarprov.go.id/id/dataset/jumlah-penduduk-miskin-berdasarkan-kabupatenkota-di-jawa-barat
# https://opendata.jabarprov.go.id/id/dataset/angka-garis-kemiskinan-per-kapita-per-bulan-berdasarkan-kabupatenkota-di-jawa-barat
# https://opendata.jabarprov.go.id/id/dataset/jumlah-pengeluaran-per-kapita-berdasarkan-kabupatenkota-di-jawa-barat
# https://www.bps.go.id/subject/23/kemiskinan-dan-ketimpangan.html