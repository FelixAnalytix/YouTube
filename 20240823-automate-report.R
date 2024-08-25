library(dplyr)
library(purrr)

data <- expand.grid(
  name = unique(starwars$name),
  year = "2024",
  stringsAsFactors = FALSE)

df <- data |> 
  dplyr::mutate(
    output_format = "docx",       # Output format (html, word, etc.)
    output_file = paste(          # Output file name
      name, year, "report.docx",
      sep = "_"
    ),
    execute_params = purrr::map2( # Named list of parameters
      year, name, 
      \(year, name) list(name = name, year = year)
    )
  ) |> 
  dplyr::select(-c(name, year))

df

purrr::pwalk(
  .l = df,                      # Dataframe to map over
  .f = quarto::quarto_render,   # Quarto render function
  input = "20240823-automate-report.qmd",       # Named arguments of .f
  .progress = TRUE              # Optionally, show a progress bar
)
