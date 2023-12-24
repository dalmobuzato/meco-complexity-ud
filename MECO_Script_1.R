library(readr)
library(udpipe)
library(dplyr)
library(tidyverse)

# Load the CSV file.
yourfile <- read_csv("C:/Users/buzat/Downloads/supp_texts.csv")

# ------------------------ English ----------------------------

# Find the row corresponding to the language "English"
line_english <- yourfile[yourfile[, 1] == "English", ]

# Remove the first column (languages) from the row
line_english <- line_english[, -1]

# Get the column names
column_names <- colnames(line_english)

# For each column, create a variable with the column name (using only the first word)
for (column in column_names) {
  variable_name <- sub(" .*", "", column)  # Use only the first word
  assign(variable_name, line_english[[column]][1])
}

column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# Loop to remove "\n", "\" and """
for (column_name in column_names) {
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
}

# English: Create a language model and udpipe texts 
model_english <- udpipe_download_model(language = "english") 
model_english <- udpipe_load_model(file = model_english$file_model)

for (name in column_names) {
  variable_name_english <- paste0(name, "_english")
  assign(variable_name_english, udpipe_annotate(model_english, x = get(name)))
  assign(variable_name_english, as.data.frame(get(variable_name_english)))
}


# ------------------------ Dutch ----------------------------

# Find the row corresponding to the language "Dutch"
line_dutch <- yourfile[yourfile[, 1] == "Dutch", ]

# Remove the first column (languages) from the row
line_dutch <- line_dutch[, -1]

# Get the column names
column_names <- colnames(line_dutch)

# For each column, create a variable with the column name (using only the first word)
for (column in column_names) {
  variable_name <- sub(" .*", "", column)  # Use only the first word
  assign(variable_name, line_dutch[[column]][1])
}

column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# Loop to remove "\n", "\" and """
for (column_name in column_names) {
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
}

# Dutch: Create a language model and udpipe texts 
model_dutch <- udpipe_download_model(language = "dutch") 
model_dutch <- udpipe_load_model(file = model_dutch$file_model)

for (name in column_names) {
  variable_name_dutch <- paste0(name, "_dutch")
  assign(variable_name_dutch, udpipe_annotate(model_dutch, x = get(name)))
  assign(variable_name_dutch, as.data.frame(get(variable_name_dutch)))
}


# ------------------------ Estonian ----------------------------

# Find the row corresponding to the language "Estonian"
line_estonian <- yourfile[yourfile[, 1] == "Estonian", ]

# Remove the first column (languages) from the row
line_estonian <- line_estonian[, -1]

# Get the column names
column_names <- colnames(line_estonian)

# For each column, create a variable with the column name (using only the first word)
for (column in column_names) {
  variable_name <- sub(" .*", "", column)  # Use only the first word
  assign(variable_name, line_estonian[[column]][1])
}

column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# Loop to remove "\n", "\" and """
for (column_name in column_names) {
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
}

# Estonian: Create a language model and udpipe texts 
model_estonian <- udpipe_download_model(language = "estonian") 
model_estonian <- udpipe_load_model(file = model_estonian$file_model)

for (name in column_names) {
  variable_name_estonian <- paste0(name, "_estonian")
  assign(variable_name_estonian, udpipe_annotate(model_estonian, x = get(name)))
  assign(variable_name_estonian, as.data.frame(get(variable_name_estonian)))
}


# ------------------------ Finnish ----------------------------

# Find the row corresponding to the language "Finnish"
line_finnish <- yourfile[yourfile[, 1] == "Finnish", ]

# Remove the first column (languages) from the row
line_finnish <- line_finnish[, -1]

# Get the column names
column_names <- colnames(line_finnish)

# For each column, create a variable with the column name (using only the first word)
for (column in column_names) {
  variable_name <- sub(" .*", "", column)  # Use only the first word
  assign(variable_name, line_finnish[[column]][1])
}

column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# Loop to remove "\n", "\" and """
for (column_name in column_names) {
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
}

# Finnish: Create a language model and udpipe texts 
model_finnish <- udpipe_download_model(language = "finnish") 
model_finnish <- udpipe_load_model(file = model_finnish$file_model)

for (name in column_names) {
  variable_name_finnish <- paste0(name, "_finnish")
  assign(variable_name_finnish, udpipe_annotate(model_finnish, x = get(name)))
  assign(variable_name_finnish, as.data.frame(get(variable_name_finnish)))
}

# ------------------------ German ----------------------------

# Find the row corresponding to the language "German"
line_german <- yourfile[yourfile[, 1] == "German", ]

# Remove the first column (languages) from the row
line_german <- line_german[, -1]

# Get the column names
column_names <- colnames(line_german)

# For each column, create a variable with the column name (using only the first word)
for (column in column_names) {
  variable_name <- sub(" .*", "", column)  # Use only the first word
  assign(variable_name, line_german[[column]][1])
}

column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# Loop to remove "\n", "\" and """
for (column_name in column_names) {
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
}

# German: Create a language model and udpipe texts 
model_german <- udpipe_download_model(language = "german") 
model_german <- udpipe_load_model(file = model_german$file_model)

for (name in column_names) {
  variable_name_german <- paste0(name, "_german")
  assign(variable_name_german, udpipe_annotate(model_german, x = get(name)))
  assign(variable_name_german, as.data.frame(get(variable_name_german)))
}

# ------------------------ Greek ----------------------------
# Find the row corresponding to the language "Greek"
line_greek <- yourfile[yourfile[, 1] == "Greek", ]

# Remove the first column (languages) from the row
line_greek <- line_greek[, -1]

# Get the column names
column_names <- colnames(line_greek)

# For each column, create a variable with the column name (using only the first word)
for (column in column_names) {
  variable_name <- sub(" .*", "", column)  # Use only the first word
  assign(variable_name, line_greek[[column]][1])
}

column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# Loop to remove "\n", "\" and """
for (column_name in column_names) {
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
}

# Greek: Create a language model and udpipe texts 
model_greek <- udpipe_download_model(language = "greek") 
model_greek <- udpipe_load_model(file = model_greek$file_model)

for (name in column_names) {
  variable_name_greek <- paste0(name, "_greek")
  assign(variable_name_greek, udpipe_annotate(model_greek, x = get(name)))
  assign(variable_name_greek, as.data.frame(get(variable_name_greek)))
}

# ------------------------ Hebrew ----------------------------
# Find the row corresponding to the language "Hebrew"
line_hebrew <- yourfile[yourfile[, 1] == "Hebrew", ]

# Remove the first column (languages) from the row
line_hebrew <- line_hebrew[, -1]

# Get the column names
column_names <- colnames(line_hebrew)

# For each column, create a variable with the column name (using only the first word)
for (column in column_names) {
  variable_name <- sub(" .*", "", column)  # Use only the first word
  assign(variable_name, line_hebrew[[column]][1])
}

column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# Loop to remove "\n", "\" and """
for (column_name in column_names) {
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
}

# Hebrew: Create a language model and udpipe texts 
model_hebrew <- udpipe_download_model(language = "hebrew") 
model_hebrew <- udpipe_load_model(file = model_hebrew$file_model)

for (name in column_names) {
  variable_name_hebrew <- paste0(name, "_hebrew")
  assign(variable_name_hebrew, udpipe_annotate(model_hebrew, x = get(name)))
  assign(variable_name_hebrew, as.data.frame(get(variable_name_hebrew)))
}
# ------------------------ Italian ----------------------------
# Find the row corresponding to the language "Italian"
line_italian <- yourfile[yourfile[, 1] == "Italian", ]

# Remove the first column (languages) from the row
line_italian <- line_italian[, -1]

# Get the column names
column_names <- colnames(line_italian)

# For each column, create a variable with the column name (using only the first word)
for (column in column_names) {
  variable_name <- sub(" .*", "", column)  # Use only the first word
  assign(variable_name, line_italian[[column]][1])
}

column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# Loop to remove "\n", "\" and """
for (column_name in column_names) {
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
}

# Italian: Create a language model and udpipe texts 
model_italian <- udpipe_download_model(language = "italian") 
model_italian <- udpipe_load_model(file = model_italian$file_model)

for (name in column_names) {
  variable_name_italian <- paste0(name, "_italian")
  assign(variable_name_italian, udpipe_annotate(model_italian, x = get(name)))
  assign(variable_name_italian, as.data.frame(get(variable_name_italian)))
}
# ------------------------ Korean ----------------------------
# Find the row corresponding to the language "Korean"
line_korean <- yourfile[yourfile[, 1] == "Korean", ]

# Remove the first column (languages) from the row
line_korean <- line_korean[, -1]

# Get the column names
column_names <- colnames(line_korean)

# For each column, create a variable with the column name (using only the first word)
for (column in column_names) {
  variable_name <- sub(" .*", "", column)  # Use only the first word
  assign(variable_name, line_korean[[column]][1])
}

column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# Loop to remove "\n", "\" and """
for (column_name in column_names) {
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
}

# Korean: Create a language model and udpipe texts 
model_korean <- udpipe_download_model(language = "korean") 
model_korean <- udpipe_load_model(file = model_korean$file_model)

for (name in column_names) {
  variable_name_korean <- paste0(name, "_korean")
  assign(variable_name_korean, udpipe_annotate(model_korean, x = get(name)))
  assign(variable_name_korean, as.data.frame(get(variable_name_korean)))
}
# ------------------------ Norwegian ----------------------------
# Find the row corresponding to the language "Norwegian"
line_norwegian <- yourfile[yourfile[, 1] == "Norwegian", ]

# Remove the first column (languages) from the row
line_norwegian <- line_norwegian[, -1]

# Get the column names
column_names <- colnames(line_norwegian)

# For each column, create a variable with the column name (using only the first word)
for (column in column_names) {
  variable_name <- sub(" .*", "", column)  # Use only the first word
  assign(variable_name, line_norwegian[[column]][1])
}

column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# Loop to remove "\n", "\" and """
for (column_name in column_names) {
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
}

# Norwegian: Create a language model and udpipe texts 
model_norwegian <- udpipe_download_model(language = "norwegian-nynorsk") 
model_norwegian <- udpipe_load_model(file = model_norwegian$file_model)

for (name in column_names) {
  variable_name_norwegian <- paste0(name, "_norwegian")
  assign(variable_name_norwegian, udpipe_annotate(model_norwegian, x = get(name)))
  assign(variable_name_norwegian, as.data.frame(get(variable_name_norwegian)))
}
# ------------------------ Russian ----------------------------
# Find the row corresponding to the language "Russian"
line_russian <- yourfile[yourfile[, 1] == "Russian", ]

# Remove the first column (languages) from the row
line_russian <- line_russian[, -1]

# Get the column names
column_names <- colnames(line_russian)

# For each column, create a variable with the column name (using only the first word)
for (column in column_names) {
  variable_name <- sub(" .*", "", column)  # Use only the first word
  assign(variable_name, line_russian[[column]][1])
}

column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# Loop to remove "\n", "\" and """
for (column_name in column_names) {
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
}

# Russian: Create a language model and udpipe texts 
model_russian <- udpipe_download_model(language = "russian") 
model_russian <- udpipe_load_model(file = model_russian$file_model)

for (name in column_names) {
  variable_name_russian <- paste0(name, "_russian")
  assign(variable_name_russian, udpipe_annotate(model_russian, x = get(name)))
  assign(variable_name_russian, as.data.frame(get(variable_name_russian)))
}
# ------------------------ Spanish ----------------------------
# Find the row corresponding to the language "Spanish"
line_spanish <- yourfile[yourfile[, 1] == "Spanish", ]

# Remove the first column (languages) from the row
line_spanish <- line_spanish[, -1]

# Get the column names
column_names <- colnames(line_spanish)

# For each column, create a variable with the column name (using only the first word)
for (column in column_names) {
  variable_name <- sub(" .*", "", column)  # Use only the first word
  assign(variable_name, line_spanish[[column]][1])
}

column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# Loop to remove "\n", "\" and """
for (column_name in column_names) {
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
}

# Spanish: Create a language model and udpipe texts 
model_spanish <- udpipe_download_model(language = "spanish") 
model_spanish <- udpipe_load_model(file = model_spanish$file_model)

for (name in column_names) {
  variable_name_spanish <- paste0(name, "_spanish")
  assign(variable_name_spanish, udpipe_annotate(model_spanish, x = get(name)))
  assign(variable_name_spanish, as.data.frame(get(variable_name_spanish)))
}
# ------------------------ Turkish ----------------------------
# Find the row corresponding to the language "Turkish"
line_turkish <- yourfile[yourfile[, 1] == "Turkish", ]

# Remove the first column (languages) from the row
line_turkish <- line_turkish[, -1]

# Get the column names
column_names <- colnames(line_turkish)

# For each column, create a variable with the column name (using only the first word)
for (column in column_names) {
  variable_name <- sub(" .*", "", column)  # Use only the first word
  assign(variable_name, line_turkish[[column]][1])
}

column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# Loop to remove "\n", "\" and """
for (column_name in column_names) {
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
}

# Turkish: Create a language model and udpipe texts 
model_turkish <- udpipe_download_model(language = "turkish") 
model_turkish <- udpipe_load_model(file = model_turkish$file_model)

for (name in column_names) {
  variable_name_turkish <- paste0(name, "_turkish")
  assign(variable_name_turkish, udpipe_annotate(model_turkish, x = get(name)))
  assign(variable_name_turkish, as.data.frame(get(variable_name_turkish)))
}
# ------------------------ Lexico-syntactic variables ----------------------------
# ------------------------ Noun-to-Verb Ratio ----------------------------

column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# List of languages
languages <- c("english", "dutch", "estonian", "finnish",
               "german", "greek", "hebrew", "italian", "korean",
               "norwegian", "russian", "spanish", "turkish")

# Create an empty data frame to store the results
noun_to_verb <- tibble(
  name_text = character(),
  num_noun = integer(),
  num_verbs = integer(),
  ratio = double(),
  language = character()
)

# External loop for languages
for (language in languages) {
  # Internal loop for variables
  for (name in column_names) {
    # Construct variable names based on language
    name_lang <- paste0(name, "_", language)
    
    # Retrieve data frames from the global environment
    df_lang <- get(name_lang, envir = .GlobalEnv)
    
    # Filter rows based on the condition
    count_lang <- df_lang %>%
      filter(upos %in% c("VERB", "NOUN")) %>%
      group_by(upos) %>%
      summarise(occurrences = n())
    
    # Extract counts from the count data frame
    num_noun_lang <- count_lang$occurrences[count_lang$upos == "NOUN"]
    num_verb_lang <- count_lang$occurrences[count_lang$upos == "VERB"]
    
    # Calculate the ratio
    ratio_lang <- num_noun_lang / num_verb_lang
    
    # Add the information to noun_to_verb
    noun_to_verb <- bind_rows(
      noun_to_verb,
      tibble(
        name_text = name_lang,
        num_noun = num_noun_lang,
        num_verbs = num_verb_lang,
        ratio = ratio_lang,
        language = language
      )
    )
  }
}



# ------------------------ Type-Token Ratio of Words (TTR-w) ----------------------------
column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# List of languages
languages <- c("english", "dutch", "estonian", "finnish",
               "german", "greek", "hebrew", "italian", "korean",
               "norwegian", "russian", "spanish", "turkish")

# Create an empty data frame to store the results
type_token_words <- tibble(
  column_name = character(),
  language = character(),
  num_types = integer(),
  num_tokens = integer(),
  ratio = double()
)

# External loop for languages
for (language in languages) {
  # Internal loop for variables
  for (name in column_names) {
    # Create variable names
    variable_name <- paste0(name, "_", language)
    
    # Retrieve data frame
    df <- get(variable_name)
    
    # Filter out rows with upos as punctuation
    df <- df[df$upos != "PUNCT", ]
    
    # Calculate num_types and num_tokens
    num_types <- length(unique(df$token))
    num_tokens <- nrow(df)
    
    ratio <- num_types / num_tokens
    
    # Add information to type_token_words
    type_token_words <- bind_rows(
      type_token_words,
      tibble(
        column_name = name,
        language = language,
        num_types = num_types,
        num_tokens = num_tokens,
        ratio = ratio
      )
    )
  }
}

# ------------------------ Type-Token Ratio DPOS (TTR_p) ----------------------------
# Create an empty data frame to store the results
type_token_dpos <- tibble(
  column_name = character(),
  language = character(),
  num_p_types = integer(),
  ttr_p = double()
)

# External loop for languages
for (language in languages) {
  # Internal loop for variables
  for (name in column_names) {
    # Create variable names
    variable_name <- paste0(name, "_", language)
    
    # Retrieve data frame
    df <- get(variable_name)
    
    # Filter out rows with upos as punctuation
    df <- df[df$upos != "PUNCT", ]
    
    # Calculate num_p_types
    num_p_types <- length(unique(df$upos))
    
    # Calculate num_tokens for TTR_w
    n_tokens <- nrow(df)
    
    # Calculate TTR_p
    ttr_p <- num_p_types / n_tokens
    
    # Add information to type_token_dpos
    type_token_dpos <- bind_rows(
      type_token_dpos,
      tibble(
        column_name = name,
        language = language,
        num_p_types = num_p_types,
        ttr_p = ttr_p
      )
    )
  }
}
# ------------------------ Type-Token Ratio Dep (TTR_d) ----------------------------
# Create an empty data frame to store the results
type_token_dep <- tibble(
  column_name = character(),
  language = character(),
  num_d_types = integer(),
  ttr_d = double()
)

# External loop for languages
for (language in languages) {
  # Internal loop for variables
  for (name in column_names) {
    # Create variable names
    variable_name <- paste0(name, "_", language)
    
    # Retrieve data frame
    df <- get(variable_name)
    
    # Filter out rows with upos as punctuation
    df <- df[df$upos != "PUNCT", ]
    
    # Calculate num_d_types
    num_d_types <- length(unique(df$dep_rel))
    
    # Calculate num_tokens for TTR_w
    n_tokens <- nrow(df)
    
    # Calculate TTR_d
    ttr_d <- num_d_types / n_tokens
    
    # Add information to type_token_dep
    type_token_dep <- bind_rows(
      type_token_dep,
      tibble(
        column_name = name,
        language = language,
        num_d_types = num_d_types,
        ttr_d = ttr_d
      )
    )
  }
}

# ------------------------ Dependency Ratio (d-ratio) ----------------------------
# Create an empty data frame to store the results
dependency_ratio <- tibble(
  column_name = character(),
  language = character(),
  d_ratio = double()
)

# External loop for languages
for (language in languages) {
  # Internal loop for variables
  for (name in column_names) {
    # Create variable names
    variable_name <- paste0(name, "_", language)
    
    # Retrieve data frame
    df <- get(variable_name)
    
    # Calculate n_heads
    n_heads <- length(unique(df$head_token_id))
    
    # Calculate n_tokens
    n_tokens <- nrow(df)
    
    # Calculate d_ratio
    d_ratio <- 1 - (n_heads / n_tokens)
    
    # Add information to dependency_ratio
    dependency_ratio <- bind_rows(
      dependency_ratio,
      tibble(
        column_name = name,
        language = language,
        d_ratio = d_ratio
      )
    )
  }
}
# ------------------------ Mean Length of Utterance (MLU) ----------------------------
# Create an empty data frame to store the results
mlu_data <- tibble(
  column_name = character(),
  language = character(),
  mlu = double()
)

# External loop for languages
for (language in languages) {
  # Internal loop for variables
  for (name in column_names) {
    # Create variable names
    variable_name <- paste0(name, "_", language)
    
    # Retrieve data frame
    df <- get(variable_name)
    
    # Filter out rows with dep_rel as punctuation
    df <- df[df$dep_rel != "PUNCT", ]
    
    # Calculate n_words
    n_words <- length(unique(df$token_id))
    
    # Calculate mlu
    mlu <- mean(n_words)
    
    # Add information to mlu_data
    mlu_data <- bind_rows(
      mlu_data,
      tibble(
        column_name = name,
        language = language,
        mlu = mlu
      )
    )
  }
}
# ------------------------ Embeddedness ----------------------------
# Create an empty data frame to store the results
embeddedness_data <- tibble(
  column_name = character(),
  language = character(),
  embeddedness = double(),
  num_complex_sentences = double(),
  num_simplex_sentences = double(),
  num_sentences = double()
)

# External loop for languages
for (language in languages) {
  # Internal loop for variables
  for (name in column_names) {
    # Create variable names
    variable_name <- paste0(name, "_", language)
    
    # Retrieve data frame
    df <- get(variable_name)
    
    # Identify complex sentences
    df <- df %>%
      group_by(doc_id, sentence_id) %>% 
      mutate(is_complex = if_else(
        any(dep_rel %in% c("parataxis", "xcomp", "ccomp", "advcl", "acl:relcl", "acl", "conj", "cc", "mark")), 1, 0)
      ) %>% 
      ungroup()
    
    # Keep only the first row of each group (slice(1))
    df_complexity <- df %>%
      group_by(doc_id, sentence_id) %>% 
      slice(1) %>% 
      select(is_complex) %>% 
      ungroup()
    
    # Calculate Embeddedness
    num_complex_sentences <- sum(df_complexity$is_complex)
    num_simplex_sentences <- nrow(df_complexity) - num_complex_sentences
    embeddedness <- 1 - (num_complex_sentences / num_simplex_sentences)
    
    # Add information to embeddedness_data
    embeddedness_data <- bind_rows(
      embeddedness_data,
      tibble(
        column_name = name,
        language = language,
        embeddedness = embeddedness,
        num_complex_sentences = num_complex_sentences,
        num_simplex_sentences = num_simplex_sentences,
        num_sentences = nrow(df_complexity)
      )
    )
  }
}


# ------------------------ Longest dependency path (LDP) ----------------------------
# Create an empty data frame to store the results
depth_data <- tibble(
  column_name = character(),
  language = character(),
  sentence_id = integer(),
  depth = double()
)

# External loop for languages
for (language in languages) {
  # Internal loop for variables
  for (name in column_names) {
    # Create variable names
    variable_name <- paste0(name, "_", language)
    
    # Retrieve data frame
    df <- get(variable_name)
    
    # Create doc_sent_id
    df$doc_sent_id <- str_c(df$doc_id, df$sentence_id, sep = "_")
    
    # Initialize my_depth
    my_depth <- tibble(doc_sent_id = unique(df$doc_sent_id), depth = as.numeric(NA))
    
    # Calculate max depth for each sentence
    for (s in unique(df$doc_sent_id)) {
      tmp_depth <- df %>% filter(doc_sent_id == s) %>% droplevels()
      
      # Calculate max depth 
      x <- tmp_depth[!is.na(tmp_depth$head_token_id), ]
      x <- x[x$sentence_id %in% min(x$sentence_id), ]
      edges <- x[x$head_token_id != 0, c("token_id", "head_token_id")]
      
      # Get number of branches
      max_depth <- igraph::diameter(igraph::graph_from_data_frame(edges,
                                                                  vertices = x[, c("token_id")],
                                                                  directed = TRUE))
      
      my_depth[my_depth$doc_sent_id == s,]$depth <- max_depth
    }
    
    # Join with tmp_doc_id
    tmp_doc_id <- df %>% select(doc_id, doc_sent_id) %>% distinct(doc_sent_id, .keep_all = TRUE)
    my_depth <- left_join(my_depth, tmp_doc_id, by = "doc_sent_id")
    
    # Extract sentence_id
    my_depth$sentence_id <- as.numeric(substr(my_depth$doc_sent_id, 1 + regexpr(pattern = "_", text = my_depth$doc_sent_id), nchar(my_depth$doc_sent_id)))
    
    # Add information to depth_data
    depth_data <- bind_rows(
      depth_data,
      tibble(
        column_name = name,
        language = language,
        sentence_id = my_depth$sentence_id,
        depth = my_depth$depth
      )
    )
  }
}

