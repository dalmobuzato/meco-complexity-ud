rm(list = ls())
library(readr)
library(udpipe)
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)

# Load the CSV file.
#yourfile <- read_csv("C:/Users/buzat/Downloads/supp_texts.csv")
yourfile <- readxl::read_xlsx("supp_texts.xlsx")
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

for (column_name in column_names) {
  # Loop to remove "\n", "\" and """
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
  
  # Replace !, ?, and ; with .
  assign(column_name, gsub("[!\\?;]", ".", get(column_name)))
  
}

# English: Create a language model and udpipe texts 
model_english <- udpipe_download_model(language = "english-gum") 
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

for (column_name in column_names) {
  # Loop to remove "\n", "\" and """
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
  
  # Replace !, ?, and ; with .
  assign(column_name, gsub("[!\\?;]", ".", get(column_name)))
  
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

for (column_name in column_names) {
  # Loop to remove "\n", "\" and """
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
  
  # Replace !, ?, and ; with .
  assign(column_name, gsub("[!\\?;]", ".", get(column_name)))
  
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

for (column_name in column_names) {
  # Loop to remove "\n", "\" and """
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
  
  # Replace !, ?, and ; with .
  assign(column_name, gsub("[!\\?;]", ".", get(column_name)))
  
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

for (column_name in column_names) {
  # Loop to remove "\n", "\" and """
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
  
  # Replace !, ?, and ; with .
  assign(column_name, gsub("[!\\?;]", ".", get(column_name)))
  
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

for (column_name in column_names) {
  # Loop to remove "\n", "\" and """
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
  
  # Replace !, ?, and ; with .
  assign(column_name, gsub("[!\\?;]", ".", get(column_name)))
  
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

for (column_name in column_names) {
  # Loop to remove "\n", "\" and """
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
  
  # Replace !, ?, and ; with .
  assign(column_name, gsub("[!\\?;]", ".", get(column_name)))
  
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

for (column_name in column_names) {
  # Loop to remove "\n", "\" and """
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
  
  # Replace !, ?, and ; with .
  assign(column_name, gsub("[!\\?;]", ".", get(column_name)))
  
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

for (column_name in column_names) {
  # Loop to remove "\n", "\" and """
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
  
  # Replace !, ?, and ; with .
  assign(column_name, gsub("[!\\?;]", ".", get(column_name)))
  
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

for (column_name in column_names) {
  # Loop to remove "\n", "\" and """
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
  
  # Replace !, ?, and ; with .
  assign(column_name, gsub("[!\\?;]", ".", get(column_name)))
  
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

for (column_name in column_names) {
  # Loop to remove "\n", "\" and """
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
  
  # Replace !, ?, and ; with .
  assign(column_name, gsub("[!\\?;]", ".", get(column_name)))
  
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

for (column_name in column_names) {
  # Loop to remove "\n", "\" and """
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
  
  # Replace !, ?, and ; with .
  assign(column_name, gsub("[!\\?;]", ".", get(column_name)))
  
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

for (column_name in column_names) {
  # Loop to remove "\n", "\" and """
  assign(column_name, gsub("[\"\\\\\n]", "", get(column_name)))
  
  # Replace !, ?, and ; with .
  assign(column_name, gsub("[!\\?;]", ".", get(column_name)))
  
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
noun_to_verb_sentence <- tibble(
  sentence_id = character(),
  num_noun = integer(),
  num_verbs = integer(),
  n_v_ratio = double(),
  language = character(),
  column_name = character()
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
      group_by(sentence_id, upos) %>%
      summarise(occurrences = n())
    
    # Pivot the data frame to wide format for easy ratio calculation
    count_lang_wide <- count_lang %>%
      spread(upos, occurrences, fill = 0)
    
    # Calculate the ratio
    count_lang_wide <- count_lang_wide %>%
      mutate(ratio = NOUN / VERB)  # Use column names directly
    
    # Summarize by sentence_id
    count_lang_summarized <- count_lang_wide %>%
      group_by(sentence_id) %>%
      summarise(
        num_noun = sum(NOUN),
        num_verbs = sum(VERB),
        n_v_ratio = sum(NOUN) / sum(VERB),
        language = language,
        column_name = name
      ) 
    
    # Convert sentence_id to character before combining data frames
    count_lang_summarized$sentence_id <- as.character(count_lang_summarized$sentence_id)
    
    # Add the information to noun_to_verb_sentence
    noun_to_verb_sentence <- bind_rows(noun_to_verb_sentence, count_lang_summarized)
  }
}

# Remove rows with empty or null sentence_id
noun_to_verb_sentence <- noun_to_verb_sentence %>%
  filter(!is.na(sentence_id) & sentence_id != "")

table(noun_to_verb_sentence$num_verbs == 0, noun_to_verb_sentence$language)
#quite a few sentences have no lexical verbs in them.

# ------------------------ Type-Token Ratio of Words (TTR-w) ----------------------------

column_names <- c('Janus', 'Shaka', 'Doping', 
                  'Thylacine', 'World', 'Monocole', 'Wine', 'Orange',
                  'Beekeeping', 'National', 'International', 'Vehicle')

# List of languages
languages <- c("english", "dutch", "estonian", "finnish",
               "german", "greek", "hebrew", "italian", "korean",
               "norwegian", "russian", "spanish", "turkish")

# Create an empty data frame to store the results
type_token_words_sentence <- tibble(
  sentence_id = character(),
  num_types = integer(),
  num_tokens = integer(),
  ratio = double(),
  language = character(),
  column_name = character()
)

# External loop for languages
for (language in languages) {
  # Internal loop for variables
  for (name in column_names) {
    # Construct variable names based on language
    name_lang <- paste0(name, "_", language)
    
    # Retrieve data frames from the global environment
    df_lang <- get(name_lang, envir = .GlobalEnv)
    
    # Filter out rows with upos as punctuation
    df_lang <- df_lang[df_lang$upos != "PUNCT", ]
    
    # Calculate num_types and num_tokens
    count_lang <- df_lang %>%
      group_by(sentence_id) %>%
      summarise(
        num_types = length(unique(token)),
        num_tokens = n(),
        ratio = length(unique(token)) / n(),
        language = language,
        column_name = name
      )
    
    # Convert sentence_id to character before combining data frames
    count_lang$sentence_id <- as.character(count_lang$sentence_id)
    
    # Add the information to type_token_words
    type_token_words_sentence <- bind_rows(type_token_words_sentence, count_lang)
  }
}

# Remove rows with empty or null sentence_id
type_token_words_sentence <- type_token_words_sentence %>%
  filter(!is.na(sentence_id) & sentence_id != "")



# ------------------------ Type-Token Ratio DPOS (TTR_p) ----------------------------

# Create an empty data frame to store the results
type_token_dpos_sentence <- tibble(
  sentence_id = character(),
  num_p_types = integer(),
  ttr_p = double(),
  language = character(),
  column_name = character()
)

# External loop for languages
for (language in languages) {
  # Internal loop for variables
  for (name in column_names) {
    # Construct variable names based on language
    name_lang <- paste0(name, "_", language)
    
    # Retrieve data frames from the global environment
    df_lang <- get(name_lang, envir = .GlobalEnv)
    
    # Filter out rows with upos as punctuation
    df_lang <- df_lang[df_lang$upos != "PUNCT", ]
    
    # Calculate num_p_types
    count_lang <- df_lang %>%
      group_by(sentence_id) %>%
      summarise(
        num_p_types = length(unique(upos)),
        ttr_p = length(unique(upos)) / n(),
        language = language,
        column_name = name
      )
    
    # Convert sentence_id to character before combining data frames
    count_lang$sentence_id <- as.character(count_lang$sentence_id)
    
    # Add the information to type_token_dpos
    type_token_dpos_sentence <- bind_rows(type_token_dpos_sentence, count_lang)
  }
}

# Remove rows with empty or null sentence_id
type_token_dpos_sentence <- type_token_dpos_sentence %>%
  filter(!is.na(sentence_id) & sentence_id != "")


# ------------------------ Type-Token Ratio Dep (TTR_d) ----------------------------

# Create an empty data frame to store the results
type_token_dep_sentence <- tibble(
  sentence_id = character(),
  num_d_types = integer(),
  ttr_d = double(),
  language = character(),
  column_name = character()
)

# External loop for languages
for (language in languages) {
  # Internal loop for variables
  for (name in column_names) {
    # Construct variable names based on language
    name_lang <- paste0(name, "_", language)
    
    # Retrieve data frames from the global environment
    df_lang <- get(name_lang, envir = .GlobalEnv)
    
    # Filter out rows with upos as punctuation
    df_lang <- df_lang[df_lang$upos != "PUNCT", ]
    
    # Calculate num_d_types
    count_lang <- df_lang %>%
      group_by(sentence_id) %>%
      summarise(
        num_d_types = length(unique(dep_rel)),
        ttr_d = length(unique(dep_rel)) / n(),
        language = language,
        column_name = name
      )
    
    # Convert sentence_id to character before combining data frames
    count_lang$sentence_id <- as.character(count_lang$sentence_id)
    
    # Add the information to type_token_dep
    type_token_dep_sentence <- bind_rows(type_token_dep_sentence, count_lang)
  }
}

# Remove rows with empty or null sentence_id
type_token_dep_sentence <- type_token_dep_sentence %>%
  filter(!is.na(sentence_id) & sentence_id != "")


# ------------------------ Dependency Ratio (d-ratio) ----------------------------
# Create an empty data frame to store the results
dependency_ratio_sentence <- tibble(
  sentence_id = character(),
  language = character(),
  d_ratio = double(),
  column_name = character()
)

# External loop for languages
for (language in languages) {
  # Internal loop for variables
  for (name in column_names) {
    # Construct variable names based on language
    name_lang <- paste0(name, "_", language)
    
    # Retrieve data frame
    df_lang <- get(name_lang, envir = .GlobalEnv)
    
    # Calculate n_heads and n_tokens per sentence_id
    head_token_counts <- df_lang %>%
      group_by(sentence_id) %>%
      summarise(
        n_heads = length(unique(head_token_id)),
        n_tokens = n()
      )
    
    # Calculate d_ratio per sentence_id
    head_token_counts <- head_token_counts %>%
      mutate(d_ratio = 1 - (n_heads / n_tokens),
             language = language,
             column_name = name)
    
    # Convert sentence_id to character before combining data frames
    head_token_counts$sentence_id <- as.character(head_token_counts$sentence_id)
    
    # Add the information to dependency_ratio_sentence
    dependency_ratio_sentence <- bind_rows(dependency_ratio_sentence, head_token_counts)
  }
}

# Remove rows with empty or null sentence_id
dependency_ratio_sentence <- dependency_ratio_sentence %>%
  filter(!is.na(sentence_id) & sentence_id != "")



# ------------------------ Mean Length of Utterance (MLU) ----------------------------
# Create an empty data frame to store the results
mlu_sentence <- tibble(
  sentence_id = character(),
  language = character(),
  mlu = double(),
  column_name = character()
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
    
    # Ensure sentence_id is character type
    df$sentence_id <- as.character(df$sentence_id)
    
    # Calculate mlu per sentence_id
    mlu_per_sentence <- df %>%
      group_by(sentence_id) %>%
      summarise(
        mlu = mean(length(unique(token_id))),
        language = language,
        column_name = name
      )
    
    # Add information to mlu_sentence
    mlu_sentence <- bind_rows(mlu_sentence, mlu_per_sentence)
  }
}

# Remove rows with empty or null sentence_id
mlu_sentence <- mlu_sentence %>%
  filter(!is.na(sentence_id) & sentence_id != "")


# ------------------------ Embeddedness ----------------------------
# Create an empty data frame to store the results
embeddedness_sentence <- tibble(
  sentence_id = character(),
  language = character(),
  embeddedness = double(),
  num_complex_sentences = double(),
  num_simplex_sentences = double(),
  num_sentences = double(),
  column_name = character()
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
      select(is_complex, sentence_id) %>%  # Ensure the sentence_id column is selected
      ungroup()
    
    # Ensure sentence_id is character type
    df_complexity$sentence_id <- as.character(df_complexity$sentence_id)
    
    # Calculate Embeddedness per sentence_id
    num_complex_sentences <- sum(df_complexity$is_complex)
    num_simplex_sentences <- nrow(df_complexity) - num_complex_sentences
    embeddedness <- 1 - (num_complex_sentences / num_simplex_sentences)
    
    # Add information to embeddedness_sentence
    embeddedness_sentence <- bind_rows(
      embeddedness_sentence,
      tibble(
        sentence_id = unique(df_complexity$sentence_id),
        language = language,
        embeddedness = embeddedness,
        num_complex_sentences = num_complex_sentences,
        num_simplex_sentences = num_simplex_sentences,
        num_sentences = nrow(df_complexity),
        column_name = name
      )
    )
  }
}

# Remove rows with empty or null sentence_id
embeddedness_sentence <- embeddedness_sentence %>%
  filter(!is.na(sentence_id) & sentence_id != "")

# ------------------------ Longest dependency path (LDP) ----------------------------
# Create an empty data frame to store the results
depth_data_sentence <- tibble(
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
    depth_data_sentence <- bind_rows(
      depth_data_sentence,
      tibble(
        column_name = name,
        language = language,
        sentence_id = my_depth$sentence_id,
        depth = my_depth$depth
      )
    )
  }
}


save(noun_to_verb_sentence, type_token_words_sentence, type_token_dpos_sentence, 
     type_token_dep_sentence, dependency_ratio_sentence, mlu_sentence, 
     embeddedness_sentence, depth_data_sentence, file = "sentence_complexity.rda",
     compress = "xz")

rm(list = ls())
load('sentence_complexity.rda')
depth_data_sentence$sentence_id = as.character(depth_data_sentence$sentence_id)

list_df = list(dependency_ratio_sentence, 
               depth_data_sentence, 
               embeddedness_sentence,
               mlu_sentence,
               noun_to_verb_sentence,
               type_token_dep_sentence,
               type_token_dpos_sentence,
               type_token_words_sentence
               )
all_sentence <- list_df %>% reduce(full_join, by = c("language", 
                                            "column_name",
                                            "sentence_id"))

save(all_sentence, file = "sentence_summary.rda")
