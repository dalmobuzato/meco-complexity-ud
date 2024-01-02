# Load the udpipe library if not already loaded
# install.packages("udpipe")
library(udpipe)
library(tidyverse)
library(igraph)

# Load the RDA file
load("C:/Users/buzat/Desktop/meco-complexity-ud/texts.meco.l2 (1).rda")

# Keep only the "text" column in the dataframe
d <- d[, "text", drop = FALSE]

# Load the model once, as it doesn't change
model <- udpipe_download_model(language = "english-gum") 
model <- udpipe_load_model(file = model$file_model)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Name the variable based on the row number
  variable_name_udpipe <- paste0("line_", i, "_udpipe")
  
  # Apply udpipe to the current row
  assign(variable_name_udpipe, udpipe_annotate(model, x = current_text))
  assign(variable_name_udpipe, as.data.frame(get(variable_name_udpipe)))
}

# ------------------------ Lexico-syntactic variables ----------------------------
# ------------------------ Noun-to-Verb Ratio ----------------------------
# Create an empty data frame to store the results
noun_to_verb_sentence <- tibble(
  sentence_id = character(),
  num_noun = integer(),
  num_verbs = integer(),
  ratio = double(),
  language = character(),
  column_name = character()
)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Filter rows based on the condition
  count_lang <- current_df %>%
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
      ratio = sum(NOUN) / sum(VERB),
      language = "english-l2",  
      column_name = paste0("line_", i)
    ) 
  
  # Convert sentence_id to character before combining data frames
  count_lang_summarized$sentence_id <- as.character(count_lang_summarized$sentence_id)
  
  # Add the information to noun_to_verb_sentence
  noun_to_verb_sentence <- bind_rows(noun_to_verb_sentence, count_lang_summarized)
}

# Remove rows with empty or null sentence_id
noun_to_verb_sentence <- noun_to_verb_sentence %>%
  filter(!is.na(sentence_id) & sentence_id != "")
# ------------------------ Type-Token Ratio of Words (TTR-w) ----------------------------
# Create an empty data frame to store the results
type_token_words <- tibble(
  sentence_id = character(),
  num_types = integer(),
  num_tokens = integer(),
  ratio = double(),
  language = character(),
  column_name = character()
)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Filter out rows with upos as punctuation
  current_df <- current_df[current_df$upos != "PUNCT", ]
  
  # Calculate num_types and num_tokens
  count_lang <- current_df %>%
    group_by(sentence_id) %>%
    summarise(
      num_types = length(unique(token)),
      num_tokens = n(),
      ratio = length(unique(token)) / n(),
      language = "english-l2",  
      column_name = paste0("line_", i)
    )
  
  # Convert sentence_id to character before combining data frames
  count_lang$sentence_id <- as.character(count_lang$sentence_id)
  
  # Add the information to type_token_words
  type_token_words <- bind_rows(type_token_words, count_lang)
}

# Remove rows with empty or null sentence_id
type_token_words <- type_token_words %>%
  filter(!is.na(sentence_id) & sentence_id != "")

# ------------------------ Type-Token Ratio DPOS (TTR_p) ----------------------------
# Create an empty data frame to store the results
type_token_dpos <- tibble(
  sentence_id = character(),
  num_p_types = integer(),
  ttr_p = double(),
  language = character(),
  column_name = character()
)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Filter out rows with upos as punctuation
  current_df <- current_df[current_df$upos != "PUNCT", ]
  
  # Calculate num_p_types
  count_lang <- current_df %>%
    group_by(sentence_id) %>%
    summarise(
      num_p_types = length(unique(upos)),
      ttr_p = length(unique(upos)) / n(),
      language = "english-l2",  
      column_name = paste0("line_", i)
    )
  
  # Convert sentence_id to character before combining data frames
  count_lang$sentence_id <- as.character(count_lang$sentence_id)
  
  # Add the information to type_token_dpos
  type_token_dpos <- bind_rows(type_token_dpos, count_lang)
}

# Remove rows with empty or null sentence_id
type_token_dpos <- type_token_dpos %>%
  filter(!is.na(sentence_id) & sentence_id != "")
# ------------------------ Type-Token Ratio Dep (TTR_d) ----------------------------
# Create an empty data frame to store the results
type_token_dep <- tibble(
  sentence_id = character(),
  num_d_types = integer(),
  ttr_d = double(),
  language = character(),
  column_name = character()
)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Filter out rows with upos as punctuation
  current_df <- current_df[current_df$upos != "PUNCT", ]
  
  # Calculate num_d_types
  count_lang <- current_df %>%
    group_by(sentence_id) %>%
    summarise(
      num_d_types = length(unique(dep_rel)),
      ttr_d = length(unique(dep_rel)) / n(),
      language = "english-l2", 
      column_name = paste0("line_", i)
    )
  
  # Convert sentence_id to character before combining data frames
  count_lang$sentence_id <- as.character(count_lang$sentence_id)
  
  # Add the information to type_token_dep
  type_token_dep <- bind_rows(type_token_dep, count_lang)
}

# Remove rows with empty or null sentence_id
type_token_dep <- type_token_dep %>%
  filter(!is.na(sentence_id) & sentence_id != "")

# ------------------------ Dependency Ratio (d-ratio) ----------------------------
# Create an empty data frame to store the results
dependency_ratio_sentence <- tibble(
  sentence_id = character(),
  language = character(),
  d_ratio = double(),
  column_name = character()
)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Calculate n_heads and n_tokens per sentence_id
  head_token_counts <- current_df %>%
    group_by(sentence_id) %>%
    summarise(
      n_heads = length(unique(head_token_id)),
      n_tokens = n()
    )
  
  # Calculate d_ratio per sentence_id
  head_token_counts <- head_token_counts %>%
    mutate(
      d_ratio = 1 - (n_heads / n_tokens),
      language = "english-l2",  
      column_name = paste0("line_", i)
    )
  
  # Convert sentence_id to character before combining data frames
  head_token_counts$sentence_id <- as.character(head_token_counts$sentence_id)
  
  # Add the information to dependency_ratio_sentence
  dependency_ratio_sentence <- bind_rows(dependency_ratio_sentence, head_token_counts)
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

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Filter out rows with dep_rel as punctuation
  current_df <- current_df[current_df$dep_rel != "PUNCT", ]
  
  # Ensure sentence_id is character type
  current_df$sentence_id <- as.character(current_df$sentence_id)
  
  # Calculate mlu per sentence_id
  mlu_per_sentence <- current_df %>%
    group_by(sentence_id) %>%
    summarise(
      mlu = mean(length(unique(token_id))),
      language = "english-l2",  
      column_name = paste0("line_", i)
    )
  
  # Add information to mlu_sentence
  mlu_sentence <- bind_rows(mlu_sentence, mlu_per_sentence)
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

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Identify complex sentences
  current_df <- current_df %>%
    group_by(sentence_id) %>% 
    mutate(is_complex = if_else(
      any(dep_rel %in% c("parataxis", "xcomp", "ccomp", "advcl", "acl:relcl", "acl", "conj", "cc", "mark")), 1, 0)
    ) %>% 
    ungroup()
  
  # Keep only the first row of each group (slice(1))
  df_complexity <- current_df %>%
    group_by(sentence_id) %>% 
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
      language = "english-l2",  
      embeddedness = embeddedness,
      num_complex_sentences = num_complex_sentences,
      num_simplex_sentences = num_simplex_sentences,
      num_sentences = nrow(df_complexity),
      column_name = paste0("line_", i)
    )
  )
}

# Remove rows with empty or null sentence_id
embeddedness_sentence <- embeddedness_sentence %>%
  filter(!is.na(sentence_id) & sentence_id != "")

# ------------------------ Longest dependency path (LDP) ----------------------------
# Create an empty data frame to store the results
depth_data <- tibble(
  column_name = character(),
  language = character(),
  sentence_id = integer(),
  depth = double()
)

# Assuming 'd' is your dataframe with a column named "text"
for (i in 1:nrow(d)) {
  # Get the text from the current row
  current_text <- d$text[i]
  
  # Apply udpipe to the current row
  current_annotation <- udpipe_annotate(model, x = current_text)
  current_df <- as.data.frame(current_annotation)
  
  # Create doc_sent_id
  current_df$doc_sent_id <- str_c(current_df$doc_id, current_df$sentence_id, sep = "_")
  
  # Initialize my_depth
  my_depth <- tibble(doc_sent_id = unique(current_df$doc_sent_id), depth = as.numeric(NA))
  
  # Calculate max depth for each sentence
  for (s in unique(current_df$doc_sent_id)) {
    tmp_depth <- current_df %>% filter(doc_sent_id == s) %>% droplevels()
    
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
  tmp_doc_id <- current_df %>% select(doc_id, doc_sent_id) %>% distinct(doc_sent_id, .keep_all = TRUE)
  my_depth <- left_join(my_depth, tmp_doc_id, by = "doc_sent_id")
  
  # Extract sentence_id
  my_depth$sentence_id <- as.numeric(substr(my_depth$doc_sent_id, 1 + regexpr(pattern = "_", text = my_depth$doc_sent_id), nchar(my_depth$doc_sent_id)))
  
  # Add information to depth_data
  depth_data <- bind_rows(
    depth_data,
    tibble(
      column_name = paste0("line_", i),
      language = "english-l2",  
      sentence_id = my_depth$sentence_id,
      depth = my_depth$depth
    )
  )
}