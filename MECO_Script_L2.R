# Load the udpipe library if not already loaded
# install.packages("udpipe")
library(udpipe)

# Load the RDA file
load("C:/Users/buzat/Downloads/texts.meco.l2 (1).rda")

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
