# Computational Social Science fall semester 2023
# Project by Kevin Schläpfer and Simon Bernhard


# setwd("C:/Users/Simon Bernhard/OneDrive - Universitaet Bern/Dokumente/Studium/Sozialwissenschaften/HS23 SOWI/Computational social science - accessing and using digital data and technologies")

library(httr)
library(jsonlite)
library(tidyverse)

# API info ----------------------------------------------------------------
headers <- c(
  "X-API-Key" = "Your_API_Key",
  "X-API-Secret" = "Your_API_Secret"
)
API_BASE_URL <- "https://swissdox.linguistik.uzh.ch/api"
API_URL_QUERY <- paste(API_BASE_URL, "/query", sep = "")
API_URL_DOWNLOAD <- paste(API_BASE_URL, "/download/bd2705ca-9054-4231-98b3-c8da56ed6406__2023_11_15T11_51_09.tsv.xz", sep = "")

# Query: ------------------------------------------------------------------
css <- "
    query:
        sources:
            - ZWA
            - ZWAO
            - BZ
            - BLI
            - BLIA
            - BU
            - WEW
            - WOZ
            - NZZS
            - NZZ
            - SWII
            - SWO
            - SBLI
            - TA
            - NNBE
            - BLIO
            - BLIAO
            - NNBU
            - NZZO
            - SRF
            - NNTA
            - WDAY
            - WEWO
            - AZ
            - AZO
            - LUZ
            - LUZO
            - SGT
            - BAZ
        dates:
            - from: 2010-01-01
              to: 2023-11-15
        languages:
            - de
        content:
            AND:
                - OR:
                    - Flücht*
                    - Asyl*
                    - Immig*
                    - flücht*
                    - asyl*
                    - immig*
    result:
        format: TSV
        maxResults: 100000
        columns:
            - id
            - pubtime
            - medium_code
            - medium_name
            - rubric
            - regional
            - doctype
            - doctype_description
            - language
            - char_count
            - dateline
            - head
            - subhead
            - content_id
            - content
    version: 1.2
" # build a query

data <- list(
  query = css,
  name = "media2023",
  comment = "Datenbank der Medienberichte über Immigration",
  expirationDate = "2024-03-28"
)

r <- POST(
  url = API_URL_QUERY,
  add_headers(.headers = headers),
  body = data
)

print(content(r, "parsed")) # check if it worked


# Downloading dataset -----------------------------------------------------
library(readr)

r <- GET(
  url = API_URL_DOWNLOAD,
  add_headers(.headers = headers)
) # retrieve the data fom the download link

if (status_code(r) == 200) {
  print(paste("Size of file: ", sprintf("%.2f KB", length(content(r, "raw")) / 1024)))
  fp <- file("./dataset.tsv.xz", "wb")
  writeBin(content(r, "raw"), fp)
  close(fp)
} else {
  print(content(r, "text"))
} # download compressed data to working directory

data <- read_delim("dataset.tsv.xz", delim = "\t", col_types = cols()) # decompress and load the dataset


# Data tidying -------------------------------------------------------------


articles <- data %>%
  select(
    ID = id,
    date = pubtime,
    medium = medium_name,
    text = content,
    title = head,
    count = char_count
  ) # create a dataset, keeping only the relevant variables

articles <- articles %>%
  mutate(date = as.Date(date, "%Y-%m-%d", tz = "Europe/Zurich")) # format date as date


articles$text <- gsub("<.{0,6}>", "", articles$text) # deletes Javascript stuff


# save(articles, file = "articles_raw_data.RData")


# Build Corpus ------------------------------------------------------------


library(htmltools)
library(seededlda)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
if (!require("quanteda.sentiment")) {
  remotes::install_github("quanteda/quanteda.sentiment", force = TRUE)
}
library(quanteda.sentiment)
library(stopwords)
library(scales)


corpus_articles <-
  corpus(
    select(
      articles,
      text,
      ID,
      date,
      medium,
      title,
      count
    ),
    text_field = "text"
  ) # build a corpus, keeping only the relevant variables

# save(corpus_articles, file = "corpus_articles.RData")


# Tokens ------------------------------------------------------------------


articles_toks <- tokens(corpus_articles,
  what = c("word"),
  remove_separators = TRUE,
  include_docvars = TRUE,
  ngrams = 1L,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_hyphens = TRUE
) # tokenizing the new corpus, for better processable data



# Data cleaning -----------------------------------------------------------


mystopwords <- c(
  stopwords("de"), "dass", "SDA", "sda", "beim", "href", "fc",
  "dpa", "DPA", "seit", "pd", "mehr", "schon", "sei", "viel",
  "zwei", "ab", "statt", "erst", "kurz", "neu", "gross", "ganz",
  "lang", "gemass", "gemäss", "uhr", "lag", "muss", "zeit",
  "soll", "sagt", "bereit", "eins", "drei", "schön", "weit",
  "laut", "seien", "dritte", "dritter", "dritt", "stark", "ja",
  "wenig", "mal", "zeit", "ab", "rund", "möglich", "moglich", "jung",
  "alt", "vre", "sowie", "wurde", "würde", "wurden", "gibt", "worden", "bild"
) # build a custom stopwords vector to remove them from tokens


articles_toks <-
  articles_toks %>%
  tokens_remove(mystopwords,
    padding = TRUE
  ) # remove custom stopwords

articles_toks <-
  articles_toks %>%
  tokens_wordstem(language = "de") # stem tokens


articles_toks <-
  articles_toks %>%
  tokens_remove(
    pattern = "\\b\\w{1}\\b",
    valuetype = "regex"
  ) # remove single character tokens


articles_toks <-
  articles_toks %>%
  tokens_remove("") # remove empty tokens


articles_toks <-
  articles_toks %>%
  tokens_tolower # transform tokens to lowercase

# save(articles_toks, file = "articles_tokens.RData")



# DFM ---------------------------------------------------------------------


articles_dfm <- dfm(articles_toks) # build dfm

# save(articles_dfm, file = "articles_dfm.RData")



# LDA ---------------------------------------------------------------------


set.seed(123)
LDA <- textmodel_lda(articles_dfm, k = 10, max_iter = 100) # fit lda with k = 10 topics

terms(LDA, 10) # take a look at LDA terms/topics

# saveRDS(LDA, file = "LDA.rds")

# to load LDA use command: LDA <- readRDS("LDA.rds")

df.articles <- docvars(corpus_articles) %>% as.data.frame()
df.articles$topic <- topics(LDA) # buid a dataframe with an added column containing the LDA topic

filtered_articles <- df.articles %>%
  filter(topic == "topic3" | topic == "topic7" | topic == "topic10") # keep only the articles from the relevant topics


filtered_articles <- filtered_articles %>%
  distinct(ID, .keep_all = T) # only keep articles with unique IDs (Live Ticker articles and articles which got updated several times
# had the same ID for each version) to enable joining them with the original articles

articles <- articles %>%
  distinct(ID, .keep_all = T) # delete articles with duplicate IDs in original dataset for the same reason as stated above


filtered_original_articles <-
  semi_join(articles, filtered_articles, by = "ID") # semijoin the articles filtered by relevant topic with the original versions
# now we have a filtered dataset with only the relevant articles


filtered_original_articles <- cbind(filtered_original_articles, filtered_articles["topic"]) # add a column containing the LDA topic to the filtered original dataset


# save(filtered_original_articles, file = "filtered_original_articles.RData")



# corpus,tokens and dfm with the filtered dataset  -------------------------------


corpus_filtered_articles <-
  corpus(
    select(
      filtered_original_articles,
      text,
      ID,
      date,
      medium,
      title,
      count
    ),
    text_field = "text",
    docid_field = "ID",
    unique_docnames = FALSE
  )


filtered_articles_toks <- tokens(corpus_filtered_articles,
  what = c("word"),
  remove_separators = TRUE,
  include_docvars = TRUE,
  ngrams = 1L,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_hyphens = TRUE
)

filtered_articles_toks <-
  filtered_articles_toks %>%
  tokens_remove(stopwords(language = "de"),
    padding = TRUE
  ) # remove custom stopwords

filtered_articles_toks <-
  filtered_articles_toks %>%
  tokens_wordstem(language = "de") # stem tokens


filtered_articles_toks <-
  filtered_articles_toks %>%
  tokens_remove(
    pattern = "\\b\\w{1}\\b",
    valuetype = "regex"
  ) # remove single character tokens


filtered_articles_toks <-
  filtered_articles_toks %>%
  tokens_remove("") # remove empty tokens


filtered_articles_toks <-
  filtered_articles_toks %>%
  tokens_tolower # transform tokens to lowercase


dfm_filtered_articles <- dfm(filtered_articles_toks) # build dfm



# Dictionary --------------------------------------------------------------


sentim <- read.delim("https://github.com/guyemerson/SentiMerge/raw/master/data/sentimerge.txt", sep = "") # read in sentiment dictionary

sentim <- sentim %>%
  select(-PoS, -weight) %>%
  rename(word = lemma) %>%
  mutate(word = str_replace_all(word, "\\d", "")) %>%
  mutate(sentiment = str_replace_all(sentiment, "[A-Za-z]", "")) %>%
  na.omit() %>%
  mutate(sentiment = as.numeric(sentiment)) %>%
  filter(between(sentiment, -1, 1)) # tidying the sentiment dictionary

senti_pos <- sentim %>%
  filter(sentiment > 0) # set positive sentiments

senti_neg <- sentim %>%
  filter(sentiment < 0) # set negative sentiments

dict_positive <- dictionary(list(positive = senti_pos$word)) # create a positive dictionary

dict_negative <- dictionary(list(negative = senti_neg$word)) # create a negative dictionary

final_dictionary <- dictionary(c(dict_positive, dict_negative)) # join them for the final dictionary


# Apply dictionary --------------------------------------------------------


sentiment <- dfm_filtered_articles %>%
  dfm_lookup(dictionary = final_dictionary) # apply the sentiment dictionary to the dfm

sentiment <- sentiment %>%
  convert(to = "data.frame") %>%
  mutate(doc_id = str_remove(doc_id, "\\.1")) %>%
  rename(ID = doc_id) %>%
  mutate(
    length = ntoken(dfm_filtered_articles),
    sentiment.score = (positive - negative) / length
  ) %>%
  mutate(sentiment.score.z = scale(sentiment.score)) # tidying up a new data frame, calculating the sentiment score and the z-standardized sentiment score.

summary(sentiment$sentiment.score.z) # check if all worked



# prep for Analysis -------------------------------------------------------


filtered_original_articles <- filtered_original_articles %>%
  mutate(ID = as.character(ID)) # format ID as character

analysis <- left_join(filtered_original_articles, sentiment, by = "ID") # join sentiment and filtered articles



# Immigration Data --------------------------------------------------------
library(readxl)
library(rvest)

base_url <- "https://www.sem.admin.ch/dam/sem/de/data/publiservice/statistik/asylstatistik/" # specify the base URL
dest_folder <- getwd() # specify destination folder

for (year in 2013:2022) {
  for (month in 1:12) { # create data frames for each year and month
    download_link <- paste0(base_url, year, "/", sprintf("%02d", month), "/7-10-Bew-Einreiseantraege-WEG-M-d-", year, "-", sprintf("%02d", month), ".xlsx.download.xlsx/7-10-Bew-Einreiseantraege-WEG-M-d-", year, "-", sprintf("%02d", month), ".xlsx") # Generate the download link for the specific year and month

    dest_file <- paste0(dest_folder, "asylstat", year, "-", sprintf("%02d", month), ".xlsx") # generate the destination file name for the specific year and month

    download.file(download_link, destfile = dest_file, mode = "wb") # download the file

    asylstat_data <- read_xlsx(dest_file, range = "CH-Nati!A7:B180", col_names = c("Herkunft", paste0(year, "-", sprintf("%02d", month)))) # read the needed data from Excel file into R

    assign(paste0("asylstat", year, sprintf("%02d", month)), asylstat_data) # create the data frames

    cat("Downloaded and read:", dest_file, "\n") # print a message indicating the status
  }
}

filtered_data_frames <- list() # create an empty list to store the filtered data frames

for (year in 2013:2022) {
  for (month in 1:12) { # loop through each year and filter rows based on specified names
    current_df <- get(paste0("asylstat", year, sprintf("%02d", month))) # Extract the data frame for the specific year

    filtered_df <- current_df %>%
      filter(Herkunft %in% c("Gesamttotal", "Afrika", "Nordafrika", "Subsahara", "Amerika", "Asien", "Europa", "Ozeanien", "Ukraine")) # filter rows based on specified names

    filtered_data_frames[[paste0("asylstat", year, sprintf("%02d", month))]] <- filtered_df # store the filtered data frame in the list
  }
}

asylum_statistic_origin <- filtered_data_frames %>%
  reduce(full_join, by = "Herkunft") %>%
  arrange(Herkunft) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) # perform a full join on the filtered data frames

asylum_statistic_origin <- asylum_statistic_origin %>%
  pivot_longer(cols = -Herkunft, names_to = "year_month", values_to = "value") %>%
  mutate(year_month = as.character(year_month)) %>%
  pivot_wider(names_from = Herkunft, values_from = value) # clean the data and pivot from wide to long



# Regression Analysis -----------------------------------------------------
library(stargazer)

z.month <- analysis %>%
  mutate(year_month = format(date, "%Y-%m")) %>%
  distinct(ID, year_month, sentiment.score.z) %>%
  select(-ID) %>%
  group_by(year_month) # format the df to fit the other data


z.month <- z.month %>%
  filter(between(year_month, "2013-01", "2022-12")) # select the right time period

ra <- merge(z.month, asylum_statistic_origin, by = "year_month") %>%
  mutate(
    date = paste0(year_month, "-01") %>% as.Date(),
    year_month = NULL
  ) %>%
  select(date, everything()) # merge the to datasets for the regression analysis

model1 <- lm(sentiment.score.z ~ Gesamttotal, data = ra)

model2 <- lm(sentiment.score.z ~ Afrika + Amerika + Asien + Europa + Ozeanien, data = ra) # calculate both regression models

summary(model1)
summary(model2) # display both models

regression_table1 <- stargazer(model1, type = "html", title = "Linear Regression Results", out = "regression_results1.html")
regression_table2 <- stargazer(model2, type = "html", title = "Linear Regression Results", out = "regression_results2.html") # convert them to html code, to be able to use them in a paper


# Visualization -----------------------------------------------------------


library(ggplot2)
library(hrbrthemes)
library(viridis)


# Barplot Frequencies of sentiments

analysis %>%
  mutate(senti_categorical = case_when(
    sentiment.score.z >= 0.3 ~ "positive",
    sentiment.score.z < 0.3 & sentiment.score.z > -0.3 ~ "neutral",
    sentiment.score.z <= -0.3 ~ "negative"
  )) %>% # categorize sentiment scores in 3 categories
  group_by(senti_categorical) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>% # add a percentage variable
  ggplot(aes(x = senti_categorical, y = percentage, fill = senti_categorical)) +
  labs(
    title = "Distribution of sentiment scores in 3 categories",
    x = "Sentiment",
    y = "Percentage"
  ) +
  guides(fill = "none") +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_bw()

# ggsave("barplot_distribution_senti_scores.png")


# Lineplot showing the Frequencies of sentiment categories over time

analysis %>%
  mutate(
    date = as.Date(date),
    senti_categorical = case_when(
      sentiment.score.z >= 0.3 ~ "positive",
      sentiment.score.z < 0.3 & sentiment.score.z > -0.3 ~ "neutral",
      sentiment.score.z <= -0.3 ~ "negative"
    )
  ) %>% # categorize sentiment scores in 3 categories
  group_by(month = lubridate::floor_date(date, unit = "month"), senti_categorical) %>% # group by month to see trends
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>% # add a percentage variable
  ggplot(aes(x = month, y = percentage, color = senti_categorical)) +
  labs(
    title = "Frequencies of sentiment categories over time",
    x = "Year",
    y = "Percentage",
    color = "sentiment score"
  ) +
  geom_line(size = 1.65) +
  scale_x_date(
    date_breaks = "1 year", labels = scales::date_format("%Y"),
    limits = c(as.Date("2010-01-01"), as.Date("2023-11-15"))
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_bw() +
  theme(
    panel.grid.minor.x = element_blank(), # Remove minor gridlines
    panel.grid.major.x = element_line(color = "grey", linetype = c(1, 2), size = c(0.5, 0.2))
  )

# ggsave("lineplot_freq_senti_score.png")


# Stacked area plot showing the Frequencies of sentiment categories over time

analysis %>%
  mutate(
    date = as.Date(date),
    senti_categorical = case_when(
      sentiment.score.z >= 0.3 ~ "positive",
      sentiment.score.z < 0.3 & sentiment.score.z > -0.3 ~ "neutral",
      sentiment.score.z <= -0.3 ~ "negative"
    )
  ) %>%
  group_by(month = lubridate::floor_date(date, unit = "month"), senti_categorical) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>% # add a percentage variable
  ggplot(aes(x = month, y = percentage, fill = senti_categorical)) +
  geom_area(alpha = 0.7, size = 1, colour = "black", position = "fill") +
  labs(
    title = "Frequencies of sentiment categories over time",
    x = "Year",
    y = "Percentage",
    fill = "sentiment score"
  ) +
  scale_x_date(
    date_breaks = "2 year", labels = scales::date_format("%Y"),
    limits = c(as.Date("2010-01-01"), as.Date("2023-11-15"))
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_fill_manual(values = c("positive" = "#00BA38", "neutral" = "#619CFF", "negative" = "#F8766D")) +
  theme_ipsum() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))


# ggsave("densitiy_plot_senti_score.png")



# Line Plot showing the Average Sentiment Score Over Time

ggplot(ra, aes(x = date, y = sentiment.score.z, group = 1)) +
  geom_line(color = "black", size = 1) +
  geom_smooth(color = "blue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Red line at y = 0
  labs(
    title = "Average Sentiment Score Over Time",
    x = "Year",
    y = "Average Sentiment Score"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # Label every year on the x-axis
  theme_bw()

# ggsave("lineplot_average_senti_score.png")



# Line Plot showing the Correlation Over Time

ggplot(
  ra %>%
    mutate(
      standardized_Gesamttotal = scale(Gesamttotal) %>% rescale(to = range(sentiment.score.z)),
      centered_scaled_Gesamttotal = standardized_Gesamttotal - mean(standardized_Gesamttotal),
      centered_scaled_avg_sentiment_score = sentiment.score.z - mean(sentiment.score.z)
    ),
  aes(x = date)
) +
  geom_smooth(aes(y = centered_scaled_Gesamttotal, color = "Gesamttotal"), size = 1) +
  geom_smooth(aes(y = centered_scaled_avg_sentiment_score, color = "sentiment.score.z")) +
  labs(
    title = "Correlation Over Time",
    x = "Year",
    y = "Centered and Scaled Values",
    color = "Variable"
  ) +
  scale_color_manual(values = c("Gesamttotal" = "blue", "sentiment.score.z" = "red")) +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # Label every year on the x-axis
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better visibility


# ggsave("lineplot_correlation.png")
