rm(list)
if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyRSS, tidytext, wordcloud2, 
               ggplot2, dplyr, tidyRSS, httr,
               xml2, tidytext, dplyr, tidyverse,
               tidytext, tm , blastula)

# 1. Obter notícias via RSS
rss_url <- "https://g1.globo.com/dynamo/rss2.xml"

# 2. Baixar o conteúdo do feed RSS
response <- GET(rss_url)

if (status_code(response) == 200) {
  # Parsear o XML
  feed <- content(response, as = "text", encoding = "UTF-8")
  xml <- read_xml(feed)
  
  # Extração manual de títulos e descrições
  titles <- xml_find_all(xml, "//item/title") %>% xml_text()
  descriptions <- xml_find_all(xml, "//item/description") %>% xml_text()
  
  # Combinar em um dataframe
  news <- data.frame(
    title = titles,
    description = descriptions,
    stringsAsFactors = FALSE
  )
  print("Acesso a RSS efetuado") # Visualizar as primeiras linhas
} else {
  print("Não foi possível acessar o feed RSS.")
}

# 3.  Preparar os textos: combinar títulos e descrições
news$text <- paste(news$title, news$description)

# 4. Limpar texto
custom_stopwords <- c(stopwords("pt"), "globo", "notícia", "g1",
                      "glbimg.com", "com", "https", "http", "html",
                      "após", "img", "src", "auth_59edd422c0c84a879bd37670ae4f538a",
                      "i.s3", "internal_photos", "sobre", "ser", "segundo", "ser",
                      "ainda", "sobre", "anos", "segunda",
                      "nesta", "desta", "disse", "dois", "três",
                      "dia", "mês", "meio", "além", "josé", "ano",
                      "duas", "seis", "três", "quadro", "bem", "sete",
                      "disso", "cerca", "ter", "lucia", "aqui", "png",
                      "antes", "toda", "ler", "leia", "sob", "maria",
                      "mil", "faz", "disso", "jpeg", "jpg"
                      )



# Pipeline de limpeza e tokenização
cleaned_words <- news %>%
  # 1. Tokenizar o texto em palavras
  unnest_tokens(word, text) %>%
  ## trim space
  mutate(word = str_trim(word,side ="both" )) %>%
  filter(!word %in% custom_stopwords) %>%
  # 2. Remover stopwords (em português)
  filter(!word %in% stopwords("pt")) %>%
  # 3. Remover números
  filter(!str_detect(word, "^[0-9]+$")) %>%
  # 4. Remover palavras curtas (ex.: "a", "é", "de") com menos de 3 letras
  filter(str_length(word) > 2) %>%
  mutate(word = str_to_lower(word)) 
#|> 
  #mutate(word =  stemDocument(word)) 

word_counts <- cleaned_words %>%
  count(word, sort = TRUE)

# Criar a nuvem de palavras limpa
wordcloud2(word_counts, size = 0.7, 
           color = "random-light")

# Definir palavras-chave relacionadas à saúde
health_keywords <- c("saúde", "doença", "epidemia", "surto", "vírus", "contágio", "tratamento", 
                     "cura", "prevenção", "vacina", "sintoma", "infecção", "pandemia", "hospitais", 
                     "cura", "tratamento", "mortalidade", "transmissão", "COVID", "vacinação", "brote",
                     "assistência",
                     "hospital", "médico", "tratamento",
                     "cura", "isolamento", "monitoramento")

# Contar as palavras de interesse
health_word_counts <- cleaned_words %>%
  filter(word %in% health_keywords) %>%
  count(word, sort = TRUE)


# Plotar gráfico de barras
ggplot(health_word_counts, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip()+
  ## custom
  theme_minimal()+
  labs(title = "Palavras Relacionadas à Saúde mais Frequentes", 
       x = "Palavra", y = "Contagem")

palavras <- unique(health_word_counts$word)

health_words <- news %>%
  unnest_tokens(word, text) %>%
  filter(word %in% health_keywords) %>%
  group_by(title) %>%
  count(word, sort = TRUE) |> 
  filter(word %in% palavras) %>%
  arrange(desc(n))

emails_df <- read_excel("lista_emails.xlsx")

library(blastula)

# Substitua pela sua senha de aplicativo gerada no Google
senha_app <- "sua_senha_de_aplicativo_aqui"

# Configuração do servidor SMTP
# Sending email by SMTP using a credentials file
date_time <- add_readable_time()



create_smtp_creds_key(
     id = "gmail_creds",
   provider = "gmail",
     user = "cmusso86@gmail.com",
   overwrite = T
     )

#rmarkdown::
body_content <- readLines("relatorio_saude.Rmd")
  
email <- compose_email(
    header = md("**Análise de Notícias - Palavras Relacionadas à Saúde**"),
    body = md(paste(body_content, collapse = "\n"))
  )
  
email |> smtp_send(
  to = "cmusso86@gmail.com",
  from = "cmusso86@gmail.com",
  subject = "TLala",
  credentials = creds_key("gmail_creds")
)
