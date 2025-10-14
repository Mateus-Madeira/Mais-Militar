# =================================================================
# APLICATIVO DE CONTROLE DE ATUALIZAÇÃO DE LEIS (VERSÃO FINAL E ROBUSTA)
# =================================================================

# --- 1. PACOTES NECESSÁRIOS ---
# install.packages(c("shiny", "dplyr", "DT", "rvest", "lubridate", "bslib", "stringr", "xml2", "purrr", "httr"))

library(shiny)
library(dplyr)
library(DT)
library(rvest)
library(lubridate)
library(bslib)
library(stringr)
library(xml2)
library(purrr)
library(httr)


# --- 2. CONFIGURAÇÃO DOS DADOS ---

DATA_STRUCTURE <- tibble(
  id = integer(), nome_lei = character(), professor = character(),
  data_gravacao = as.Date(character()), url_lei = character(),
  status = factor(levels = c("Atualizado", "Verificando...", "Necessita Atualização", "Erro")),
  ultima_verificacao = as.Date(character())
)
DATA_FILE <- "leis_cadastradas.csv"

load_data <- function() {
  if (file.exists(DATA_FILE)) {
    df <- read.csv(DATA_FILE, stringsAsFactors = FALSE)
    status_levels <- c("Atualizado", "Verificando...", "Necessita Atualização", "Erro")
    if (!"data_gravacao" %in% names(df)) df$data_gravacao <- as.Date(NA) else df$data_gravacao <- as.Date(df$data_gravacao)
    if (!"ultima_verificacao" %in% names(df)) df$ultima_verificacao <- as.Date(NA) else df$ultima_verificacao <- as.Date(df$ultima_verificacao)
    if (!"status" %in% names(df)) df$status <- factor(NA, levels = status_levels) else df$status <- factor(df$status, levels = status_levels)
    return(df)
  } else {
    DATA_STRUCTURE
  }
}

save_data <- function(data) {
  write.csv(data, DATA_FILE, row.names = FALSE, quote = TRUE)
}


# --- 3. FUNÇÃO DE VERIFICAÇÃO PROFUNDA (WEB SCRAPING) ---

safe_read_html <- function(url) {
  url <- stringr::str_trim(url)
  url <- stringr::str_remove_all(url, "^[<\\[]|[>\\]]$")  # remove colchetes/ângulos colados da cópia
  tryCatch({
    resp <- httr::RETRY(
      "GET", url,
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) R (httr/rvest)"),
      httr::timeout(20),
      times = 3, pause_min = 0.5
    )
    httr::stop_for_status(resp)
    raw <- httr::content(resp, as = "raw")
    xml2::read_html(raw, base_url = url)
  }, error = function(e) {
    message("Falha ao acessar a URL: ", url, " - Erro: ", e$message)
    NULL
  })
}


parse_portuguese_date <- function(date_string) {
  if (is.na(date_string)) return(NA)
  current_locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", current_locale))
  try_locales <- c("Portuguese_Brazil.1252", "pt_BR.UTF-8", "pt_BR", "Portuguese")
  for (l in try_locales) {
    try(Sys.setlocale("LC_TIME", l), silent = TRUE)
    parsed_date <- as.Date(tolower(date_string), format = "%d de %B de %Y")
    if (!is.na(parsed_date)) return(parsed_date)
  }
  return(NA)
}

verificar_atualizacao_lei <- function(url_base, data_gravacao) {
  url_limpa <- str_squish(url_base)
  if (is.na(url_limpa) || !startsWith(url_limpa, "http")) {
    message(sprintf("ERRO: URL inválida ou vazia fornecida: '%s'", url_base))
    return("Erro")
  }
  
  message("\n----------------------------------------------------")
  message("Iniciando verificação para: ", url_limpa)
  
  if (!grepl("planalto.gov.br", url_limpa)) {
    message("   - ERRO: A URL não é do site planalto.gov.br. Verificação não suportada.")
    return("Erro")
  }
  
  pagina_base <- safe_read_html(url_limpa)
  if (is.null(pagina_base)) { message("ERRO: Falha ao carregar a página principal."); return("Erro") }
  
  ano_gravacao <- year(data_gravacao)
  necessita_atualizacao <- FALSE
  
  # --- MUDANÇA FINAL E MAIS ROBUSTA ---
  # Seletor CSS que busca por qualquer link que vá para uma página de Lei ou Decreto.
  # Esta é a abordagem mais ampla e segura.
  seletor_css <- "a[href*='/lei/'], a[href*='/Lei/'], a[href*='/decreto/'], a[href*='/Decreto/']"
  links_alteracao <- pagina_base %>% html_nodes(css = seletor_css)
  
  for (link in links_alteracao) {
    href <- html_attr(link, "href")
    # Para extrair o ano, pegamos o texto do próprio link e também do nó "pai" (o parágrafo <p> inteiro).
    # Isso captura casos onde o ano está fora do texto do link.
    texto_contexto <- link %>% xml_parent() %>% html_text() %>% str_squish()
    ano_alteracao_match <- str_extract(texto_contexto, "\\b(20\\d{2})\\b") # Busca por anos 20xx
    
    # Se não encontrar ano 20xx, busca qualquer ano de 4 dígitos para leis mais antigas.
    if (is.na(ano_alteracao_match)) {
      ano_alteracao_match <- str_extract(texto_contexto, "\\b(\\d{4})\\b")
    }
    
    if (!is.na(ano_alteracao_match)) {
      ano_alteracao <- as.numeric(ano_alteracao_match)
      
      # Compara o ano da alteração com o ano da gravação da aula
      if (ano_alteracao >= ano_gravacao) {
        message(sprintf("   - Link suspeito encontrado para o ano %d. Verificando a data exata...", ano_alteracao))
        url_absoluta <- xml2::url_absolute(href, url_limpa)
        Sys.sleep(0.5) # Pausa para não sobrecarregar o servidor
        pagina_alteracao <- safe_read_html(url_absoluta)
        if (is.null(pagina_alteracao)) next
        
        # Tenta extrair o texto completo do cabeçalho da lei/decreto
        texto_data_node <- pagina_alteracao %>% html_node(xpath = "//p[contains(., 'DECRETO Nº') or contains(., 'LEI Nº')]")
        
        if (!is.na(texto_data_node)) {
          texto_data <- html_text(texto_data_node)
          match_data <- str_match(texto_data, "(\\d{1,2} DE .*? DE \\d{4})")
          if (!is.na(match_data[1, 2])) {
            data_publicacao <- parse_portuguese_date(match_data[1, 2])
            if (!is.na(data_publicacao) && data_publicacao > data_gravacao) {
              message(sprintf("   *** ATUALIZAÇÃO NECESSÁRIA! Data da norma: %s > Data da gravação: %s ***", data_publicacao, data_gravacao))
              necessita_atualizacao <- TRUE; break
            }
          }
        }
      }
    }
  }
  status_final <- if (necessita_atualizacao) "Necessita Atualização" else "Atualizado"
  message("Verificação concluída. Status final: ", status_final)
  return(status_final)
}

# --- 4. INTERFACE DO USUÁRIO (UI) ---

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Dashboard de Controle de Leis"),
  sidebarLayout(
    sidebarPanel(
      h4("Adicionar Nova Lei"),
      textInput("nome_lei", "Nome da Lei", placeholder = "Ex: Código Penal"),
      textInput("professor", "Professor(a)", placeholder = "Ex: Prof. Daiane"),
      dateInput("data_gravacao", "Data da Gravação", value = today(), language = "pt-BR"),
      textInput("url_lei", "URL da Lei no Planalto", placeholder = "https://www.planalto.gov.br/..."),
      actionButton("add_btn", "Adicionar Lei", class = "btn-primary", icon = icon("plus")),
      hr(), h4("Ações"),
      p("Selecione uma ou mais leis para verificar, ou apenas uma para editar."),
      actionButton("verify_btn", "Verificar Selecionadas", class = "btn-success", icon = icon("sync")),
      actionButton("edit_btn", "Editar Selecionada", class = "btn-warning", icon = icon("edit"))
    ),
    mainPanel(h4("Leis Cadastradas"), DTOutput("tabela_leis"))
  )
)

# --- 5. LÓGICA DO SERVIDOR (SERVER) ---

server <- function(input, output, session) {
  dados_leis <- reactiveVal(load_data()); linha_selecionada <- reactiveVal(NULL)
  observe({ req(dados_leis()); save_data(dados_leis()) })
  observeEvent(input$add_btn, {
    if (nchar(trimws(input$nome_lei)) == 0 || nchar(trimws(input$professor)) == 0 || nchar(trimws(input$url_lei)) == 0) {
      showNotification("Por favor, preencha todos os campos.", type = "warning"); return()
    }
    tryCatch({
      current_data <- dados_leis(); new_id <- if (nrow(current_data) == 0) 1 else max(current_data$id) + 1
      updated_data <- tibble::add_row(.data = current_data, id = new_id, nome_lei = trimws(input$nome_lei), professor = trimws(input$professor), data_gravacao = input$data_gravacao, url_lei = trimws(input$url_lei), status = factor("Atualizado", levels = levels(current_data$status)), ultima_verificacao = today())
      dados_leis(updated_data)
      updateTextInput(session, "nome_lei", value = ""); updateTextInput(session, "professor", value = ""); updateTextInput(session, "url_lei", value = "")
      showNotification("Lei adicionada com sucesso!", type = "message")
    }, error = function(e) { showNotification(paste("Erro:", e$message), type = "error", duration = 10) })
  })
  observeEvent(input$edit_btn, {
    selected_rows <- input$tabela_leis_rows_selected
    if (length(selected_rows) != 1) { showModal(modalDialog(title = "Atenção", "Selecione uma lei para editar.", easyClose = TRUE)); return() }
    linha_selecionada(selected_rows); lei_para_editar <- dados_leis()[selected_rows, ]
    showModal(modalDialog(
      title = "Editar Informações da Lei",
      textInput("edit_nome_lei", "Nome", value = lei_para_editar$nome_lei),
      textInput("edit_professor", "Professor(a)", value = lei_para_editar$professor),
      dateInput("edit_data_gravacao", "Nova Data", value = lei_para_editar$data_gravacao, language = "pt-BR"),
      textInput("edit_url_lei", "URL", value = lei_para_editar$url_lei),
      footer = tagList(modalButton("Cancelar"), actionButton("save_edit_btn", "Salvar", class = "btn-primary"))
    ))
  })
  observeEvent(input$save_edit_btn, {
    req(linha_selecionada()); dados_atuais <- dados_leis(); row_index <- linha_selecionada()
    dados_atuais$nome_lei[row_index] <- trimws(input$edit_nome_lei)
    dados_atuais$professor[row_index] <- trimws(input$edit_professor)
    dados_atuais$data_gravacao[row_index] <- input$edit_data_gravacao
    dados_atuais$url_lei[row_index] <- trimws(input$edit_url_lei)
    dados_atuais$status[row_index] <- "Atualizado"; dados_atuais$ultima_verificacao[row_index] <- today()
    dados_leis(dados_atuais); linha_selecionada(NULL); removeModal()
    showNotification("Lei atualizada com sucesso!", type = "message")
  })
  output$tabela_leis <- renderDT({
    datatable(dados_leis(), rownames = FALSE, selection = 'multiple', options = list(pageLength = 10, order = list(list(0, 'desc')), language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json'))) %>%
      formatStyle('status', backgroundColor = styleEqual(c("Atualizado", "Necessita Atualização", "Erro", "Verificando..."), c("#d4edda", "#f8d7da", "#fff3cd", "#cce5ff")))
  })
  observeEvent(input$verify_btn, {
    selected_rows <- input$tabela_leis_rows_selected
    if (is.null(selected_rows)) { showModal(modalDialog(title = "Atenção", "Selecione pelo menos uma lei para verificar.", easyClose = TRUE)); return() }
    dados_atuais <- dados_leis(); dados_atuais$status[selected_rows] <- "Verificando..."; dados_leis(dados_atuais)
    withProgress(message = 'Iniciando verificação profunda...', value = 0, {
      for (i in seq_along(selected_rows)) {
        row_index <- selected_rows[i]
        incProgress(1/length(selected_rows), detail = paste("Analisando:", dados_atuais$nome_lei[row_index]))
        novo_status <- verificar_atualizacao_lei(url_base = dados_atuais$url_lei[row_index], data_gravacao = dados_atuais$data_gravacao[row_index])
        dados_atuais$status[row_index] <- novo_status; dados_atuais$ultima_verificacao[row_index] <- today()
      }
    })
    dados_leis(dados_atuais)
    showNotification("Verificação profunda concluída!", type = "message")
  })
}

# --- 6. EXECUTAR O APLICATIVO ---
shinyApp(ui, server)