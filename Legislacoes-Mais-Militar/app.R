# =================================================================
# APLICATIVO DE CONTROLE DE ATUALIZAÇÃO DE LEIS (VERSÃO FINAL COM EDIÇÃO)
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

# Define a estrutura de dados com tipos corretos
DATA_STRUCTURE <- tibble(
  id = integer(),
  nome_lei = character(),
  professor = character(),
  data_gravacao = as.Date(character()),
  url_lei = character(),
  status = factor(levels = c("Atualizado", "Verificando...", "Necessita Atualização", "Erro")),
  ultima_verificacao = as.Date(character())
)

DATA_FILE <- "leis_cadastradas.csv"

# Função para carregar os dados
load_data <- function() {
  if (file.exists(DATA_FILE)) {
    df <- read.csv(DATA_FILE, stringsAsFactors = FALSE)
    df$data_gravacao <- as.Date(df$data_gravacao)
    df$ultima_verificacao <- as.Date(df$ultima_verificacao)
    df$status <- factor(df$status, levels = c("Atualizado", "Verificando...", "Necessita Atualização", "Erro"))
    return(df)
  } else {
    DATA_STRUCTURE
  }
}

# Função para salvar os dados
save_data <- function(data) {
  write.csv(data, DATA_FILE, row.names = FALSE)
}


# --- 3. FUNÇÃO DE VERIFICAÇÃO PROFUNDA (WEB SCRAPING) ---

# Função segura para ler HTML, simulando um navegador
safe_read_html <- function(url) {
  tryCatch({
    response <- httr::GET(
      url,
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/117.0"),
      httr::timeout(20)
    )
    httr::stop_for_status(response)
    return(rvest::read_html(response))
  }, error = function(e) {
    message("Falha ao acessar a URL: ", url, " - Erro: ", e$message)
    return(NULL)
  })
}

# Função para converter data em português
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

# Função principal de verificação
verificar_atualizacao_lei <- function(url_base, data_gravacao) {
  if (is.na(url_base) || nchar(url_base) < 10) return("Erro")
  pagina_base <- safe_read_html(url_base)
  if (is.null(pagina_base)) return("Erro")
  ano_gravacao <- year(data_gravacao)
  links_alteracao <- pagina_base %>% html_nodes("a[href*='Lei/L']")
  necessita_atualizacao <- FALSE
  for (link in links_alteracao) {
    texto_link <- html_text(link)
    ano_alteracao_match <- str_extract(texto_link, "\\b(20\\d{2})\\b")
    if (!is.na(ano_alteracao_match)) {
      ano_alteracao <- as.numeric(ano_alteracao_match)
      if (ano_alteracao >= ano_gravacao) {
        url_relativa <- html_attr(link, "href")
        url_absoluta <- xml2::url_absolute(url_relativa, url_base)
        Sys.sleep(0.5)
        pagina_alteracao <- safe_read_html(url_absoluta)
        if (is.null(pagina_alteracao)) next
        texto_data <- pagina_alteracao %>%
          html_node(xpath = "//a[contains(@href, 'Viw_Identificacao')]") %>%
          html_text()
        if (!is.na(texto_data)) {
          match_data <- str_match(texto_data, "(\\d{1,2} DE [^DE]+ DE \\d{4})")
          if (!is.na(match_data[1, 2])) {
            data_publicacao <- parse_portuguese_date(match_data[1, 2])
            if (!is.na(data_publicacao) && data_publicacao > data_gravacao) {
              necessita_atualizacao <- TRUE
              break 
            }
          }
        }
      }
    }
  }
  if (necessita_atualizacao) return("Necessita Atualização") else return("Atualizado")
}


# --- 4. INTERFACE DO USUÁRIO (UI) ---

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Dashboard de Controle de Leis"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Adicionar Nova Lei"),
      textInput("nome_lei", "Nome da Lei (Ex: ECA)", placeholder = "Estatuto da Criança e do Adolescente"),
      textInput("professor", "Professor(a)", placeholder = "Ex: Prof. Daiane"),
      dateInput("data_gravacao", "Data da Gravação da Aula", value = today(), language = "pt-BR"),
      textInput("url_lei", "URL da Lei no Planalto", placeholder = "https://www.planalto.gov.br/..."),
      actionButton("add_btn", "Adicionar Lei", class = "btn-primary", icon = icon("plus")),
      hr(),
      h4("Ações"),
      p("Selecione uma ou mais leis na tabela para verificar, ou apenas uma para editar."),
      actionButton("verify_btn", "Verificar Selecionadas", class = "btn-success", icon = icon("sync")),
      # --- NOVO BOTÃO DE EDIÇÃO ---
      actionButton("edit_btn", "Editar Selecionada", class = "btn-warning", icon = icon("edit"))
    ),
    
    mainPanel(
      h4("Leis Cadastradas"),
      DTOutput("tabela_leis")
    )
  )
)


# --- 5. LÓGICA DO SERVIDOR (SERVER) ---

server <- function(input, output, session) {
  
  dados_leis <- reactiveVal(load_data())
  
  # Reativo para guardar a linha selecionada para edição
  linha_selecionada <- reactiveVal(NULL)
  
  # Salva os dados sempre que forem alterados
  observe({
    req(dados_leis())
    save_data(dados_leis())
  })
  
  # Lógica para ADICIONAR nova lei
  observeEvent(input$add_btn, {
    # Lógica de adicionar (sem alterações)
    if (nchar(trimws(input$nome_lei)) == 0 || nchar(trimws(input$professor)) == 0 || nchar(trimws(input$url_lei)) == 0) {
      showNotification("Por favor, preencha todos os campos.", type = "warning"); return()
    }
    tryCatch({
      current_data <- dados_leis(); new_id <- if (nrow(current_data) == 0) 1 else max(current_data$id) + 1
      updated_data <- tibble::add_row(.data = current_data, id = new_id, nome_lei = trimws(input$nome_lei), professor = trimws(input$professor), data_gravacao = input$data_gravacao, url_lei = trimws(input$url_lei), status = factor("Atualizado", levels = levels(current_data$status)), ultima_verificacao = today())
      dados_leis(updated_data)
      updateTextInput(session, "nome_lei", value = ""); updateTextInput(session, "professor", value = ""); updateTextInput(session, "url_lei", value = "")
      showNotification("Lei adicionada com sucesso!", type = "message")
    }, error = function(e) {
      showNotification(paste("Erro ao adicionar a lei:", e$message), type = "error", duration = 10)
    })
  })
  
  # --- NOVA LÓGICA PARA ABRIR A JANELA DE EDIÇÃO ---
  observeEvent(input$edit_btn, {
    selected_rows <- input$tabela_leis_rows_selected
    
    # Valida se exatamente uma linha foi selecionada
    if (length(selected_rows) != 1) {
      showModal(modalDialog(title = "Atenção", "Por favor, selecione exatamente uma lei para editar.", easyClose = TRUE))
      return()
    }
    
    # Guarda a linha selecionada e os dados dela
    linha_selecionada(selected_rows)
    lei_para_editar <- dados_leis()[selected_rows, ]
    
    # Mostra a janela modal com os campos preenchidos
    showModal(modalDialog(
      title = "Editar Informações da Lei",
      textInput("edit_nome_lei", "Nome da Lei", value = lei_para_editar$nome_lei),
      textInput("edit_professor", "Professor(a)", value = lei_para_editar$professor),
      dateInput("edit_data_gravacao", "Nova Data da Gravação", value = lei_para_editar$data_gravacao, language = "pt-BR"),
      textInput("edit_url_lei", "URL da Lei", value = lei_para_editar$url_lei),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("save_edit_btn", "Salvar Alterações", class = "btn-primary")
      )
    ))
  })
  
  # --- NOVA LÓGICA PARA SALVAR AS ALTERAÇÕES ---
  observeEvent(input$save_edit_btn, {
    req(linha_selecionada())
    
    dados_atuais <- dados_leis()
    row_index <- linha_selecionada()
    
    # Atualiza os dados no dataframe
    dados_atuais$nome_lei[row_index] <- trimws(input$edit_nome_lei)
    dados_atuais$professor[row_index] <- trimws(input$edit_professor)
    dados_atuais$data_gravacao[row_index] <- input$edit_data_gravacao
    dados_atuais$url_lei[row_index] <- trimws(input$edit_url_lei)
    
    # Importante: Reseta o status para "Atualizado"
    dados_atuais$status[row_index] <- "Atualizado"
    dados_atuais$ultima_verificacao[row_index] <- today()
    
    # Atualiza o reativo, que por sua vez atualiza a tabela e salva o arquivo
    dados_leis(dados_atuais)
    
    # Limpa a seleção e fecha a janela
    linha_selecionada(NULL)
    removeModal()
    showNotification("Lei atualizada com sucesso!", type = "message")
  })
  
  # Lógica para EXIBIR a tabela
  output$tabela_leis <- renderDT({
    # Lógica de exibição da tabela (sem alterações)
    datatable(dados_leis(), rownames = FALSE, selection = 'multiple', options = list(pageLength = 10, order = list(list(0, 'desc')), language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json'))) %>%
      formatStyle('status', backgroundColor = styleEqual(c("Atualizado", "Necessita Atualização", "Erro", "Verificando..."), c("#d4edda", "#f8d7da", "#fff3cd", "#cce5ff")))
  })
  
  # Lógica para VERIFICAR as leis
  observeEvent(input$verify_btn, {
    # Lógica de verificação (sem alterações)
    selected_rows <- input$tabela_leis_rows_selected
    if (is.null(selected_rows)) {
      showModal(modalDialog(title = "Atenção", "Por favor, selecione pelo menos uma lei para verificar.", easyClose = TRUE)); return()
    }
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