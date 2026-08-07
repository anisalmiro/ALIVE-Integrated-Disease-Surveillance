# SUMÁRIO DE ALTERAÇÕES - AUTO-UPDATE DE DADOS

## 📋 Resumo

Foi implementada uma funcionalidade de **atualização automática de dados a cada 1 hora** que:

1. ✅ Faz download automático dos dados do servidor DHIS2
2. ✅ Substitui o ficheiro `data-raw/dados_processados.csv` com os novos dados
3. ✅ Faz refresh automático no dashboard para visualizar os dados atualizados
4. ✅ Mantém logs de todas as operações na consola R

---

## 📁 Ficheiros Alterados

### 1. **app.R** (modificado)
- ✅ Adicionadas dependências: `httr` e `jsonlite` à lista de pacotes obrigatórios
- ✅ Adicionado `source("R/auto_update.R", local = TRUE)` para carregar o módulo de atualização

### 2. **R/auto_update.R** (criado - NOVO)
- Função `get_dhis2_events()` - faz download paginado dos dados do DHIS2
- Função `download_and_save_dhis2_data()` - faz download e guarda em CSV
- Inclui tratamento de erros robusto
- Regista todas as operações em logs

### 3. **R/server.R** (modificado)
- ✅ Adicionado timer reativo que dispara a cada 1 hora (3600000 ms)
- ✅ Adicionado observador que executa o download automático
- ✅ Adicionada validação: só faz download se nenhum ficheiro personalizado estiver carregado
- ✅ Adicionado refresh automático dos dados após actualização

### 4. **Ficheiros de Documentação**

#### AUTO_UPDATE.md (criado - NOVO)
- Documentação completa sobre como configurar o auto-update
- Instruções passo a passo
- Resolução de problemas
- Recomendações de segurança

#### .Renviron.exemplo (criado - NOVO)
- Template para ficheiro de configuração de credenciais
- Inclui instruções de segurança

#### iniciar_painel_com_auto_update.R (criado - NOVO)
- Script pronto para usar para iniciar o painel
- Deteta se as credenciais estão configuradas
- Mostra status do auto-update ao iniciar

#### .gitignore (criado - NOVO)
- Previne commit de credenciais e dados sensíveis
- Inclui `.Renviron`, `dados_processados.csv`, etc.

---

## 🚀 Como Usar

### Passo 1: Configurar Credenciais

**Opção A - Variáveis de Ambiente (Recomendado):**
```r
Sys.setenv(DHIS2_USERNAME = "abule")
Sys.setenv(DHIS2_PASSWORD = "An1s1@1990$")
```

**Opção B - Ficheiro .Renviron:**
1. Copie `.Renviron.exemplo` para `.Renviron`
2. Preencha com as suas credenciais
3. O R carregará automaticamente

### Passo 2: Iniciar o Painel

**Forma Simples:**
```r
shiny::runApp()
```

**Forma Recomendada (com auto-update ativado):**
```bash
Rscript iniciar_painel_com_auto_update.R
```

### Passo 3: Verificar no Console

Veja mensagens como:
```
[Auto-update] 04/08/2026 00:31:25 - Iniciando download de dados...
[Auto-update] 04/08/2026 00:32:10 - Sucesso! Guardados 1234 registos em data-raw/dados_processados.csv
```

---

## ⚙️ Funcionamento Técnico

### Timer e Observador
- Timer reativo dispara a cada **3600 segundos (1 hora)**
- Observador detecta o disparo e inicia o download
- Download ocorre em background, sem bloquear a UI

### Download de Dados
- Faz paginação automática (100 registos/página)
- Conecta ao DHIS2 com autenticação básica HTTP
- Converte dados JSON em CSV
- Salva em `data-raw/dados_processados.csv`

### Refresh Automático
- Após salvar os dados, invalida o cache com `shiny::invalidateLater(100)`
- Força a recarga dos dados no painel em 100ms
- Dashboard atualiza com os novos dados automaticamente

### Segurança
- Só faz download se nenhum ficheiro personalizado estiver carregado
- Credenciais lidas de variáveis de ambiente (não hardcoded)
- Tratamento robusto de erros com try-catch

---

## 📊 Comparação: Antes vs Depois

| Aspecto | Antes | Depois |
|---------|-------|--------|
| Atualização de dados | Manual (upload CSV) | Automática (1 hora) |
| Intervalo de refresh | Nunca (manual) | 1 hora |
| Credenciais | Não aplicável | Seguras em env vars |
| Logs | Nenhum | Detalhados no console |
| Falhas | Sem tratamento | Try-catch robusto |

---

## ⚠️ Notas Importantes

1. **Credenciais Seguras**: Nunca coloque credenciais direto no código
2. **Variáveis de Ambiente**: Use `.Renviron` ou variáveis de sistema
3. **Continuidade**: O painel deve estar sempre em execução (nunca encerre)
4. **Logs**: Verifique regularmente o console R para detectar problemas
5. **Desativação**: Se deixar credenciais em branco, auto-update fica desativado

---

## 🔧 Troubleshooting

**Problema**: "Credenciais DHIS2 não configuradas"
- **Solução**: Defina `DHIS2_USERNAME` e `DHIS2_PASSWORD` antes de iniciar

**Problema**: "Erro HTTP 401"
- **Solução**: Verifique username e password

**Problema**: Dados não atualizam
- **Solução**: Verifique console R para mensagens de erro; confirme credenciais

---

## 📞 Próximos Passos

1. Teste a funcionalidade com `Rscript iniciar_painel_com_auto_update.R`
2. Configure as credenciais no ficheiro `.Renviron`
3. Deixe o painel a rodar continuamente (considere usar Docker ou supervisor)
4. Monitore os logs no console R

---

**Versão**: 1.0
**Data**: 04/08/2026
**Status**: ✅ Implementado e pronto para teste
