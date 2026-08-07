# Atualização Automática de Dados

O painel agora inclui funcionalidade de **atualização automática a cada 1 hora** dos dados do servidor DHIS2.

## Configuração

Para ativar a atualização automática, é necessário definir as credenciais do DHIS2 como variáveis de ambiente **antes** de iniciar o painel.

### Opção 1: Variáveis de Ambiente (Recomendado)

Defina as variáveis de ambiente no seu sistema ou no script que inicia o painel:

```r
Sys.setenv(DHIS2_USERNAME = "seu_utilizador")
Sys.setenv(DHIS2_PASSWORD = "sua_palavra_passe")

# Depois inicie o painel
shiny::runApp()
```

### Opção 2: Ficheiro `.Renviron`

Crie um ficheiro `.Renviron` no diretório raiz do projeto com o seguinte conteúdo:

```
DHIS2_USERNAME=seu_utilizador
DHIS2_PASSWORD=sua_palavra_passe
```

O R carregará automaticamente estas variáveis ao iniciar.

### Opção 3: Docker / Variáveis de Sistema

Se estiver a executar em Docker ou num servidor, defina as variáveis no ambiente do contentor ou do servidor.

## Comportamento

- **Intervalo**: A atualização ocorre automaticamente a cada **1 hora** (3600 segundos)
- **Condição**: O download só ocorre se **nenhum ficheiro CSV personalizado** estiver carregado manualmente
- **Localização**: Os dados são guardados em `data-raw/dados_processados.csv`
- **Refresh Automático**: Após cada actualização, o painel recarrega automaticamente os dados
- **Logs**: As operações são registadas na consola do R para auditoria

## Exemplo Completo

### Script de inicialização recomendado

```r
# script_iniciar_painel.R

# Configurar credenciais
Sys.setenv(
  DHIS2_USERNAME = "abule",
  DHIS2_PASSWORD = "An1s1@1990$"
)

# Carregar o painel
shiny::runApp(
  appDir = ".",
  host = "0.0.0.0",
  port = 3838,
  launch.browser = FALSE
)
```

Depois execute no terminal:

```bash
Rscript script_iniciar_painel.R
```

## Desativar Atualização Automática

Se preferir desativar a atualização automática:

1. Deixe as variáveis `DHIS2_USERNAME` e `DHIS2_PASSWORD` **vazias ou indefinidas**
2. O painel funcionará normalmente, apenas sem atualizar automaticamente

## Segurança

⚠️ **Importante**: Nunca coloque credenciais diretamente no código ou em ficheiros versionados no Git.

- Guarde as credenciais em variáveis de ambiente seguras
- Use `.gitignore` para excluir ficheiros como `.Renviron`
- Em ambientes de produção, considere usar um gestor de segredos (e.g., Vault, Kubernetes Secrets)

## Resolução de Problemas

### "Credenciais DHIS2 não configuradas"

Certifique-se de que as variáveis de ambiente estão definidas **antes** de iniciar o painel.

Verifique com:

```r
Sys.getenv("DHIS2_USERNAME")
Sys.getenv("DHIS2_PASSWORD")
```

### Erro de Autenticação

Verifique:
- O utilizador e a palavra-passe estão correctos
- O servidor DHIS2 está acessível
- A URL do servidor está correta em `R/auto_update.R`

### Dados Não Atualizam

1. Verifique se o painel está a rodar continuamente (não encerre a sessão R)
2. Abra a consola R e procure por mensagens `[Auto-update]`
3. Confirme que as credenciais estão configuradas

## Contacto

Para problemas com a atualização automática, contacte a equipa de desenvolvimento.
