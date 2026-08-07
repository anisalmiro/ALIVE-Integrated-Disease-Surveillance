# Projecto ALIVE — painel avançado de monitoria e análise

Aplicação Shiny para a exportação do DHIS2 Tracker do **Projecto ALIVE**. Reúne monitoria da introdução e qualidade dos dados, sinais epidemiológicos, análise por grupos de unidades sanitárias (US), acesso e continuidade dos serviços, grupos vulneráveis e variação entre US.

## Como iniciar

1. Abra a pasta `painel_monitoria_dados` no RStudio.
2. Copie a exportação para `data-raw/dados_processados.csv`.
3. Na primeira utilização, execute:

```r
source("instalar_pacotes.R")
```

4. Inicie a aplicação:

```r
shiny::runApp()
```

Também pode usar **Run App** no RStudio ou carregar um CSV directamente no painel. Para definir outro caminho antes de iniciar:

```r
Sys.setenv(MONITORIA_DATA_PATH = "/caminho/para/meu_ficheiro.csv")
shiny::runApp()
```

## Módulo “Análises ALIVE”

### US individuais ou agrupadas

O utilizador pode seleccionar qualquer conjunto de US e escolher entre:

- analisar cada US separadamente; ou
- juntar as US num grupo com nome definido pelo utilizador.

O período, província, distrito, idade e sexo continuam a respeitar os filtros globais. A deduplicação está activa por defeito e conserva uma observação representativa por ID/grupo clínico repetido.

### Alertas de aumento de casos

Os limiares são calculados por diagnóstico e por US/grupo. A linha de base usa apenas os períodos anteriores ao período avaliado, evitando incorporar o próprio aumento no seu limiar. Estão disponíveis:

| Método | Cálculo e uso |
|---|---|
| Média + DP | Média histórica + número configurável de desvios-padrão. |
| Mediana + MAD | Alternativa robusta para séries com extremos. |
| Percentil histórico | Compara o período com o percentil escolhido da linha de base. |
| Crescimento | Compara com o período imediatamente anterior. |

O utilizador define a periodicidade, número de períodos históricos, mínimo de história, parâmetro do método e mínimo de casos. Um alerta é gerado apenas quando há história suficiente, o mínimo de casos é atingido e o volume ultrapassa o limiar.

Esses métodos são sinais estatísticos de investigação, não definições etiológicas de surto. A referência para média + 2 DP, percentis e CUSUM é o documento da OMS sobre detecção de epidemias de malária: [WHO — Methods for malaria epidemic detection](https://iris.who.int/bitstreams/035f2b17-fb2b-4aee-9aca-e2591405bfa7/download).

### Sarampo segundo a OMS

O painel implementa as seguintes regras operacionais da OMS:

- **caso suspeito:** febre e erupção maculopapular generalizada não vesicular, ou qualquer pessoa em quem um profissional de saúde suspeite de sarampo;
- **definição clínica:** suspeita do profissional, ou febre + erupção + tosse, coriza ou conjuntivite;
- **sinal de surto suspeito:** pelo menos 5 casos epidemiologicamente ligados com início de erupção separado por 7–21 dias;
- **sinal de surto confirmado:** pelo menos 2 casos confirmados laboratorialmente, temporalmente relacionados por 7–21 dias e ligados epidemiológica ou virologicamente.

Fonte: [WHO Measles Outbreak Toolbox](https://www.who.int/emergencies/outbreak-toolkit/disease-outbreak-toolboxes/measles-outbreak-toolbox) e [WHO Vaccine-Preventable Diseases Surveillance Standards — Measles](https://cdn.who.int/media/docs/default-source/immunization/vpd_surveillance/vpd-surveillance-standards-publication/who-surveillancevaccinepreventable-11-measles-r2.pdf).

A exportação fornecida não contém todos os campos clínicos e epidemiológicos. Por isso, o painel mostra **sinais a verificar**, nunca declara automaticamente um surto. Quando não existe data de início da erupção, a data do evento é assinalada como aproximação. A tabela de lacunas indica os UIDs opcionais que devem ser configurados.

### Cólera segundo a OMS

Na ausência de surto, o painel sinaliza 2 ou mais casos suspeitos ou 1 teste rápido positivo na mesma unidade/grupo em 7 dias. Também aplica os limiares OMS de testes rápidos positivos entre testados em 14 dias para um sinal provável e assinala a presença de confirmação laboratorial. A aquisição local e a ligação entre casos precisam de verificação epidemiológica.

Fonte: [WHO Cholera Outbreak Toolbox](https://www.who.int/emergencies/outbreak-toolkit/disease-outbreak-toolboxes/cholera-outbreak-toolbox).

### Acesso e continuidade dos serviços

O painel apresenta, por US:

- contactos, pessoas distintas e dias com actividade;
- primeiras consultas e consultas de seguimento;
- proporção com diagnóstico e tratamento;
- referência comunitária e acesso de grupos vulneráveis;
- retorno dentro do prazo configurado entre os registos com seguimento indicado;
- altas e desfechos negativos;
- contactos por 1.000 habitantes e alcance da população-alvo, quando há denominadores.

Use `config/modelo_denominadores.csv` ou baixe o modelo no painel. Preencha `population` e, se aplicável, `target_population`. Sem denominadores, os indicadores de acesso são apresentados como utilização observada e não como cobertura populacional.

### Vulnerabilidade, equidade e variação entre US

- grupos vulneráveis registados, crianças de 0–4 anos e pessoas de 55+ anos;
- acesso por sexo e faixa etária;
- tratamento, continuidade e desfechos negativos nos grupos prioritários;
- comparação da proporção de cada diagnóstico por 100 contactos entre US;
- casos por 1.000 habitantes quando a população está disponível;
- razão relativa à média, z-score e sinais de variação extrema.

Variações entre US podem reflectir risco epidemiológico, perfil da população, referência de casos, procura de serviços ou diferenças de registo; precisam de interpretação local.

## Monitoria e qualidade dos dados

O painel mantém análises simples e avançadas de:

- volume de registos, US activas e diagnósticos;
- completude por campo, registo e US;
- cobertura de períodos com introdução de dados e silêncios;
- pontualidade, atraso mediano e percentil 90;
- unicidade, validade e consistência;
- mapa de calor, carta de controlo e detecção robusta de picos/quedas;
- alertas de qualidade por gravidade;
- relatórios CSV e Excel desidentificados.

| Indicador | Definição |
|---|---|
| Completude | Percentagem de campos obrigatórios preenchidos. |
| Cobertura de reporte | Percentagem de combinações US–período com o mínimo de eventos escolhido. |
| Pontualidade | Registos actualizados dentro do prazo após a data do evento. |
| Unicidade | Ausência de IDs repetidos e possíveis duplicações clínicas. |
| Validade | IDs presentes, datas plausíveis e categorias válidas. |
| Consistência | Coerência entre respostas condicionais, estado, resultado, tratamento e desfecho. |
| Índice global | 35% completude + 20% pontualidade + 15% unicidade + 15% validade + 15% consistência. |

Classificação: **Crítico** (<60%), **Atenção** (60–79,9%), **Bom** (80–89,9%) e **Excelente** (≥90%).

## Configuração dos UIDs

Os 42 campos da exportação original estão mapeados em `R/config.R`, no objecto `FIELD_MAP`. Para aplicar integralmente as definições OMS, preencha `OPTIONAL_FIELD_MAP` ou defina as variáveis de ambiente `ALIVE_UID_*` indicadas nesse ficheiro. O painel continua a funcionar quando os campos opcionais não existem e apresenta as limitações na interface.

O formulário Word fornecido não mostra os UIDs. Confirme no dicionário de metadados do DHIS2 os campos `vK9PPhCrA6Q` e `M9GNJpUsAeT`, interpretados provisoriamente como população vulnerável e referência comunitária.

## Protecção de dados pessoais

Nome e apelido são usados internamente apenas para detectar possíveis duplicações. Nome, apelido e profissão não aparecem nas tabelas nem nas exportações. O pacote não inclui a base bruta fornecida.

## Limitações essenciais

- A base contém eventos individuais, não formulários agregados de notificação zero. Um período sem eventos pode significar ausência de casos ou falha de reporte.
- O acesso populacional exige denominadores oficiais por US; sem eles, contactos não equivalem a cobertura.
- Continuidade é aproximada pelo próximo evento do mesmo identificador de utente e depende da qualidade desse identificador.
- Os limiares estatísticos e sinais OMS orientam verificação e resposta; não substituem investigação epidemiológica, confirmação laboratorial nem decisão das autoridades de saúde.

## Estrutura

- `app.R`: inicia a aplicação;
- `R/config.R`: UIDs, parâmetros e referências;
- `R/functions.R`: limpeza, qualidade, limiares e análises;
- `R/ui.R`: interface;
- `R/server.R`: reactivos, gráficos, tabelas e exportações;
- `config/dicionario_variaveis.csv`: mapeamento dos campos;
- `config/modelo_denominadores.csv`: população e população-alvo por US;
- `VALIDACAO.md`: resultados esperados com a base fornecida;
- `gerar_relatorio_sem_painel.R`: relatório de qualidade sem abrir o Shiny.
