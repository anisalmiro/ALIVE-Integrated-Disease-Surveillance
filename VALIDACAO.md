# Validação realizada com a base fornecida

## Estrutura

- 3.874 registos;
- 42 colunas;
- 10 unidades sanitárias;
- período de 02/02/2025 a 21/07/2026;
- todos os 42 campos esperados pelo mapeamento estão presentes;
- todos os ficheiros R passam na análise sintáctica.

## Resultados de referência — período completo

Estes valores permitem confirmar se a aplicação está a processar a base correctamente com o prazo de pontualidade definido em 2 dias.

| Indicador | Resultado esperado |
|---|---:|
| Completude média dos campos obrigatórios | 99,86% |
| Registos digitados dentro de 2 dias | 49,77% |
| Unicidade | 93,70% |
| Validade | 100,00% |
| Consistência | 99,56% |
| Índice global de qualidade | 88,89% |
| Linhas afectadas por IDs repetidos | 168 |
| Linhas em possíveis duplicações clínicas | 244 |
| Registos digitados fora do prazo | 1.946 |
| Registos com pelo menos um alerta | 2.096 |

Um ID repetido duas vezes conta como duas **linhas afectadas**, embora corresponda a apenas um ID duplicado. Da mesma forma, o indicador de possíveis duplicações mostra todas as linhas pertencentes aos grupos sinalizados.

## Campos obrigatórios ausentes

| Campo | Registos ausentes |
|---|---:|
| Estado do exame/teste | 14 |
| Resultado | 12 |
| Diagnóstico | 10 |
| Recebeu tratamento | 9 |
| Desfecho | 9 |
| Tipo de consulta | 6 |
| Exame/teste solicitado | 6 |

## Observações específicas da base

- As coordenadas são `0,0` em todos os registos e não são tratadas como localizações válidas.
- A mediana do atraso de digitação no período completo é 3 dias, mas há importações históricas com atrasos muito maiores; por isso o painel mostra também o percentil 90 e limita visualmente os extremos nos gráficos.
- O formulário fornecido não contém os UIDs dos elementos de dados. Os campos `vK9PPhCrA6Q` e `M9GNJpUsAeT` foram interpretados, respectivamente, como população vulnerável e referência comunitária. Confirme estes dois significados no dicionário de metadados do DHIS2 antes de publicar o painel.
- A base contém eventos, não formulários agregados de notificação zero; por isso a “cobertura de reporte” deve ser usada como alerta operacional, e não como completude oficial do DHIS2.

## Validação do módulo ALIVE

- O selector permite analisar as 10 US separadamente ou reuni-las num grupo definido pelo utilizador.
- Os quatro métodos de limiar usam exclusivamente períodos anteriores; o período corrente não entra na sua própria linha de base.
- A deduplicação analítica conserva um registo por ID repetido e, numa segunda etapa, um registo por chave clínica completa repetida.
- A base contém 7 registos relacionados com sarampo, todos em CS Bandua. Cinco estão concentrados em 9–10/12/2025 e têm resultado registado como ligação epidemiológica.
- Esses cinco registos não são declarados automaticamente como surto OMS: faltam a data específica de início da erupção e confirmação da separação temporal de 7–21 dias. O painel mostra “agregado acima da contagem OMS” e solicita verificação.
- A exportação original não contém os campos opcionais de erupção, tosse, coriza, conjuntivite, data de início, IgM/PCR específico nem ligação epidemiológica estruturada. A tabela “Campos necessários e lacunas” deve mostrar UID não configurado e completude 0% para esses campos.
- O modelo `config/modelo_denominadores.csv` contém as 10 US e deixa população e população-alvo em branco. Antes de interpretar contactos por 1.000 habitantes ou alcance populacional, devem ser preenchidos denominadores oficiais.
- A continuidade é calculada como retorno dentro do prazo escolhido entre os eventos com seguimento indicado, usando o próximo evento do mesmo identificador de utente.

## Verificações técnicas

- Todos os ficheiros R passam na análise sintáctica.
- Todos os outputs declarados na interface possuem implementação no servidor.
- Os selectores da secção Análises ALIVE são actualizados apenas quando muda o universo de dados; a actualização dos próprios selectores não reinicia o observador nem produz um ciclo visual de “piscar”.
- A pasta de distribuição não contém o CSV bruto nem nomes de utentes.
