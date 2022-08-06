# Arboviroses_R
Trata-se de um conjunto de scripts (em desenvolvimento) com a função de, a partir de base DBF (de 2009 ao ano atual) obtidos do SINAN e dados entomológicos (obtidos de planilhas Google Sheets), atingir os seguintes produtos finais:

1. Informe Epidemiológico Regional ( a ser prodzido em RMarkDown;  
2. Conjunto de tabelas com informações que servirão de base para dashboards municipais;  
3. Conjunto de tabelas com informações que servirão de base para Sistema de Informações Geográficas de Dados de Saúde - Arboviroses (SIG Arboviroses Regional).

## Contexto de Elaboração do Conjunto de Scripts  
Os scripts Arboviroses_R estão sendo elaborados para agilizarem o processo de obtenção e tratamento de dados em uma regional de saúde, permitindo que os técnicos servidores da regional possam otimizar o gasto de tempo priorizando a análise dos dados.  
Inicialmente a equipe técnica utilizava o SIG Arboviroses Regional como aglutinador de todas as informações produzidas pelo Programa Regional de Controle de Arboviroses, entretanto, o processo de obtenção e manipulação de dados para a produção das informações mostrava-se altamente complexo e demorado, consumindo tempo passível de ser utilizado para a análise das informações em sí.  
Com a determinação de um processo fixo e simplificado de obtenção de dados entomológicos (planilhas Google Sheet municipais), as BASES DBF de casos notificados no SINAN (um arquivo por ano) e a utilização da linguagem R, é possível para a Regional de Saúde centralizar de forma ágil todos os dados dos Programas Municipais de Controle de Arboviroses e redistribuir estes dados, já como informações, de volta aos municípios.  

## Metodologia de Obtenção de Dados