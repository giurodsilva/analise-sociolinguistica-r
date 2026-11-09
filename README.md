# analise-sociolinguistica-r

# Análise Estatística Sociolinguística: Variabilidade do /a/ Átono

Este repositório contém um script avançado em **R** desenvolvido para a análise exploratória, inferencial e multivariada de um corpus sociolinguístico (amostra SP2010), investigando o fenômeno da **nasalização variável do /a/ átono**.

---

# Nota sobre os Dados (Confidencialidade)

> **Aviso importante:** Por motivos de privacidade institucional, restrições éticas e restrições de confidencialidade da base de dados original (`a_atono.csv`), **os dados brutos utilizados nesta análise não estão incluídos neste repositório**. 

> O script é disponibilizado estritamente para fins de demonstração metodológica, portfólio acadêmico e compartilhamento de técnicas de modelagem estatística em R.

---

# Metodologia e Abordagem Estatística

O pipeline analítico implementado no script abrange as principais etapas da estatística aplicada à linguística quantitativa:

1. **Preparação e Limpeza de Dados:**
   - Padronização de fatores e reclassificação de níveis categóricos com o ecossistema `tidyverse`.
   - Tratamento de valores ausentes e inconsistências na base original.

2. **Análise Exploratória e Visualização (`ggplot2`):**
   - Geração automatizada de gráficos de barras proporcionais para variáveis sociais (faixa etária, escolaridade, renda, índice socioeconômico, geração, região/zona) e linguísticas (posição tonal, classe morfológica, vogal seguinte, número de sílabas).
   - Análise de distribuição para variáveis contínuas (como idade).

3. **Análise Inferencial Univariada:**
   - Testes de normalidade (Shapiro-Wilk) para verificação de distribuições.
   - Testes de independência de Qui-Quadrado ($\chi^2$) para variáveis nominais.
   - Testes não-paramétricos de Wilcoxon para comparação de grupos numéricos.

4. **Tratamento de Multicolinearidade:**
   - Diagnóstico rigoroso de colinearidade entre preditores sociais e demográficos utilizando o fator de inflação da variância generalizado (`GVIF` via pacote `car`).

5. **Modelagem Multivariada (Regressão Logística e Efeitos Mistos):**
   - Ajuste de modelos de regressão logística binária (`lrm` do pacote `rms`).
   - Implementação de **Modelos Lineares Generalizados de Efeitos Mistos (`glmer`** via `lme4`), controlando efeitos aleatórios de falante (`FALANTE`) e item lexical (`ITEM.LEXICAL`).
   - Análise de interações sociais e exportação de tabelas de regressão formatadas (`sjPlot`).

---

# Tecnologias e Pacotes Utilizados

O projeto foi inteiramente construído em **R**, fazendo uso dos seguintes pacotes:
* **Manipulação e Organização:** `tidyverse`
* **Modelagem Estatística e Mista:** `lme4`, `lmerTest`, `rms`
* **Diagnóstico de Multicolinearidade:** `car`
* **Visualização de Dados:** `ggplot2`, `gridExtra`
* **Extração de Resultados:** `sjPlot`, `effects`
