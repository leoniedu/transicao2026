**A virada de Lula veio de uma mobilização de tamanho sem precedente, e quase toda de um lado só.**

*Análise, código, gráfico e texto produzidos pelo Claude (IA da Anthropic), com as decisões de método e a revisão de Eduardo Leoni.*

O debate pós-eleição trata 2026 como excepcional: a primeira virada no 2º turno desde a redemocratização, explicada pela rejeição a Flávio Bolsonaro, pela campanha digital e pelo erro das pesquisas. Os dados das seções eleitorais mostram onde a virada aconteceu.

A análise estimou, seção por seção (dados abertos do TSE), para onde foram os eleitores de cada grupo do 1º turno nos três segundos turnos presidenciais em que o presidente disputava a reeleição: 2014, 2022 e 2026.

🔹 **Em 2026, a eleição se decidiu entre quem não tinha escolhido candidato no 1º turno** (abstenção, branco e nulo). Lula teve saldo de +6,2 milhões nesse grupo; Flávio, de +0,5 milhão, uma proporção de 12 para 1. 

🔹 **A direção não é nova; o tamanho é.** Nos três segundos turnos com presidente candidato à reeleição, quem governava levou a maior parte desse grupo, inclusive Bolsonaro em 2022. Mas a vantagem foi de +1,0 milhão com Dilma em 2014 e de +1,9 milhão com Bolsonaro em 2022, contra +5,7 milhões com Lula em 2026: de três a seis vezes maior. Em 2014, o comparecimento caiu 2,4 milhões entre os turnos; em 2022, subiu 0,6 milhão. O comparecimento subiu 3 milhões entre os turnos em 2026.

🔹 **Os eleitores dos candidatos derrotados não decidiram.** Os de Caiado, Cury, Renan e Zema se dividiram ao meio, cerca de 3,6 milhões para cada lado, embora Caiado, Cury e Zema tenham declarado apoio a Flávio. Em 2014 foi diferente: os eleitores de Marina foram majoritariamente para Aécio (+13,9 contra +8,0 milhões), e isso anulou a vantagem de Dilma na mobilização.

Outros pontos do debate, à luz dos dados:
- **Nordeste:** teve o maior ganho relativo (+5,9% do eleitorado), mas o Sudeste respondeu pela maior parte da vantagem líquida de Lula (43%, contra 39% do Nordeste). A mobilização aconteceu em todas as regiões.
- **Pesquisas:** o erro se concentra justamente no grupo que não tinha escolhido candidato. É o eleitor que os modelos de "eleitor provável" descartam.

São três eleições: a vantagem de quem governa é um padrão a testar, não uma lei. O que os dados mostram com clareza é que, em 2026, ela foi maior e mais desigual do que nos casos anteriores. Os próximos passos são 2006, as eleições com sucessor (2002 e 2010) e os segundos turnos para governador, que permitem separar a vantagem de quem governa do efeito de arrasto do candidato a presidente.

**Método e limites.** Inferência ecológica por programação linear (pacote lphom, em R), com uma estimativa para cada zona eleitoral e as seções como unidades. O desenho foi escolhido depois de uma simulação com transições conhecidas. Os números são saldos líquidos estimados a partir de resultados agregados; não descrevem o comportamento de indivíduos nem identificam causas (rejeição, campanha, benefícios). O grupo "não escolheram candidato" é a parte menos precisa da estimativa, por isso abstenção, branco e nulo aparecem somados. Ainda faltam intervalos de incerteza e uma validação com pesquisa individual (ESEB).

Código, tabelas e gráfico: https://github.com/leoniedu/transicao2026

#Eleições2026 #CiênciaPolítica #DadosAbertos #IA
