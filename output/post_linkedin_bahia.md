**O que é que a Bahia tem?**

*Análise, código, gráfico e texto produzidos pelo Claude (IA da Anthropic), com as decisões de método e a revisão de Eduardo Leoni.*

Nenhuma grande cidade do país reduziu tanto a abstenção entre os turnos de 2026 quanto Salvador: 4,4 pontos, contra 1,8 no resto do país.

A virada de Lula veio de quem não tinha escolhido candidato no 1º turno (abstenção, branco e nulo). A mesma estimativa, feita seção por seção com os dados abertos do TSE e somada por município, mostra onde essa vantagem foi maior do que o esperado.

🔹 **A Grande Salvador tem 1,7% do eleitorado do país e deu 4,1% da vantagem de Lula nesse grupo.** Nos 13 municípios da região metropolitana, a vantagem de Lula entre quem não tinha escolhido candidato foi de 8,6 pontos do eleitorado, contra 3,5 no resto do país.

🔹 **Não é só porque Lula já era forte lá.** Em todo o país, a vantagem cresce com a votação de Lula no 1º turno. Descontado esse efeito, a Grande Salvador ainda fica 3,9 pontos acima do esperado: Lauro de Freitas +5,5, Salvador +4,2, Simões Filho +4,4, Candeias +4,2. Feira de Santana (+3,2) e Camaçari (+3,0) vêm logo atrás. Nas cidades pequenas da Bahia, ao contrário, o resultado ficou abaixo do esperado. Nenhuma outra capital do Nordeste passa de +1.

🔹 **Já aconteceu antes.** Em 2014, com Dilma candidata à reeleição, a Grande Salvador ficou 3,7 pontos acima do esperado, e as quatro cidades que mais se destacam em 2026 (Lauro de Freitas, Simões Filho, Salvador e Candeias) já estavam entre 3,9 e 5,2 pontos acima. Mas naquele ano o efeito apareceu em quase todas as capitais do Nordeste (Aracaju +5,2, Teresina +4,0, Recife +3,5). Em 2022, com Lula na oposição, a Grande Salvador ficou só 0,8 acima, no mesmo nível do resto da Bahia. Em 2026, o efeito volta, mas só na Bahia.

O padrão aparece nas duas eleições em que o PT estava no governo federal e disputava a reeleição. São três eleições: é uma pista, não uma explicação. A pergunta que fica é o que, na Grande Salvador, mobilizou esse eleitor nessas condições, e por que em 2026, ao contrário de 2014, isso não se repetiu nas outras capitais do Nordeste.

**Método e limites.** Inferência ecológica por programação linear (pacote lphom, em R), com uma estimativa para cada zona eleitoral e as seções como unidades. O "esperado" vem de uma regressão, ponderada pelo eleitorado, da vantagem de Lula sobre a votação dele no 1º turno em cada município. Os números são saldos líquidos estimados a partir de resultados agregados: não descrevem indivíduos nem identificam causas. A queda da abstenção, por outro lado, é dado observado. Em 2014 a abstenção subiu entre os turnos, inclusive em Salvador, então o mecanismo daquele ano foi outro.

Código e tabelas: https://github.com/leoniedu/transicao2026

#Eleições2026 #Bahia #CiênciaPolítica #DadosAbertos #IA
