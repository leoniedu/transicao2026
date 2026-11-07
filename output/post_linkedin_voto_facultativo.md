**A virada de Lula não veio do voto facultativo.**

*Análise, código e texto produzidos pelo Claude (IA da Anthropic), com as decisões de método e a revisão de Eduardo Leoni.*

Na Folha (3/11), Dora Kramer escreveu que a virada de Lula "mostrou o peso do voto facultativo" e que "os dados comprovam a adesão dos eleitores livres da obrigatoriedade do voto: jovens de 16 a 17 anos e pessoas de 70 ou mais". Os dados das seções eleitorais confirmam a primeira metade da frase (esses eleitores foram mais às urnas), mas não a conclusão: eles não decidiram a eleição.

🔹 **A abstenção não caiu de 22% para 19%.** Em 2022, ela foi de 20,95% no 1º turno e 20,59% no 2º; não chegou a 22%. Em 2026, o 1º turno teve 21,08%, a maior desde 1998. A queda para 19,22% aconteceu entre os dois turnos de 2026, e foi sobretudo de eleitores de voto obrigatório: dos cerca de 3 milhões de votantes a mais no 2º turno, dois terços tinham entre 18 e 69 anos.

🔹 **Os idosos de fato votaram mais, mas se dividiram.** A abstenção acima de 70 anos caiu de 59% no 1º turno de 2022 para 49% no 1º turno de 2026 (dado oficial do TSE). Foram 8,6 milhões de votantes, contra 6,0 milhões em 2022. Esse grupo pende para a direita: o saldo de Flávio entre eles foi de 0,8 milhão de votos, no 1º e no 2º turno.

🔹 **Os jovens de 16 e 17 anos votaram em Lula, mas são poucos.** Eram 2,1 milhões de eleitores em 2022 e 1,6 milhão em 2026. Quem se alistou compareceu mais (abstenção de 13%, a menor de todas as faixas), mas o saldo de Lula entre eles caiu de 0,7 milhão para 0,3 milhão.

🔹 **Somado, o voto facultativo favoreceu Flávio, nos dois turnos.** O saldo foi de cerca de 0,5 milhão de votos para a direita nos quatro turnos de 2022 e 2026, sem mudança entre o 1º e o 2º turno.

🔹 **A virada veio de quem é obrigado a votar.** Entre os turnos, Lula passou de 2,8 milhões atrás para 3,8 milhões à frente nas seções analisadas, uma virada de 6,6 milhões. Praticamente toda ela ocorreu entre eleitores de 18 a 69 anos. Os idosos a mais se dividiram quase meio a meio entre Lula e Flávio, como já tinha acontecido em 2022.

O ponto central da coluna, a mobilização, se sustenta: a eleição foi decidida por quem não tinha escolhido candidato no 1º turno. Mas esse eleitor estava, em sua grande maioria, sujeito à obrigatoriedade, e mesmo assim tinha ficado em casa ou anulado. A obrigatoriedade não garante o comparecimento, e a mobilização funcionou dentro dela. Se 2026 traz uma lição para a reforma política, ela não é sobre o voto facultativo.

**Método e limites.** A abstenção por idade no 1º turno de 2022 e de 2026 e no 2º turno de 2022 é dado oficial do TSE, por zona eleitoral. O dado do 2º turno de 2026 ainda não foi publicado: foi estimado somando, em cada zona, a variação de 2022 de cada faixa à taxa de 2026 e ajustando ao total observado de cada zona. Em 2022, a abstenção de cada zona no 2º turno repetiu a do 1º com um deslocamento quase fixo por faixa de idade (3 a 5 pontos a menos acima de 70 anos, quase zero nas demais). O voto de cada faixa é estimado por inferência ecológica (programação linear, com a mesma função objetivo do pacote lphom, em R), com uma estimativa por zona eleitoral, as seções como unidades e a abstenção de cada faixa fixada no valor oficial. São saldos líquidos estimados a partir de dados agregados; não descrevem indivíduos. O Distrito Federal ficou de fora porque o arquivo do TSE de 2022 não traz a UF.

Código e tabelas: https://github.com/leoniedu/transicao2026

#Eleições2026 #CiênciaPolítica #DadosAbertos #IA
