# Análise de Séries Temporais - R

Data 08.08.2022
-------------------------------

Trabalho de conclusão da disciplina 'Análise de Séries Temporais', em R, do MBA Executivo em Business Analytics e BigData, pela Fundação Getúlio Vargas (FGV).  
  
--------------
Datasets:    

Os dados do arquivo anexado “Fertilizantes.xlsx” contemplam a entrega de fertilizantes ao mercado em mil toneladas no período mensal de janeiro de 1998 a setembro de 2021.  
A fonte dos dados é o sítio da Associação Nacional para Difusão de Adubos –ANDA (http://anda.org.br/estatisticas/).


--------------

OBJETIVO:  
  
1. Um breve comentário inicial relacionado à análise exploratória dos dados, incluindo a visualização, identificação de padrões, decomposição e o entendimento do padrão da série.   

2. Considerar os seguintes subconjuntos de dados: 
- Efetuar um corte na série temporal a partir de janeiro/2008   
- Notar o comportamento da série no período de março/20, a grande parada por conta dos casos de covid.  

3. Selecionar os modelos de estudo:     
Holt-Winters Aditivo   
Holt_Winters Multiplicativo  
Modelo SARIMA(p,d,q)(P,D,Q)[m].  
  
4. Plotar os correlogramas ACF e PACF e verificar a estacionariedade da série temporal.   

5. Efetuar os testes de raíz-unitária: Augmented Dickey_Fuller, Kwiatkowski–Phillips–Schmidt–Shin (KPSS) e Phillip-Perron.  
 
6. Modelagem do SARIMA(p,d,q)(P,D,Q)[s].   

7. Escolha do modelo por meio do critério de informação de Akaike.    
  
8. Comparar a série real em mil toneladas (abaixo) com a série projetada.  

Outubro-21	 4.706  
Novembro-21 	 4.201   
Dezembro-21	 3.314  
Janeiro-22		 3.223  
Fevereiro-22	 2.509  
Março-22		 2.952 
Abril-22		 2.713  

9. Conclusão sucinta.  
