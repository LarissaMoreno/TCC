# Notas

- Foi reduzido o número de anime para a criação dos modelo.
- Os modelos foram criados a partir do pacote recommenderlab.
- A filtragem baseada em conteúdo foi utilizado o métod explicado no video [Content Based Recommender](https://www.youtube.com/watch?v=YMZmLx-AUvY&list=LL&index=4).
- A base de dados **urlanime1.csv** contém os anime utilizados com os url das imagens de seus anime.
- O método de clusterização k-means foi utilizado para a filtragem baseada em conteúdo. O número k escolhido foi igual à 3.


# O que Falta Fazer
- Escolher a quantidade de animes para os modelo
- escrever em rds os modelos e a base de dados com os url 
- mudar o visual com css
- mudar a url do shiny
- encontrar uma forma de usar dois eventReactive para um unico actionbuttom
- colocar imagem dos animes recomendados?
- publicar o shiny
