# Notas

- Foi reduzido o número de anime para a criação dos modelo.
- Os modelos foram criados a partir do pacote recommenderlab.
- A filtragem baseada em conteúdo foi utilizado o método explicado no video [Content Based Recommender](https://www.youtube.com/watch?v=YMZmLx-AUvY&list=LL&index=4).
- A base de dados **urlanime1.csv** contém os anime utilizados com os url das imagens de seus anime.
- O método de clusterização k-means foi utilizado para a filtragem baseada em conteúdo. O número k escolhido foi igual à 3.


# O que Falta Fazer
- Escolher a quantidade ideal de animes para os modelo
  - Se ficar com muitos animes talzez os usuarios não irão querer usar o app (demora um tempo considerado para avaliar)
  - Ver quanto de memória o shiny online suporta
  
- mudar a url do shiny
- colocar mensagem de error
- retirar coluna anime id
- label coluna rating para nota prevista
- colocar icon nos menuitens
- publicar o shiny
