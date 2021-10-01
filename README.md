# Introdução
Identificar o que é e o que não é relevante é o principal objetivo dos sistemas de recomendação e existem critérios para determinar isso. Veremos adiante que alguns algoritmos utilizam informações baseadas nos usuários ou na concordância e similaridade entre eles e outros dados sobre o conteúdo/características de seus itens. A utilização de algumas técnicas estatísticas é essencial para ajudar a classificar e estimar produtos que usuários possam se interessar. 

Neste trabalho, focaremos nos serviços de streaming de animes. A quantidade de animes produzidos é em torno de 150 a 200 por ano. Diante disso, procurar algo que seja de interesse de algum usuário em meio a grandes possibilidades de opções é uma tarefa difícil. Sendo assim, interessante criar algoritmos que recomendam programas que sejam mais interessantes ou que sejam mais preferíveis para cada usuário. Esse trabalho visa otimizar esse problema do tempo de procura por esses programas. 


# Sistemas de Recomendação
Sistemas de recomendações são algoritmos criados para resolver problemas de sobrecarga de informação, auxiliando na busca por produtos e sugerindo lista de itens que podem ser relevantes através das informações sobre o usuário e dos itens. Existem várias de técnicas de recomendações, dentre elas destacam-se: filtragem baseada no conteúdo e filtragem colaborativa.   



## Por Popularidade
Funciona basicamente sugerindo produtos que estão em alta. Essa abordagem é caracterizada pela tendência. Assim como no sistema anterior, ele generaliza as recomendações para todos os usuários, independentemente das suas características ou preferências.  A vantagem dessa filtragem é que não tem problema de recomendação para usuários novos, porém não consegue sugerir itens novos. 

## Baseada em Conteúdo
Essa abordagem se baseia principalmente na similaridade dos itens, ou seja, um usuário gostará de itens semelhantes ao que ele goste. Essa técnica necessita de dados fornecidos pelos usuários, como as avaliações de produtos, lembrando que o usuário não precisa necessariamente avaliar todos os produtos do catálogo, no entanto, quanto mais informação a pessoa fornecer, mais precisa serão as recomendações.

## Colaborativa Baseada em Usuário
Nesse procedimeno, utiliza-se uma matriz UxI, onde as linhas representam os usuários e as colunas os itens, e as entradas Aij é nota que o usuário i fez ao item j. A partir dessa matriz, compara-se o usuário-alvo à k outros usuários usando medidas de similaridade. Depois de detecta o grupo de usuários mais similares, avalia-se os itens comprados por esse grupo a partir da média, frequência ou por pesos das avaliações. Em seguida recomenda-se o top-N itens mais bem avaliados.  

## Colaborativa Baseada em Item

Para cada dois itens medido o quão semelhantes eles são em termos de terem recebido classificações semelhantes por usuários semelhantes depois, para cada item, indetifica-se os k-itens mais semelhantes. Para cada usuário, identifica-se os itens que são mais semelhantes às compras do usuário e recomenda-se aqueles com uma avaliação mais alta.


# Metodologia
Foram coletados três conjuntos de dados o primeiro (animes.csv) contendo uma lista com informações de diferentes animes com título, descrição, gênero, avaliação etc., o segundo (rating.csv) refere-se às avaliações que cada usuário fez de um determinado anime, juntamente com o id do anime e do usuário. Os dados foram retirados do site  MyAnimeList.net por meio de web scraping e podem ser encontrados na plataforma online da comunidade de cientistas de dados Kaggle em formato CSV disponíveis em https://www.kaggle.com/CooperUnion/anime-recommendations-database 

Os algoritmos serão implementados utilizando o software estatístico R em conjunto com o pacote "recomeberlab" (HAHSLER,2021). 
Para a criação dos modelos, a base de dados foi separada 80\% para treino e 20\% para teste. A perfomance dos algoritmos será avaliada por medidas de avaliação de desempenho também apresentadas na seção anterior.
