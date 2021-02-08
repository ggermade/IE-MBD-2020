# Topic Modelling

In Natural Language Processing, Topic Modelling is an Unsupervised Learning task dealing with finding similarities of documents according to common tokens.

Our objective was to create a proof of concept for a social help app, called "iCanHelp" where users can post questions related to topics such as career advice or help with travel preparations.
For this app idea, we wish to implement NLP to automatically assign topics to questions, in order to then make personalized recommendations to users according to their tastes.

For this project's scenario, our documents were 2 million+ Quora questions from a Kaggle competition, which we analyzed to find similarity clusters, in order to find
discussion topics within Quora's forum. This served as a proxy for what would be our questions if we launched our app.

Our work focused primarily on the NLP components – tokenization, stemmatization, lemmatization, Bag of Words and TD-IDF formatting, trying out different amounts of clusters. 
To make the clusters themselves, implemented Latent Dirichlet Allocation, using LDAMulticore from GENSIM, a popular library for applying NLP for tasks including topic modelling.

Finally, we implemented a very basic recommendation system based on similarity metrics between documents' topic vectors and a sample of user tastes' vectors 
to serve as proof of concept for recommendations. We did not attempt to make a more sophisticated recommender system because we only needed a PoC to satisfy our project's needs.
However, given the outputs of the project, we're confident that the resulting vector representation of the documents can be used with ease to create content-based recommendations.



