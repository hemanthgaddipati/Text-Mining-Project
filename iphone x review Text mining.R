require(rJava)||install.packages("rjava");library(rJava)

require(tm)||install.packages("tm");library(tm)

require(SnowballC)||install.packages("SnowballC");library(SnowballC)

require(wordcloud)||install.packages("wordcloud");library(wordcloud)

require(RWeka)||install.packages("RWeka");library(RWeka)

require(qdap)||install.packages("qdap");library(qdap)

require(textir)||install.packages("textir");library(textir)

require(igraph)||install.packages("igraph");library(igraph)

require(maptpx)||install.packages("maptpx");library(maptpx)

## functions

  ##func to make wordcloud and view some freqs for docs

    makewordc = function(a)	#plot wordcloud function opens
    {
      a.colsum = apply(a,2,sum);
      
      min1 = min(150,length(a.colsum))   #no morethan 150 trms in wordcloud
      
      words = colnames(a)[1:min1]
      
      freq = a.colsum*100
      
      wordcloud(words, freq, scale=c(8,0.3), colors=1:10)
    }     ## function ends

  ##function to make cluster dendrograms

    clusdend = function(a)  ##writing function clusdend
    {
      mydata.df = as.data.frame(inspect(a));
      
      mydata1.df = mydata.df[, order(-colSums(mydata.df))];
      
      min1 = min(ncol(mydata.df),40)
      
      test = matrix(0,min1,min1)
      
      test1 = test
      
      for(i1 in 1:(min1-1))
      {
        for(i2 in i1:min1)
        {
          test = sum(mydata1.df[,i1]-mydata1.df[,i2])^2
          
          test1[i1,i2] = test; test1[i2,i1] = test1[i1, i2] }
        }
      ##making dissimilarity matrix out of freq one
  
      test2 = test1
      
      rownames(test2) = colnames(mydata1.df)[1:min1]
  
      ##now plot col-location dendrogram
  
      d <- dist(test2, method="euclidean") #distance matrix
      
      fit <- hclust(d,method = "ward.D")
      
      plot(fit)
  
    }	    ##cluster dendrogram function ends

  ##elementary sentiment analysis

    pos=scan(file.choose(), what = "character", comment.char = ";")	#read-in +ve words.txt

    neg=scan(file.choose(), what = "character", comment.char = ";")	#read-in -ve words.txt

    pos.words = c(pos,"wow","kudos","hurray")	#include our own slang or our own words

    neg.words = c(neg, "fuck","shit","crap")	#include our own slang or our own words

  ##positive sentiment wordcloud

    makeposwordc = function(a)  #plot wordcloud function
    {
      pos.matches = match(colnames(a), pos.words)
  
      pos.matches = !is.na(pos.matches)
  
      b1 = apply(a,2,sum)[pos.matches]; b1 = as.data.frame(b1);
  
      colnames(b1) = c("freq");
  
      wordcloud(rownames(b1),b1[,1], scale = c(5,1), colors = 1:10)	#wordcloud of +ve words
  
    }     	#+ve wordcloud function ends


  ##negative sentiment wordcloud

    makenegwordc = function(a)  #plot wordcloud function
    {
      neg.matches = match(colnames(a), neg.words)
  
      neg.matches = !is.na(neg.matches)
  
      b1 = apply(a,2,sum)[neg.matches]; b1 = as.data.frame(b1);
  
      colnames(b1) = c("freq");
  
      wordcloud(rownames(b1),b1[,1], scale = c(5,1), colors = 1:10)	#wordcloud of -ve words
  
    }	    # -ve wordcloud function ends


    ## making social network of top 40 terms
    
    semantic.na <- function(dtm1)
    {		
      
      dtm1.new = inspect(dtm1[, 1:40]); dim(dtm1.new)	# top-25 tfidf weighted terms
      
      term.adjacency.mat = t(dtm1.new) %*% dtm1.new;	dim(term.adjacency.mat)
      
      # term.adjacency.mat1 = 1*(term.adjacency.mat != 0) 	# adjacency matrix ready
      
      # now invoke igraph and build a social network 
      
      require(igraph)
      
      g <- graph.adjacency(term.adjacency.mat, weighted = T, mode = "undirected")
      
      g <- simplify(g)		 # remove loops
      
      V(g)$label <- V(g)$name	# set labels and degrees of vertices
      
      V(g)$degree <- degree(g)
      
      #now the plot itself
      
      set.seed(1234)		# set seed to make the layout reproducible
      
      layout1 <- layout.fruchterman.reingold(g)
      
      plot(g, layout=layout1)
      
    } 
    
    ## make a similar network for the individuals, not the terms this time
      
      ppl.semantic.na <- function(dtm1, names){		# dtm has terms in columns, names is names list corres to docs
  
      dtm2.new = inspect(dtm1[,]); dim(dtm2.new)	
  
      term.adjacency.mat2 = dtm2.new %*% t(dtm2.new);	dim(term.adjacency.mat2)
  
      if (is.null(names)) {names = seq(1, nrow(term.adjacency.mat2), 1)}
  
      rownames(term.adjacency.mat2) = as.matrix(names)
      
      colnames(term.adjacency.mat2) = as.matrix(names)
  
      g1 <- graph.adjacency(term.adjacency.mat2, weighted = T, mode = "undirected")
  
      g1 <- simplify(g1)		 # remove loops
  
      V(g1)$label <- V(g1)$name	# set labels and degrees of vertices
  
      V(g1)$degree <- degree(g1)
  
      # now the plot itself

      set.seed(1234)	#set seed to make the layout reproducible

      layout2 <- layout.fruchterman.reingold(g1)
      
      plot(g1, layout = layout2)    #looks good isn't it.

    }	    #Semantic network function ends


### ---------------------------------- ###
###  Amazon IPHONE X Reviews dataset   ###
### ---------------------------------- ###

    # reading data from saved notepad  ""DATA HAS BEEN TAKEN USING "EXTRACT.R" AND SAVED IN .TXT FORMAT"" 

      x = readLines(file.choose()) 	# first, read-in data from "amazon nokia lumia reviews.txt"

      length(x)			# checks the length of data

      x1 = Corpus(VectorSource(x))  	# Constructs a source for a vector as input
      
      x1
      
      View(x1)
      
      x1 = tm_map(x1, stripWhitespace) 	# removes white space
      
      x1 = tm_map(x1, tolower)		# converts to lower case
      
      x1 = tm_map(x1, removePunctuation)	# removes punctuation marks
      
      x1 = tm_map(x1, removeNumbers)		# removes numbers in the documents

      x1 = tm_map(x1, removeWords, c(stopwords('english'), "project", "null"))

      tdm1 = TermDocumentMatrix(x1, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE), stopwords = TRUE));	# replace "i" by "I"		

      tdm0 = TermDocumentMatrix(x1);			# gives regular TF weighing, Above gave TFIDF weighing

      dim(tdm1)			# check its basic characteristics


    # remove blank documents (i.e. columns with zero sums)

      a0 = NULL; 

      for (i1 in 1:ncol(tdm0)){ if (sum(tdm0[, i1]) == 0) {a0 = c(a0, i1)} }

      length(a0)		# no. of empty docs in the corpus

      if (length(a0) >0) 
        { 
          tdm01 = tdm0[, -a0]} else {tdm01 = tdm0
        }
      dim(tdm01)	# under TF weighing

      if (length(a0) >0)
        { 
          tdm11 = tdm1[, -a0]
        } ;else{tdm11 = tdm1}
      
      dim(tdm11)	# under TFIDF weighing

      inspect(tdm01[1:5, 1:10])		# to view elements in tdm1, use inspect()
    
      str(tdm01)

    # convert tdms to dtms i.e. docs become rows and terms become columns
    
    # change dtm weighting from Tf (term freq) to TfIdf (term freq Inverse Doc freq)
      
      inspect(tdm01[1:5,1:5])
      
      dtm0 = t(tdm01)				# docs are rows and terms are cols
      
      inspect(dtm0[1:5,1:5])
    
    #tdmo1
    
      inspect(tdm11[1:5,1:5])
      
      dtm = t(tdm11)					# new dtm with TfIdf weighting
      
      inspect(dtm[1:20,1:30]) # (inverse frequency)
    
    # rearrange terms in descending order of Tf and view

      a1 = apply(dtm0, 2, sum);
    
      a1[335]
      
      dim(a1)
      
      a2 = sort(a1, decreasing = TRUE, index.return = TRUE)
      
      dtm01 = dtm0[, a2$ix]
      
      inspect(dtm01[1:5,1:5])

      dtm1 = dtm[, a2$ix];	
      
      inspect(dtm1[1:5, 1:5])

      windows()			# opens new windows & prevents over-writing of plots

      makewordc(dtm1)

      title(sub = "UNIGRAM - Wordcloud using TFIDF")

      windows();	semantic.na(dtm1)

      windows();	clusdend(dtm01)

      windows();	makeposwordc(dtm1)

      title(sub = "UNIGRAM - POSITIVE Wordcloud using TFIDF")

      windows();	makenegwordc(dtm1)

      title(sub = "UNIGRAM - NEGATIVE Wordcloud using TFIDF")


  # we can cluster the documents based on term usage #

    # kmeans proc #

    # better cluster on TF dtm rather than tfidf dtm for solution stability #

      wss = (nrow(dtm01)-1)*sum(apply(dtm01, 2, var))		 # Determine number of clusters by scree-plot 
      
      for (i in 2:8) wss[i] = sum(kmeans(dtm01, centers=i)$withinss)
      
      plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
      
      title(sub = "K-Means Clustering Scree-Plot")

      k1 = 2		# based on the scree elbow plot

      a3 = kmeans(dtm0, k1);	a3$size

      round(a3$size/sum(a3$size), 2)		# segmt-sizes as proportions

  # analyze each segment for what they're saying... #

      a4 = NULL

      for (i1 in 1:max(a3$cluster)) 
        { 
            a4[[i1]] = dtm1[(a3$cluster == i1),]
        } 
      

  # now plot wordclouds for by segment and see

      par(ask = TRUE)

      for (i2 in 1:max(a3$cluster))
        {	
          windows();makewordc(a4[[i2]])	
        } 

      par(ask = FALSE)		# close ask facility for graph making


# cluster dendograms cluster terms *within* documents
# in contrast, kmeans clusters documents themselves using word freqs across documents

# now look these clusters...

clusdend(a4[[1]])
clusdend(a4[[2]])
clusdend(a4[[3]])
clusdend(a4[[4]])


#that's it .....now draw inferences from the clusters and dendrograms