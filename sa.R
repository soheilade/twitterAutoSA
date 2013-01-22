library(twitteR)
library(stringr)
library(plyr)
library(ggplot2)
library(doBy)

    scores.sentiment <-function(sentence, pos.words, neg.words) {
      
        # and convert to lower case:
        sentence = tolower(sentence)
 
        # split into words. str_split is in the stringr package
        word.list = str_split(sentence, '\\s+')
        # sometimes a list() is one level of hierarchy too much
        words = unlist(word.list)
 
        # compare our words to the dictionaries of positive & negative terms
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
 
        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
 
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
        score = sum(pos.matches) - sum(neg.matches)
 
        return(score)
    }

 score.sentiment<-function(sentences,scores,allcities)
{
 
    scores.df <-data.frame(score=scores, text=sentences, onecity=allcities)
    return(scores.df)
}
pos.words=scan('G:/gephi pld graph modularity 1/R sentiment analysis/positive-words.txt',what='character',comment.char=';')
neg.words=scan('G:/gephi pld graph modularity 1/R sentiment analysis/negative-words.txt',what='character',comment.char=';')
pos.words=append(pos.words,':)')
neg.words=append(neg.words,':(')

brands<-c('#Honda','#Dodge','#Chevrolet','#Toyota','#Ford')

all.score<-c()

for (b in brands){
  b.tweets=searchTwitter(b,n=1500, lang="en")
	b_txt = sapply(b.tweets, function(x) x$getText())
	b_clean = clean.text(b_txt)
	bnum = length(b_clean)
  b_df = data.frame(text=b_clean, sentiment=rep("", bnum),score=1:bnum, stringsAsFactors=FALSE)
	sentiment = rep(0, bnum)
	tweet<-c()
	scores<-c()
	allbrands<-c()
	for(i in 1:bnum){
		scores<-append(scores,scores.sentiment(b_clean[i],pos.words,neg.words))
		tweet<-append(tweet,b_clean[i])
		allbrands<-append(allbrands,b)

}	

b.score<-score.sentiment(tweet,scores,allbrands)
all.score=rbind(all.score,b.score)	
}

ggplot(data=all.score)+geom_bar(aes(x=score,fill=oneb),binwidth=1)+facet_grid(oneb ~ .)+theme_bw()
