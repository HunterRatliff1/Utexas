library(devtools)
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)

# If someone were to report sexual assault or sexual misconduct to an official, how likely is it that...
Response <- read.csv("~/Downloads/Utexas sexual assult - Table 1.1.csv", na.strings="s") %>%
  mutate(question = paste(Noun, Verb)) %>% select(-Noun, -Verb, -Survey.Item)

Response$Response <- factor(Response$Response, levels = c("Not at all", "A little", "Somewhat", "Very", "Extremely"), ordered = T)


qplot(data=Response, x=Response, y=question, size=Percentage, geom = "dotplot")

ggplot(Response, aes(y=Percentage, x=question)) + 
  geom_bar(aes(fill=Response, color=Sex), stat="identity") + facet_wrap("question")  +
  facet_grid(Level ~ .) + coord_flip() +
  ggtitle("If someone were to report sexual assault or sexual misconduct to an official, how likely is it that...") +
  theme(
    plot.title = element_text(lineheight=.8, size=28, face="bold"),
    strip.text.x = element_text(size=16, face="bold"), #angle=45),
    strip.text.y = element_text(size=12, face="bold"),
    strip.background = element_rect(colour="red", fill="#CCCCFF")) + xlab("")

ggplot(Response, aes(y=Percentage, x=Sex)) + 
  geom_bar(aes(fill=Response), stat="identity", position = "dodge") + #facet_wrap("question")  +
  facet_grid(Level ~ question) + coord_flip() + 
  ggtitle("If someone were to report sexual assault or sexual \nmisconduct to an official, how likely is it that...") +
  theme(strip.text.x = element_text(size=8, angle=45),
        strip.text.y = element_text(size=12, face="bold"),
        strip.background = element_rect(colour="red", fill="#CCCCFF"))


ggplot(Response, aes(x=question, y=Response)) + geom_tile(aes(fill=Percentage)) + facet_grid(Sex ~ Level) + coord_flip() + scale_fill_gradient(low="red", high="green", space="Lab")
  geom_boxplot(aes(fill=Percentage)), stat="identity") + facet_wrap("question")  +
  facet_grid(Level ~ .) + coord_flip() +
  ggtitle("If someone were to report sexual assault or sexual misconduct to an official, how likely is it that...") +
  theme(
    plot.title = element_text(lineheight=.8, size=28, face="bold"),
    strip.text.x = element_text(size=16, face="bold"), #angle=45),
    strip.text.y = element_text(size=12, face="bold"),
    strip.background = element_rect(colour="red", fill="#CCCCFF")) + xlab("")