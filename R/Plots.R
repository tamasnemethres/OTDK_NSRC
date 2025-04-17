###############################################################################
#Main Plots
###############################################################################

###############################################################################
#First-Order false belief

#False-Belief across groups
ggplot(tom, aes(x = grouping_new, y = ToM, fill= grouping_new)) +
  geom_violin(show.legend= FALSE)+
  scale_fill_manual(values = c("#E0EAF7", "#E0EAF7", "#156082"))+
  labs(x = "Csoportok", 
       y= "Elsőfokú hv teszt sikere")+
  scale_x_discrete(labels = c("pre-Covid", "Covid", "poszt-Covid")) +
  scale_y_continuous(breaks = c(0,0.5, 1))+
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        panel.background = element_rect(fill="#FCEEE5"),
        plot.background = element_rect(fill="#FCEEE5"),
        axis.text.x = element_text(color="black", size = 16),
        axis.text.y = element_text(color="black", size = 18),
        axis.text = element_text(color="black", size= 16),
        axis.title = element_text(color = "black", size = 18))



###############################################################################
#Second-Order false belief

# Second-Order false-belief across groups
ggplot(tom2nd_filtered, aes(x = grouping_new, y = ToM_2nd, fill= grouping_new)) +
  geom_violin(show.legend= FALSE)+
  scale_fill_manual(values = c("#E0EAF7", "#156082"))+
  labs(x = "Csoportok", 
       y= "Másodfokú hv teszt sikere")+
  scale_x_discrete(labels = c("Covid", "poszt-Covid")) +
  scale_y_continuous(breaks = c(0,0.5, 1))+
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        panel.background = element_rect(fill="#FCEEE5"),
        plot.background = element_rect(fill="#FCEEE5"),,
        axis.text.x = element_text(color="black", size = 14),
        axis.text.y = element_text(color="black", size = 16),
        axis.text = element_text(color="black", size= 14),
        axis.title = element_text(color = "black", size = 16))
###############################################################################
#Real-Apparent Emotions

#Real Apparent Emotions accross groups
ggplot(appenreal, aes(x = grouping_new, y = Appen_r_a, fill= grouping_new)) +
  geom_violin(show.legend= FALSE)+
  scale_fill_manual(values = c("#E0EAF7", "#156082", "#156082"))+
  scale_x_discrete(labels = c("pre-Covid", "Covid", "poszt-Covid")) +
  labs(x = "Csoportok", 
       y= "Vl érzelmek teszt sikere")+
  scale_y_continuous(breaks = c(0,0.5, 1))+
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        panel.background = element_rect(fill="#FCEEE5"),
        plot.background = element_rect(fill="#FCEEE5"),
        axis.text.x = element_text(color="black", size = 14),
        axis.text.y = element_text(color="black", size = 16),
        axis.text = element_text(color="black"),
        axis.title = element_text(color = "black", size = 16))




###############################################################################
#Appendix
###############################################################################

#First order

#Age and ToM accross groups
ggplot(tom, aes(x=Age,
                y= ToM))+
  geom_jitter(height = .05,
              alpha = 0.5,
              size = 2,
              aes(color = grouping_new)) +
  geom_smooth(method = "glm",
              method.args = list(family ="binomial"),
              se = FALSE,
              aes(color= grouping_new),
              alpha= 0.2)+
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, 
              color = "black", 
              linetype = "dashed",
              size=1.5)+
  labs(x = "Kor (év)", 
       y= "Elsőfokú hv teszt sikere",
       color= "Csoportok")+
  scale_color_discrete(
    labels = c("pre-Covid", "Covid", "poszt-Covid"))+
  scale_y_continuous(breaks = c(0, 0.5,  1))+
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        panel.background = element_rect(fill="#FCEEE5"),
        plot.background = element_rect(fill="#FCEEE5"),
        legend.background = element_rect(fill="#FCEEE5"),
        axis.text.x = element_text(color="black", size = 14),
        axis.text.y = element_text(color="black", size = 16),
        axis.text = element_text(color="black", size= 14),
        axis.title = element_text(color = "black", size = 18),
        legend.title = element_blank(),
        legend.text = element_text(color="black", size = 14))+
  xlim(3.80, 7.5)

#Age across groups
ggplot(tom,aes(x=grouping_new,y=Age))+
  geom_violin(fill= "#E1B590")+
  theme_classic()+
  labs(x="Csoportok",y="Kor (év)")+
  scale_x_discrete(labels=c("pre-Covid","Covid","poszt-Covid"))+
  theme(axis.text.x=element_text(colour="black",size=14),
        axis.text.y=element_text(colour="black",size=14),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text = element_text(color="black"),
        panel.background = element_rect(fill="#FCEEE5"),
        plot.background = element_rect(fill="#FCEEE5"),
        axis.title = element_text(color = "black"))


#Classification tree
tree <- rpart(ToM ~ Age, data=tom, method= "class")
rpart.plot(tree) 
################################################################################
#ToM2nd



#Age and ToM2nd accross groups
ggplot(tom2nd_filtered, aes(x=Age,
                            y= ToM_2nd))+
  geom_jitter(height = .05,
              alpha = 0.5,
              size = 2,
              aes(color = grouping_new)) +
  geom_smooth(method = "glm",
              method.args = list(family ="binomial"),
              se = FALSE,
              aes(color= grouping_new),
              alpha= 0.2)+
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, 
              color = "black", 
              linetype = "dashed",
              size=1.5)+
  labs(x = "Kor (év)", 
       y= "Másodfokú hv teszt sikere",
       color= "Csoportok")+
  scale_color_discrete(
    labels = c("Covid", "poszt-Covid"))+
  scale_y_continuous(breaks = c(0, 0.5,  1))+
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        panel.background = element_rect(fill="#FCEEE5"),
        plot.background = element_rect(fill="#FCEEE5"),
        legend.background = element_rect(fill="#FCEEE5"),
        axis.text.x = element_text(color="black", size = 14),
        axis.text.y = element_text(color="black", size = 16),
        axis.text = element_text(color="black"),
        axis.title = element_text(color = "black", size = 16),
        legend.title = element_blank(),
        legend.text = element_text(color="black", size = 14))
xlim(3.80, 10)



#Age across groups
ggplot(tom2nd_filtered,aes(x=grouping_new,y=Age))+
  geom_violin(fill= "#E1B590")+
  theme_classic()+
  labs(x="Csoportok",y="Kor (év)")+
  scale_x_discrete(labels=c("Covid","poszt-Covid"))+
  theme(axis.text.x=element_text(colour="black",size=14),
        axis.text.y=element_text(colour="black",size=14),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text = element_text(color="black"),
        plot.background = element_rect(fill = "white"),
        axis.title = element_text(color = "black"))


rpart.plot(tree_2)
################################################################################
#Real-Apparent Emotion Task
###############################################################################
#Groups and Real Apparent Emotions


ggplot(appenreal,aes(x=grouping_new,y=Age))+
  geom_violin(fill= "#E1B590")+
  theme_classic()+
  labs(x="Csoportok",y="Kor (év)")+
  scale_x_discrete(labels=c("pre-Covid", "Covid", "poszt-Covid"))+
  theme(axis.text.x=element_text(colour="black",size=14),
        axis.text.y=element_text(colour="black",size=14),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text = element_text(color="black"),
        panel.background = element_rect(fill="#FCEEE5"),
        plot.background = element_rect(fill="#FCEEE5"),
        axis.title = element_text(color = "black")
  )


