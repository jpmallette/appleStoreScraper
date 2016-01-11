    # purpose : script to automate scraping of topappchartdownload

#--- initilisation 
source("mainFonction.R")

#--- Category extraction
# general
books<-extract_data_category("Books") 
education<-extract_data_category("Education")

# game
action_games<-extract_data_category("Action+Games")
adventure_games<-extract_data_category("Adventure+Games")
arcade_games<-extract_data_category("Arcade+Games")
educational_games<-extract_data_category("Educational+Games")
family_games<-extract_data_category("Family+Games")
puzzle_games<-extract_data_category("Puzzle+Games")  
simulation_games<-extract_data_category("Simulation+Games")
strategy_games<- extract_data_category("Strategy+Games")
trivia_games<-extract_data_category("Trivia+Games")
racing_games<-extract_data_category("Racing+Games")
role_games<-extract_data_category("Role+Playing+Games")

# top
#top_games<-extract_data_category("Top+Games") 
#top_apps<-extract_data_category("Top+Apps") 

#--- Code example : Vertical merge and writing csv
df<-rbind(books,education,action_games,adventure_games,arcade_games,
          educational_games,family_games,puzzle_games,simulation_games,
          strategy_games,trivia_games,racing_games,role_games)

write.csv(df,paste0("C:\\Users\\Terry\\Desktop\\Project\\Applestore\\data\\top_app","_",today,".csv"))
