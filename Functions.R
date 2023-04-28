# Functions

# Adding stats function
playerstats <- function(){
  # Import individual player statistic for 2022/23 season
  totals <- read_excel("2022-23 Season Totals.xlsx")
  # Field goal percentage
  totals$"FG%" <- totals$FG / totals$FGA
  # 3 point field goal percentage
  totals$"3P%" <- totals$"3P" / totals$"3PA"
  # 2 point field goal percentage
  totals$"2P%" <- totals$"2P" / totals$"2PA"
  # Free throw percentage
  totals$"FT%" <- totals$FT / totals$FTA
  # Assist to turnover ratio
  totals$"AST:TOV" <- totals$AST / totals$TOV
  # Minutes per game
  totals$MPG <- totals$MP / totals$G
  # Effective field goal percentage
  totals$"eFG%" <- (totals$FG + 0.5 * totals$"3P") / totals$FGA
  # Turnover percentage
  totals$"TOV%" <- 100 * totals$TOV / (totals$FGA + 0.44 * totals$FTA + totals$TOV)
  # True shooting attempts
  totals$TSA <- totals$FGA + 0.44 * totals$FTA
  # True shooting percentage
  totals$"TS%" <- totals$PTS / (2 * totals$TSA)
  # Points per game
  totals$PPG <- totals$PTS / totals$G
  # Assists per game
  totals$APG <- totals$AST / totals$G
  # Rebounds per game
  totals$RPG <- totals$TRB / totals$G
  # Offensive rebounds per game
  totals$ORPG <- totals$ORB / totals$G
  # Defensive rebounds per game
  totals$DRPG <- totals$DRB / totals$G
  # Steals per game
  totals$SPG <- totals$STL / totals$G
  # Blocks per game
  totals$BPG <- totals$BLK / totals$G
  # Turnovers per game
  totals$TPG <- totals$TOV / totals$G
  # Fouls per game
  totals$PFPG <- totals$PF / totals$G
  # Field goals made per game
  totals$FGPG <- totals$FG / totals$G
  # Field goals attempted per game
  totals$FGAPG <- totals$FGA / totals$G
  # 3 point field goals made per game
  totals$"3PPG" <- totals$"3P" / totals$G
  # 3 point field goals attempted per game
  totals$"3PAPG" <- totals$"3PA" / totals$G
  # 2 point field goals made per game
  totals$"2PPG" <- totals$"2P" / totals$G
  # 2 point field goals attempted per game
  totals$"2PAPG" <- totals$"2PA" / totals$G
  # Free throws made per game
  totals$FTPG <- totals$FT / totals$G
  # Free throws attempted per game
  totals$FTAPG <- totals$FTA / totals$G
  return(totals)
}
# Get season stats for any player
get_player_stats <- function(player){
  totals = totals
  player = totals[totals$Player == player, ]
  return(player)
}
# Get season stats for any team
get_team_stats <- function(team){
  totals = totals
  team = totals[totals$Tm == team, ]
  return(team)
}
# Atlanta Hawks players season stats
atl_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "ATL", ]
  return(team)
}
# Boston Celtics players season stats
bos_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "BOS", ]
  return(team)
}
# Brooklyn Nets players season stats
brk_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "BRK", ]
  return(team)
}
# Chicago Bulls players season stats
chi_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "CHI", ]
  return(team)
}
# Charlotte Hornets players season stats
cho_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "CHO", ]
  return(team)
}
# Cleveland Cavaliers players season stats
cle_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "CLE", ]
  return(team)
}
# Dallas Mavericks players season stats
dal_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "DAL", ]
  return(team)
}
# Denver Nuggets players season stats
den_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "DEN", ]
  return(team)
}
# Detroit Pistons players season stats
det_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "DET", ]
  return(team)
}
# Golden State Wwarriors players season stats
gsw_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "GSW", ]
  return(team)
}
# Houston Rockets players season stats
hou_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "HOU", ]
  return(team)
}
# Indiana Pacers players season stats
ind_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "IND", ]
  return(team)
}
# Los Angeles Clippers players season stats
lac_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "LAC", ]
  return(team)
}
# Los Angeles Lakers players season stats
lal_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "LAL", ]
  return(team)
}
# Memphis Grizzlies players season stats
mem_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "MEM", ]
  return(team)
}
# Miami Heat players season stats
mia_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "MIA", ]
  return(team)
}
# Milwaukee Bucks players season stats
mil_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "MIL", ]
  return(team)
}
# Minnesota Timberwolves players season stats
min_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "MIN", ]
  return(team)
}
# New Orleans Pelicans players season stats
nop_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "NOP", ]
  return(team)
}
# New York Knicks players season stats
nyk_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "NYK", ]
  return(team)
}
# Oklahoma City Thunder players season stats
okc_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "OKC", ]
  return(team)
}
# Orlando Magic players season stats
orl_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "ORL", ]
  return(team)
}
# Philadelphia 76ers players season stats
phi_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "PHI", ]
  return(team)
}
# Phoenix Suns players season stats
pho_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "PHO", ]
  return(team)
}
# Portland Trailblazers players season stats
por_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "POR", ]
  return(team)
}
# Sacremento Kings players season stats
sac_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "SAC", ]
  return(team)
}
# San Antonio Spurs players season stats
sas_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "SAS", ]
  return(team)
}
# Toronto Raptors players season stats
tor_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "TOR", ]
  return(team)
}
# Utah Jazz players season stats
uta_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "UTA", ]
  return(team)
}
# Washington Wizards players season stats
was_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "WAS", ]
  return(team)
}
# Players who played for more than one teams stats
tot_team_stats <- function(){
  totals = totals
  team = totals[totals$Tm == "TOT", ]
  return(team)
}

