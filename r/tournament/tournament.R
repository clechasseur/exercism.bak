tournament <- function(input) {
  with.team <- function(board, team.name) {
    if (nrow(board[board$Team == team.name,]) == 0) {
      board <- rbind(board, data.frame(Team = team.name, MP = 0, W = 0, D = 0, L = 0, P = 0))
    }
    board
  }
  register.win <- function(board, winning.team, losing.team) {
    board <- with.team(with.team(board, winning.team), losing.team)
    
    winning.team.infos <- board[board$Team == winning.team,]
    winning.team.infos$MP <- winning.team.infos$MP + 1
    winning.team.infos$W <- winning.team.infos$W + 1
    winning.team.infos$P <- winning.team.infos$P + 3
    board[board$Team == winning.team,] <- winning.team.infos
    
    losing.team.infos <- board[board$Team == losing.team,]
    losing.team.infos$MP <- losing.team.infos$MP + 1
    losing.team.infos$L <- losing.team.infos$L + 1
    board[board$Team == losing.team,] <- losing.team.infos
    
    board
  }
  register.draw <- function(board, drawing.team) {
    board <- with.team(board, drawing.team)
    
    drawing.team.infos <- board[board$Team == drawing.team,]
    drawing.team.infos$MP <- drawing.team.infos$MP + 1
    drawing.team.infos$D <- drawing.team.infos$D + 1
    drawing.team.infos$P <- drawing.team.infos$P + 1
    board[board$Team == drawing.team,] <- drawing.team.infos
    
    board
  }
  register.draws <- function(board, drawing.team.1, drawing.team.2) {
    board <- register.draw(board, drawing.team.1)
    board <- register.draw(board, drawing.team.2)
    
    board
  }
  
  global.board <- data.frame(Team = c(), MP = c(), W = c(), D = c(), L = c(), P = c())
  for (line in input) {
    infos <- unlist(strsplit(line, ";"))
    if (length(infos) == 3) {
      home.team <- infos[1]
      away.team <- infos[2]
      match.outcome <- infos[3]
      global.board <- switch(match.outcome,
                             win = register.win(global.board, winning.team = home.team, losing.team = away.team),
                             loss = register.win(global.board, winning.team = away.team, losing.team = home.team),
                             draw = register.draws(global.board, home.team, away.team),
                             global.board)
    }
  }
  sorted.board <- global.board[order(-global.board$P, global.board$Team),]
  rownames(sorted.board) <- c(1:nrow(sorted.board))
  sorted.board
}
