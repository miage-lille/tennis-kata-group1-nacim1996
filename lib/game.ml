(* TYPES *)
type player =
  | Player_one
  | Player_two

(* Surely not the best choice *)
type point =
  | Love
  | Fifteen
  | Thirty



type points_data =
  { player_one : point
  ; player_two : point
  }

type forty_data = {
  player: player (* The player who have forty points *)
  ;other_point: point (* Points of the other player *)
}


(* Surely incomplete *)
type score =
  | Points of points_data
  | Deuce
  | Game of player
  | Forty of forty_data
  | Advantage of player



(* TOOLING FUNCTIONS *)

let string_of_player player =
  match player with
  | Player_one -> "Player 1"
  | Player_two -> "Player 2"


let other_player player =
  match player with
  | Player_one -> Player_two
  | Player_two -> Player_one


let string_of_point : point -> string =
 fun point ->
  match point with
  | Love -> "0"
  | Fifteen -> "15"
  | Thirty -> "30"

 



let string_of_score : score -> string =
 fun score ->
  match score with
  | Points score -> "player_one :" ^ string_of_point score.player_one ^ string_of_point score.player_two
  |Forty score -> 
     begin match score.player with
      | player_one -> string_of_player score.player ^ " : 40 " ^ " |player two :" ^ string_of_point score.other_point
      | player_two -> "player one :" ^ string_of_point score.other_point ^ "|" ^ string_of_player score.player ^ "" end
  
  | Deuce -> " The score is : player one   40 /  player two  40"
  | Game score -> string_of_player score ^ " win"
  | Advantage score -> string_of_player score ^ " : advantage" 



(* An exemple how to use option to avoid null values *)
let increment_point : point -> point option =
 fun point ->
  match point with
  | Love -> Some Fifteen
  | Fifteen -> Some Thirty
  | Thirty -> None
  


(* An exemple how to extract values from 'a option value*)
let read_from_option_point : point option -> point =
 fun optinal_point ->
  match optinal_point with
  | Some a -> a
  | None -> Love


(* TRANSITION FUNCTIONS *)
let score_when_deuce : player -> score =
 fun winner -> Advantage winner 


let score_when_advantage : player -> player -> score =
 fun advantagedPlayer winner -> if advantagedPlayer = winner 
                                then Game winner
                                else Deuce


let score_when_forty current_forty winner =
  if current_forty.player = winner
  then Game winner
  else
    match increment_point current_forty.other_point with
    | None -> Deuce
    | Some p -> Forty { player = current_forty.player; other_point = p }

    
let score_when_game : player -> score =
 fun winner -> Game winner


let score_when_point : 'points_data -> player -> score =
 fun current winner -> 
   if winner = Player_one
     then  
        match increment_point current.player_one with
           | None -> Forty{player = Player_one ; other_point = current.player_two}
           | Some p -> Points {player_one = p ; player_two = current.player_two}
   else match increment_point current.player_two with
           | None -> Forty{player = Player_two;other_point = current.player_one}
           | Some p -> Points {player_one = current.player_one;player_two = p}



let score current winner =
  match current with
  | Points p -> score_when_point p winner
  | Forty f -> score_when_forty f winner
  | Deuce -> score_when_deuce winner
  | Advantage a -> score_when_advantage a winner
  | Game g -> score_when_game g
