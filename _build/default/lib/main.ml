open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different kinds
     of games *)
  let empty_game = Game.empty Game.Game_kind.Tic_tac_toe

  let place_piece (game : Game.t) ~piece ~position : Game.t =
    let board = Map.set game.board ~key:position ~data:piece in
    { game with board }
  ;;

  let win_for_x =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  let non_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;

  let get_str_piece ~tup ~game = 
    let row, col = tup in
    let open Game in
    let pos = { Position.row = row; Position.column = col} in
    match Map.find game.board pos with
    | Some piece -> Game.Piece.to_string piece
    | _ -> " "
  ;;

  let print_game (game : Game.t) = 
    let idx = Game.Game_kind.board_length game.game_kind 
      |> List.init ~f:(fun i -> i) in
    let sep, line = " | ", "\n---------\n" in
    List.fold idx ~init:[] ~f:(fun acc i -> 
      acc @ [List.fold idx ~init:[] 
      ~f:(fun acc j -> 
        acc @ [get_str_piece ~tup:(i, j) ~game]) 
      |> String.concat ~sep])
      |> String.concat ~sep:line
      |> print_endline
  ;;

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;

  let%expect_test "print_non_win" =
    print_game non_win;
    [%expect
      {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;;

  let get_pos ~tup ~game = 
    let row, col = tup in
    let open Game in
    let pos = { Position.row = row; Position.column = col} in
    match Map.find game.board pos with
    | Some _ -> None
    | _ -> Some pos
  ;;

  (* Exercise 1 *)
  let available_moves (game : Game.t) : Game.Position.t list =
    let idx = Game.Game_kind.board_length game.game_kind 
      |> List.init ~f:(fun i -> i) in
    List.fold idx ~init:[] ~f:(fun acc i -> 
      acc @ List.fold idx ~init:[] ~f:(fun acc j -> 
        match get_pos ~tup:(i, j) ~game with
        | Some pos -> pos :: acc
        | _ -> acc))
  ;;

  let%expect_test "available_moves_win_for_x" =
    let moves = available_moves win_for_x in
    print_s [%sexp (moves : Game.Position.t list)];
    [%expect
      {|()|}];
    return ()
  ;;

  let%expect_test "available_moves_non_win" =
    let moves = available_moves non_win in
    print_s [%sexp (moves : Game.Position.t list)];
    [%expect
      {|
      (((row 0) (column 2)) ((row 0) (column 1)) ((row 1) (column 2)) 
       ((row 1) (column 1)) ((row 2) (column 1)))
      |}];
    return ()
  ;;

  let check (lst: Game.Piece.t option list list) ~win_length = 
    List.fold lst ~init:(false, None) ~f:(fun (bool_val, player) sub_lst -> 
      let sub_lst = List.filter_opt sub_lst in
      match List.length sub_lst = win_length with
      | false -> (bool_val, player)
      | true -> 
        let head = sub_lst |> List.hd_exn in
        let unique_lst = List.filter sub_lst
          ~f:(fun i -> Game.Piece.equal head i |> not) in
        match unique_lst |> List.is_empty with
        | true -> (true, Some head)
        | false -> (bool_val, player))
  ;;
  let check_winner game = 
    let open Game in
    let idx = Game.Game_kind.board_length game.game_kind in
    let win_length = Game.Game_kind.win_length game.game_kind in
    let rows = List.init idx ~f:(fun i -> 
      List.init win_length ~f:(fun j -> 
        Map.find game.board {Game.Position.row = i; column = j})) in
    match check rows ~win_length with
    | true, player -> true, player
    | _ -> 
      let cols = List.init idx ~f:(fun i -> 
        List.init win_length ~f:(fun j -> 
          Map.find game.board {Game.Position.row = j; column = i})) in
      match check cols ~win_length with
      | true, player -> true, player
      | _ -> 
        let left_diag = [List.init idx ~f:(fun i -> 
          Map.find game.board {Game.Position.row = i; column = i})] in
          match check left_diag ~win_length with 
          | true, player -> true, player
          | _ -> 
            let right_diag = [List.init idx ~f:(fun i -> 
              Map.find game.board {Game.Position.row = i; column = win_length - (i + 1)})] in
              match check right_diag ~win_length with 
              | true, player -> true, player
              | _ -> false, None
  ;;

  let all_pieces_in_bound game = 
    let open Game in
    Map.fold game.board ~init:[] ~f:(fun ~key ~data:_ acc -> 
      match Game.Position.in_bounds key ~game_kind:game.game_kind with 
      | false -> key :: acc
      | true -> acc) |> List.is_empty
  ;;

  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t = 
    match check_winner game with
    | true, player -> Game.Evaluation.Game_over {winner = player}
    | _ -> match available_moves game with
          | [] -> Game.Evaluation.Game_over {winner = None} 
          | _ -> 
            match all_pieces_in_bound game with
            | false -> Game.Evaluation.Illegal_move
            | true -> Game.Evaluation.Game_continues
  ;;

  let%expect_test "evaluate_win_for_x" =
    let evaluation = evaluate win_for_x in
    print_s [%sexp (evaluation : Game.Evaluation.t)];
    [%expect
      {|(Game_over (winner (X)))|}];
    return ()
  ;;

  let%expect_test "evaluate_moves_non_win" =
    let evaluation = evaluate non_win in
    print_s [%sexp (evaluation : Game.Evaluation.t)];
    [%expect
      {|
      Game_continues
      |}];
    return ()
  ;;

  (* Exercise 3 *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t) : Game.Position.t list = 
    let moves = available_moves game in
    List.map moves ~f:(fun i -> 
      let tmp_game = {Game.game_kind = game.game_kind; 
        board = Map.add_exn game.board ~key:i ~data:me} in
      match evaluate tmp_game with 
      | Game.Evaluation.Game_over {winner = Some player} when Game.Piece.equal player me 
          -> Some i
      | _ -> None
    ) |> List.filter_opt
  ;;

  let%expect_test "winning_moves_win_for_x" =
    let moves = winning_moves ~me:Game.Piece.O win_for_x in
    print_s [%sexp (moves : Game.Position.t list)];
    [%expect
      {|()|}];
    return ()
  ;;

  let%expect_test "winning_moves_non_win" =
    let moves = winning_moves ~me:Game.Piece.X non_win in
    print_s [%sexp (moves : Game.Position.t list)];
    [%expect
      {|(((row 1) (column 1)))|}];
    return ()
  ;;

  (* Exercise 4 *)
  let losing_moves ~(me : Game.Piece.t) (game : Game.t) : Game.Position.t list =
    winning_moves ~me:(Game.Piece.flip me) game;
  ;;

  let%expect_test "losing_moves_win_for_x" =
    let moves = losing_moves ~me:Game.Piece.X win_for_x in
    print_s [%sexp (moves : Game.Position.t list)];
    [%expect
      {|()|}];
    return ()
  ;;

  let%expect_test "losing_moves_non_win" =
    let moves = losing_moves ~me:Game.Piece.O non_win in
    print_s [%sexp (moves : Game.Position.t list)];
    [%expect
      {|(((row 1) (column 1)))|}];
    return ()
  ;;

  let available_moves_that_do_not_immediately_lose ~(me : Game.Piece.t) (game : Game.t) : Game.Position.t list =
    let moves = available_moves game in
    List.map moves ~f:(fun i -> 
      let tmp_game = {Game.game_kind = game.game_kind; 
        board = Map.add_exn game.board ~key:i ~data:me} in
      match losing_moves tmp_game ~me with 
      | [] -> Some i
      | _ -> None
    ) |> List.filter_opt
  ;;

  let%expect_test "available_moves_that_do_not_immediately_lose_win_for_x" =
    let moves = available_moves_that_do_not_immediately_lose ~me:Game.Piece.X win_for_x in
    print_s [%sexp (moves : Game.Position.t list)];
    [%expect
      {|()|}];
    return ()
  ;;

  let%expect_test "available_moves_that_do_not_immediately_lose_non_win" =
    let moves = available_moves_that_do_not_immediately_lose ~me:Game.Piece.O non_win in
    print_s [%sexp (moves : Game.Position.t list)];
    [%expect
      {|(((row 1) (column 1)))|}];
    return ()
  ;;

  let%expect_test "available_moves_that_do_not_immediately_lose_non_win" =
    let current_game =
      let open Game in
      empty_game
      |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }
      |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
      |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    in
    let moves = available_moves_that_do_not_immediately_lose ~me:Game.Piece.O current_game in
    print_s [%sexp (moves:Game.Position.t list)];
    [%expect
      {|(((row 0) (column 0)))|}];
      return ()
  ;;

  let score ~(me : Game.Piece.t) (game : Game.t) = 
    match evaluate game with
    | Game_over {winner = Some player} 
        when Game.Piece.equal player me -> Int.max_value
    | Game_over {winner = Some player} 
        when Game.Piece.equal player (Game.Piece.flip me) -> Int.min_value
    | Game_over {winner = None} -> 0
    | _ -> 0
  ;;

  let%expect_test "score_win_for_x" =
    let sc = score ~me:Game.Piece.X win_for_x in
    print_s [%sexp (sc : int)];
    [%expect
      {|4611686018427387903|}];
    return ()
  ;;

  let%expect_test "score_win_for_x" =
    let sc = score ~me:Game.Piece.O win_for_x in
    print_s [%sexp (sc : int)];
    [%expect
      {|-4611686018427387904|}];
    return ()
  ;;

  let%expect_test "non_win_for_x" =
    let sc = score ~me:Game.Piece.O non_win in
    print_s [%sexp (sc : int)];
    [%expect
      {|0|}];
    return ()
  ;;

  let get_tmp_game ~game ~tmp_pos ~curr_player =
    let open Game in 
    {Game.game_kind = game.game_kind; 
    board = Map.add_exn game.board ~key:tmp_pos ~data:curr_player}
  ;;

  let rec minimax ~(game : Game.t) ~(curr_player : Game.Piece.t) ~(me : Game.Piece.t) ~(max_depth) ~_acc =
    (* print_s [%message (max_depth:int)]; *)
    (* print_game game;
    print_s [%message (curr_player:Game.Piece.t)];
    print_endline ""; *)
    match evaluate game with
    | Game.Evaluation.Game_over _ -> score game ~me
    | _ ->
      match max_depth with
      | 0 -> score game ~me 
      | _ -> 
          match Game.Piece.equal curr_player me with 
          | true -> 
            available_moves_that_do_not_immediately_lose game ~me:curr_player 
            |> List.fold 
            ~init:Int.min_value
            ~f:(fun best_sc tmp_pos -> 
              let value = minimax ~game:(get_tmp_game ~game ~curr_player ~tmp_pos) 
                ~max_depth:(max_depth - 1)
                ~curr_player:(Game.Piece.flip curr_player)
                ~me
                ~_acc:best_sc in
                (match value >= best_sc with
                | true -> value
                | false -> best_sc)
              )
          | false ->  
            available_moves_that_do_not_immediately_lose game ~me:curr_player 
            |> List.fold 
            ~init:Int.max_value 
            ~f:(fun best_sc tmp_pos -> 
              let value = minimax ~game:(get_tmp_game ~game ~curr_player ~tmp_pos) 
                ~max_depth:(max_depth - 1)
                ~curr_player:(Game.Piece.flip curr_player)
                ~me
                ~_acc:best_sc in
                (match value <= best_sc with
                | true -> value
                | false -> best_sc)
              )
  ;;

  let best_move ~game ~curr_player ~me =
    let moves = available_moves_that_do_not_immediately_lose game ~me in
    let pos, _ =  moves |> 
    List.fold ~init:({Game.Position.row = 0; column = 0}, Int.min_value) ~f:(
      fun (best_pos, best_sc) tmp_pos -> 
        let tmp_game = get_tmp_game ~game ~curr_player ~tmp_pos in
        let sc = minimax ~game:tmp_game ~curr_player:(Game.Piece.flip curr_player) ~me ~max_depth:10 ~_acc:0 in
        match sc >= best_sc with 
        | true -> tmp_pos, sc
        | false -> best_pos, best_sc
      ) in
    pos
  ;;

  let%expect_test "minimax_win_for_x" =
    let current_game =
		let open Game in
		empty_game
		|> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 0 }
		|> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 0 }
		|> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }
		|> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 0 }
		|> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 1 }
		|> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 2 }
	in
    let sc = best_move ~me:Game.Piece.X ~game:current_game ~curr_player:Game.Piece.X in
    print_s [%sexp (sc : Game.Position.t)];
    [%expect
      {|((row 1) (column 2))|}];
    return ()
  ;;

  let rec alphabeta ~game ~max_depth ~alpha ~beta ~curr_player ~me =
    match evaluate game with
    | Game.Evaluation.Game_over _ -> score game ~me
    | _ ->
      match max_depth with
      | 0 -> score game ~me 
      | _ -> 
        match Game.Piece.equal curr_player me with 
          | true -> available_moves_that_do_not_immediately_lose game ~me:curr_player 
          |> List.fold_until 
          ~init:Int.min_value
          ~f:(fun best_sc tmp_pos -> 
            let value = alphabeta ~game:(get_tmp_game ~game ~curr_player ~tmp_pos) ~beta ~alpha
              ~max_depth:(max_depth - 1)
              ~curr_player:(Game.Piece.flip curr_player)
              ~me in
              if value >= beta
                then Stop 0
              else
              (match value >= best_sc with
              | true -> Continue value
              | false -> Continue (best_sc))
            )
            ~finish:(fun best_sc -> best_sc)
          | false -> 
            available_moves_that_do_not_immediately_lose game ~me:curr_player 
            |> List.fold_until 
            ~init:Int.max_value
            ~f:(fun best_sc tmp_pos -> 
              let value = alphabeta ~game:(get_tmp_game ~game ~curr_player ~tmp_pos) ~alpha ~beta
                ~max_depth:(max_depth - 1)
                ~curr_player:(Game.Piece.flip curr_player)
                ~me in
                if value <= alpha
                  then Stop 0
                else
                (match value <= best_sc with
                | true -> Continue (value)
                | false -> Continue (best_sc))
              )
              ~finish:(fun best_sc -> best_sc)
  ;;

  let best_move_ab ~game ~curr_player ~me =
    let moves = available_moves_that_do_not_immediately_lose game ~me in
    let pos, _ =  moves |> 
    List.fold ~init:({Game.Position.row = 0; column = 0}, Int.min_value) ~f:(
      fun (best_pos, best_sc) tmp_pos -> 
        let tmp_game = get_tmp_game ~game ~curr_player ~tmp_pos in
        let sc = alphabeta ~game:tmp_game ~curr_player:(Game.Piece.flip curr_player) ~me ~max_depth:10 ~alpha:Int.min_value ~beta:Int.max_value in
        match sc >= best_sc with 
        | true -> tmp_pos, sc
        | false -> best_pos, best_sc
      ) in
    pos
  ;;

  let%expect_test "minimax_win_for_x" =
    let current_game =
		let open Game in
		empty_game
		|> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 0 }
		|> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 0 }
		|> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }
		|> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 0 }
		|> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 1 }
		|> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 2 }
	in
    let sc = best_move_ab ~me:Game.Piece.X ~game:current_game ~curr_player:Game.Piece.X in
    print_s [%sexp (sc : Game.Position.t)];
    [%expect
      {|((row 1) (column 2))|}];
    return ()
  ;;

  let exercise_one =
    Command.async
      ~summary:"Exercise 1: Where can I move?"
      (let%map_open.Command () = return () in
       fun () ->
         let moves = available_moves win_for_x in
         print_s [%sexp (moves : Game.Position.t list)];
         let moves = available_moves non_win in
         print_s [%sexp (moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_two =
    Command.async
      ~summary:"Exercise 2: Is the game over?"
      (let%map_open.Command () = return () in
       fun () ->
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let evaluation = evaluate non_win in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         return ())
  ;;

  let piece_flag =
    let open Command.Param in
    flag
      "piece"
      (required (Arg_type.create Game.Piece.of_string))
      ~doc:
        ("PIECE "
         ^ (Game.Piece.all |> List.map ~f:Game.Piece.to_string |> String.concat ~sep:", ")
        )
  ;;

  let exercise_three =
    Command.async
      ~summary:"Exercise 3: Is there a winning move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let winning_moves = winning_moves ~me:piece non_win in
         print_s [%sexp (winning_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_four =
    Command.async
      ~summary:"Exercise 4: Is there a losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let losing_moves = losing_moves ~me:piece non_win in
         print_s [%sexp (losing_moves : Game.Position.t list)];
         return ())
  ;;

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one"  , exercise_one
      ; "two"  , exercise_two
      ; "three", exercise_three
      ; "four" , exercise_four
      ]
  ;;
end

let handle (_client : unit) query = 
  print_s [%message "Received query" (query : Rpcs.Take_turn.Query.t)];
  let response = 
  Exercises.best_move 
            ~game:query.game 
            ~curr_player:query.you_play 
            ~me:query.you_play in
  return {Rpcs.Take_turn.Response.position = response; piece = query.you_play}
;;

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:[ Rpc.Rpc.implement Rpcs.Take_turn.rpc handle]
;;

let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     (* and controller =
       flag "-controller" (required host_and_port) ~doc:"_ host_and_port of controller" *)
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
      let%bind server =
           Rpc.Connection.serve
             ~implementations
             ~initial_connection_state:(fun _client_identity _client_addr ->
               (* This constructs the "client" values which are passed to the
                  implementation function above. We're just using unit for now. *)
               ())
             ~where_to_listen:(Tcp.Where_to_listen.of_port port)
             ()
         in
         Tcp.Server.close_finished server)
       (* We should start listing on the supplied [port], ready to handle incoming
          queries for [Take_turn] and [Game_over]. We should also connect to the
          controller and send a [Start_game] to initiate the game. *)
            
       (* ignore controller; *)
       (* ignore port; *)
       (* return ()) *)
;;

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "play", command_play; "exercises", Exercises.command ]
;;
