let parse_game line =
  (* split game line into game number and game record *)
  let num, record =
    match String.split_on_char ':' line with
    | num_text :: game_text :: _ -> (
        let split_game_text = String.split_on_char ' ' num_text in
        match split_game_text with
        | _ :: num :: _ -> (int_of_string num, game_text)
        | _ -> failwith "badly formate")
    | _ -> failwith "badly formatted line!"
  in
  (* split each subset into lists of tuples of (number of cubes, color) *)
  let parsed =
    record |> String.split_on_char ';'
    |> List.map (fun subset ->
           subset |> String.split_on_char ','
           |> List.map (fun cube_showing ->
                  let split_showing =
                    String.split_on_char ' ' (String.trim cube_showing)
                  in
                  match split_showing with
                  | num :: color :: _ -> (int_of_string num, color)
                  | _ -> failwith "parse error!"))
  in
  (* fold over parsed line to get an (r, g, b) tuple of the number of cubes in each
     individual showing *)
  let cubes =
    parsed
    |> List.map (fun subset ->
           subset
           |> List.fold_left
                (fun acc cube_showing ->
                  let num, color = cube_showing in
                  let r, g, b = acc in
                  match color with
                  | "red" -> (r + num, g, b)
                  | "green" -> (r, g + num, b)
                  | "blue" -> (r, g, b + num)
                  | _ -> failwith "not a color!")
                (0, 0, 0))
  in
  (num, cubes)

let solve file fn =
  In_channel.with_open_text file In_channel.input_lines
  |> List.map fn |> List.fold_left ( + ) 0

module Part1 = struct
  let valid_game line =
    (* check if the game on this line is valid *)
    let game_num, cubes = parse_game line in
    let valid =
      cubes
      |> List.fold_left
           (fun acc ball_showing ->
             let r, g, b = ball_showing in
             acc && r <= 12 && g <= 13 && b <= 14)
           true
    in
    if valid then game_num else 0
end

module Part2 = struct
  let power_of_min line =
    (* fold over a specific line to get the min possible number of each cube color *)
    let _, cubes = parse_game line in
    let mins =
      cubes
      |> List.fold_left
           (fun acc ball_showing ->
             let min_r, min_g, min_b = acc in
             let r, g, b = ball_showing in
             (max min_r r, max min_g g, max min_b b))
           (0, 0, 0)
    in
    let r, g, b = mins in
    r * g * b
end

let p1_soln = solve "data/day2.txt" Part1.valid_game
let p2_soln = solve "data/day2.txt" Part2.power_of_min
let () = Printf.printf "Part 1: %d\nPart 2: %d\n" p1_soln p2_soln
