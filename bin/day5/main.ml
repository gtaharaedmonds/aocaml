open Base

let split_on lst on =
  let rec split_on_aux lst on cache =
    match lst with
    | [] -> [ cache ]
    | hd :: tl ->
        if String.equal hd on then cache :: split_on_aux tl on []
        else split_on_aux tl on (hd :: cache)
  in
  let split = split_on_aux lst on [] in
  List.map split ~f:List.rev

let parse_seeds line =
  let seeds =
    match String.rsplit2 line ~on:':' with
    | None -> failwith "parse error!"
    | Some (_, seeds) -> seeds
  in
  seeds |> String.strip |> String.split ~on:' ' |> List.map ~f:Int.of_string

let parse_map lines =
  let lines =
    match List.tl lines with
    | None -> failwith "parse error!"
    | Some lines -> lines
  in
  List.map lines ~f:(fun line ->
      let split = line |> String.split ~on:' ' |> List.map ~f:Int.of_string in
      match split with
      | [ dst; src; len ] -> (dst, src, len)
      | _ -> failwith "parse error!")

let parse_almanac lines =
  let seeds, lines =
    match lines with
    | seeds :: _ :: lines -> (seeds, lines)
    | _ -> failwith "parse error!"
  in
  let maps = split_on lines "" in
  (parse_seeds seeds, List.map maps ~f:parse_map)

let apply_map input map =
  match
    List.find_map map ~f:(fun (dst, src, len) ->
        if input >= src && input < src + len then Some (dst + (input - src))
        else None)
  with
  | Some num -> num
  | None -> input

let rec apply_almanac input maps =
  match maps with
  | [] -> input
  | map :: rem_maps -> apply_almanac (apply_map input map) rem_maps

let solve file =
  let lines = In_channel.with_open_text file In_channel.input_lines in
  let seeds, maps = parse_almanac lines in
  seeds
  |> List.map ~f:(fun seed -> apply_almanac seed maps)
  |> List.fold ~init:Int.max_value ~f:min

let p1_ans = solve "data/day5.txt"
let () = Stdio.printf "%d\n" p1_ans
