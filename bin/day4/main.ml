open Base

let parse_nums nums =
  nums
  |> String.strip
  |> String.split ~on:' '
  |> List.filter ~f:(fun str -> not (String.equal str ""))
  |> List.map ~f:Int.of_string

let parse_line line =
  let nums =
    match String.rsplit2 line ~on:':' with
    | Some (_, nums) -> nums
    | None -> failwith "error"
  in
  let winning, mine =
    match String.rsplit2 nums ~on:'|' with
    | Some nums -> nums
    | None -> failwith "error"
  in
  (parse_nums winning, parse_nums mine)

let line_matches line =
  let winning, mine = parse_line line in
  List.count mine ~f:(fun my_num ->
      List.exists winning ~f:(fun winning_num -> winning_num = my_num))

module Part1 = struct
  let solve file =
    In_channel.with_open_text file In_channel.input_lines
    |> List.map ~f:(fun line ->
           let matches = line_matches line in
           if matches = 0 then 0 else Int.pow 2 (matches - 1))
    |> List.fold ~init:0 ~f:( + )
end

module Part2 = struct
  let update_copies copies matches mult =
    let copies =
      List.mapi copies ~f:(fun idx num ->
          num + if idx < matches then mult else 0)
    in
    let extend_len = max 0 (matches - List.length copies) in
    let extend_list = List.init extend_len ~f:(fun _ -> mult) in
    List.append copies extend_list

  let rec scratchards lines copies =
    match lines with
    | [] -> 0
    | line :: rem_lines ->
        let matches = line_matches line in
        let mult, rem_copies =
          match copies with [] -> (1, []) | hd :: tl -> (1 + hd, tl)
        in
        let updated_copies = update_copies rem_copies matches mult in
        1 + (matches * mult) + scratchards rem_lines updated_copies

  let solve file =
    let lines = In_channel.with_open_text file In_channel.input_lines in
    scratchards lines []
end

let p1_soln = Part1.solve "data/day4.txt"
let p2_soln = Part2.solve "data/day4.txt"
let () = Stdio.printf "Part 1: %d\nPart 2: %d\n" p1_soln p2_soln
